package scala.scalanative.codegen.llvm

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.sys.process._

import org.junit.Assert._
import org.junit.Assume.assumeTrue
import org.junit.Test

import scala.scalanative.build.{AgbccStackFrames, BuildException}
import scala.scalanative.nir

class AgbccStackFramesTest {
  private def assembly(body: String): String =
    ".thumb_func\nfixture:\n" + body + "\n.size fixture, .-fixture\n"

  @Test def describesFixedLargeAndDynamicFrames(): Unit = {
    assertTrue(
      AgbccStackFrames
        .rewrite(assembly("@sn_xlat_frameaddr r0\nbx lr"))
        .contains("mov\tr0, sp\n\tsub\tr0, #8")
    )
    assertTrue(
      AgbccStackFrames
        .rewrite(
          assembly(
            "push {r4-r7, lr}\nadd sp, sp, #-16\n@sn_xlat_frameaddr r0\nadd sp, sp, #16\npop {r4-r7, pc}"
          )
        )
        .contains("mov\tr0, sp\n\tadd\tr0, #28")
    )
    val large = AgbccStackFrames.rewrite(
      assembly(
        "push {r7, lr}\nldr r3, .Lframe\nadd sp, sp, r3\nmov r7, sp\n@sn_xlat_frameaddr r0\nsub sp, sp, r0\nmov sp, r7\nbx lr\n.Lframe:\n.word -4096"
      )
    )
    assertTrue(large.contains("mov\tr0, r7"))
    assertEquals(
      4096,
      "#([0-9]+)".r
        .findAllMatchIn(large.substring(large.indexOf("mov\tr0, r7")))
        .map(_.group(1).toInt)
        .sum
    )
    val high = AgbccStackFrames.rewrite(
      assembly(
        "push {r4, r5, lr}\nmov r5, r8\nmov r4, r9\npush {r4, r5}\n@sn_xlat_frameaddr r0\nbx lr"
      )
    )
    assertTrue(high.contains("add\tr0, #12"))
    val snapshot = AgbccStackFrames.rewrite(
      assembly(
        "push {r7, lr}\nmov r7, sp\n.code 16\nmov r7, r0\n@sn_xlat_frameaddr r0\nbx lr"
      )
    )
    assertTrue(snapshot.contains("mov\tr0, sp"))
    assertFalse(snapshot.contains("mov\tr0, r7"))
    val arrayPointer = AgbccStackFrames.rewrite(
      assembly(
        "push {r7, lr}\nadd sp, sp, #-16\nmov r7, sp\nstr r0, [r7]\nmov r7, r0\n@sn_xlat_frameaddr r1\nbx lr"
      )
    )
    assertTrue(arrayPointer.contains("mov\tr1, sp\n\tadd\tr1, #16"))
    val plain = assembly("mov r0, sp\nbx lr")
    assertEquals(plain, AgbccStackFrames.rewrite(plain))
  }

  @Test def protectsOriginalLocalsBeforeSavingRegisters(): Unit = {
    val rewritten = AgbccStackFrames.rewrite(
      assembly(
        "push {r4, r5, r6, lr}\nsub sp, sp, #12\n@sn_xlat_protect_stack 12\n@sn_xlat_frameaddr r0\nadd sp, sp, #12\npop {r4, r5, r6}\npop {r1}\nbx r1"
      )
    )
    assertTrue(rewritten.contains("fixture:\n\tsub\tsp, sp, #12\npush"))
    assertTrue(rewritten.contains("mov\tr0, sp\n\tadd\tr0, #32"))
    assertTrue(rewritten.contains("pop {r1}\n\tadd\tsp, sp, #12\nbx r1"))
    assertFalse(rewritten.contains("@sn_xlat_protect_stack"))
    for (end <- Seq("pop {pc}", "bx r3", "b other")) {
      try {
        AgbccStackFrames.rewrite(assembly("@sn_xlat_protect_stack 12\n" + end))
        fail(end)
      } catch { case _: BuildException => () }
    }
  }

  @Test def rejectsUntrackedFramesAndMalformedMarkers(): Unit = {
    for (source <- Seq(
          assembly("mov r1, r0\nsub sp, sp, r1\n@sn_xlat_frameaddr r0\nbx lr"),
          assembly("mov r1, r0\npush {r1}\n@sn_xlat_frameaddr r0\nbx lr"),
          assembly("@sn_xlat_frameaddr r9\nbx lr"),
          "@sn_xlat_frameaddr r0\n"
        )) {
      try { AgbccStackFrames.rewrite(source); fail(source) }
      catch { case _: BuildException => () }
    }
  }

  @Test def validatesProtectedStackAbi(): Unit = {
    import nir.{Defn, Inst, Local, Next, Op, Type, Val}
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
    val owner = nir.Global.Top("ProtectedSlotFixture")
    def global(name: String) = owner.member(nir.Sig.Extern(name))
    val protectTy = Type.Function(Seq(Type.Int), Type.Unit)
    val marker = global("__xlat_protect_stack")
    def emit(arguments: Int, bytes: Int): String = {
      val params = (1 to arguments).map(i => Val.Local(Local(i), Type.Int))
      val definition = Defn.Define(
        nir.Attrs.None,
        global("protected_fixture"),
        Type.Function(params.map(_.ty), Type.Int),
        Seq(
          Inst.Label(Local(0), params),
          Inst.Let(
            Local(10),
            Op.Call(
              protectTy,
              Val.Global(marker, Type.Ptr),
              Seq(Val.Int(bytes))
            ),
            Next.None
          ),
          Inst.Ret(Val.Int(1))
        )
      )
      AgbccCodeGen.emit(
        Seq(definition),
        Seq(definition, Defn.Declare(nir.Attrs.None, marker, protectTy))
      )
    }
    val code = emit(4, 12)
    assertTrue(code, code.contains("@sn_xlat_protect_stack 12"))
    assertFalse(code, code.contains("__xlat_protect_stack"))
    for ((args, bytes) <- Seq((5, 12), (1, 0), (1, 13), (1, 4100))) {
      try { emit(args, bytes); fail(s"accepted $args arguments, $bytes bytes") }
      catch { case _: BuildException => () }
    }
  }

  @Test def emitsStackReadsInTheAllocatingFunction(): Unit = {
    import nir.{Defn, Inst, Local, Next, Op, Type, Val}
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
    val owner = nir.Global.Top("StackReadFixture")
    def global(name: String) = owner.member(nir.Sig.Extern(name))
    val ty = Type.Function(Nil, Type.Int)
    val names = Seq("__xlat_frameaddr", "__xlat_stackptr")
    val definition = Defn.Define(
      nir.Attrs.None,
      global("stack_reads"),
      ty,
      Seq(
        Inst.Label(Local(0), Nil),
        Inst.Let(
          Local(1),
          Op.Call(ty, Val.Global(global(names(0)), Type.Ptr), Nil),
          Next.None
        ),
        Inst.Let(
          Local(2),
          Op.Call(ty, Val.Global(global(names(1)), Type.Ptr), Nil),
          Next.None
        ),
        Inst.Ret(Val.Local(Local(1), Type.Int))
      )
    )
    val code = AgbccCodeGen.emit(
      Seq(definition),
      Seq(definition) ++ names.map(n =>
        Defn.Declare(nir.Attrs.None, global(n), ty)
      )
    )
    assertTrue(code, code.contains("@sn_xlat_frameaddr %0"))
    assertTrue(code, code.contains("@sn_xlat_stackptr %0"))
    for (name <- names) assertFalse(code, code.contains(name + "("))
    assertTrue(code, code.contains("\"memory\", \"cc\""))
  }

  private def scopedBufferCode: String = {
    import nir.{Defn, Inst, Local, Next, Op, Type, Val}
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
    val owner = nir.Global.Top("ScopedBufferFixture")
    def global(name: String) = owner.member(nir.Sig.Extern(name))
    val pointer = Val.Local(Local(3), Type.Ptr)
    val saved = Val.Local(Local(2), Type.Ptr)
    val save = Type.Function(Nil, Type.Ptr)
    val restore = Type.Function(Seq(Type.Ptr), Type.Unit)
    val clobber = Type.Function(Nil, Type.Unit)
    val definition = Defn.Define(
      nir.Attrs.None,
      global("entry_scoped"),
      Type.Function(Seq(Type.Int), Type.Int),
      Seq(
        Inst.Label(Local(0), Seq(Val.Local(Local(1), Type.Int))),
        Inst.Let(
          Local(2),
          Op.Call(save, Val.Global(global("llvm.stacksave"), Type.Ptr), Nil),
          Next.None
        ),
        Inst.Let(Local(3), Op.Stackalloc(Type.Int, Val.Size(16)), Next.None),
        Inst.Let(
          Local(4),
          Op.Store(Type.Int, pointer, Val.Int(0x12345678), None),
          Next.None
        ),
        Inst.Jump(Next.Label(Local(5), Seq(Val.Int(0)))),
        Inst.Label(Local(5), Seq(Val.Local(Local(6), Type.Int))),
        Inst.Let(
          Local(7),
          Op.Call(
            restore,
            Val.Global(global("llvm.stackrestore"), Type.Ptr),
            Seq(saved)
          ),
          Next.None
        ),
        Inst.Let(
          Local(8),
          Op.Call(clobber, Val.Global(global("clobber_stack"), Type.Ptr), Nil),
          Next.None
        ),
        Inst.Let(Local(9), Op.Load(Type.Int, pointer, None), Next.None),
        Inst.Let(
          Local(10),
          Op.Comp(
            nir.Comp.Ieq,
            Type.Int,
            Val.Local(Local(9), Type.Int),
            Val.Int(0x12345678)
          ),
          Next.None
        ),
        Inst.If(
          Val.Local(Local(10), Type.Bool),
          Next.Label(Local(11), Nil),
          Next.Label(Local(15), Nil)
        ),
        Inst.Label(Local(11), Nil),
        Inst.Let(
          Local(12),
          Op.Bin(
            nir.Bin.Iadd,
            Type.Int,
            Val.Local(Local(6), Type.Int),
            Val.Int(1)
          ),
          Next.None
        ),
        Inst.Let(
          Local(13),
          Op.Comp(
            nir.Comp.Slt,
            Type.Int,
            Val.Local(Local(12), Type.Int),
            Val.Int(4)
          ),
          Next.None
        ),
        Inst.If(
          Val.Local(Local(13), Type.Bool),
          Next.Label(Local(5), Seq(Val.Local(Local(12), Type.Int))),
          Next.Label(Local(14), Nil)
        ),
        Inst.Label(Local(14), Nil),
        Inst.Ret(Val.Int(1)),
        Inst.Label(Local(15), Nil),
        Inst.Ret(Val.Int(0))
      )
    )
    val code = AgbccCodeGen.emit(
      Seq(definition),
      Seq(
        definition,
        Defn.Declare(nir.Attrs.None, global("clobber_stack"), clobber)
      )
    )
    assertTrue(code, code.contains("sn_stack_3[16]"))
    assertFalse(code, code.contains("mov sp, %0"))
    assertFalse(code, code.contains("@sn_xlat_stackptr"))
    code
  }

  private def unsignedComparisonCode: String = {
    import nir.{Defn, Inst, Local, Next, Op, Type, Val}
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
    val owner = nir.Global.Top("UnsignedCallFixture")
    def global(name: String) = owner.member(nir.Sig.Extern(name))
    val ty = Type.Function(Seq(Type.Int), Type.Int)
    val argument = Val.Local(Local(1), Type.Int)
    val provider = Defn.Define(
      nir.Attrs.None,
      global("u16_source"),
      ty,
      Seq(Inst.Label(Local(0), Seq(argument)), Inst.Ret(argument))
    )
    val compare = Defn.Define(
      nir.Attrs.None,
      global("compare_u16"),
      ty,
      Seq(
        Inst.Label(Local(0), Seq(argument)),
        Inst.Let(
          Local(2),
          Op.Call(ty, Val.Global(provider.name, Type.Ptr), Seq(argument)),
          Next.None
        ),
        Inst.Let(
          Local(3),
          Op.Bin(
            nir.Bin.And,
            Type.Int,
            Val.Local(Local(2), Type.Int),
            Val.Int(65535)
          ),
          Next.None
        ),
        Inst.Let(
          Local(4),
          Op.Comp(
            nir.Comp.Sge,
            Type.Int,
            Val.Local(Local(3), Type.Int),
            Val.Int(32767)
          ),
          Next.None
        ),
        Inst.Let(
          Local(5),
          Op.Conv(nir.Conv.Zext, Type.Int, Val.Local(Local(4), Type.Bool)),
          Next.None
        ),
        Inst.Ret(Val.Local(Local(5), Type.Int))
      )
    )
    val code = AgbccCodeGen.emit(
      Seq(provider, compare),
      Seq(provider, compare),
      Map("u16_source" -> Type.Function(Seq(Type.Int), Type.Char)),
      preserveDefinitionOrder = true
    )
    assertTrue(code, code.contains("((sn_u32)(sn_l3)) >= ((sn_u32)(32767))"))
    code
  }

  private def armDivisionCode: String = {
    import nir.{Defn, Inst, Local, Next, Op, Type, Val}
    implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
    implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
    val owner = nir.Global.Top("ArmDivisionFixture")
    def global(name: String) = owner.member(nir.Sig.Extern(name))
    val definitions = Seq(32 -> Type.Int, 64 -> Type.Long).map {
      case (bits, ty) =>
        val signature = Type.Function(Seq(ty, ty), ty)
        val left = Val.Local(Local(1), ty)
        val right = Val.Local(Local(2), ty)
        Defn.Define(
          nir.Attrs.None,
          global(s"arm_div$bits"),
          signature,
          Seq(
            Inst.Label(Local(0), Seq(left, right)),
            Inst.Let(
              Local(3),
              Op.Call(
                signature,
                Val.Global(global(s"__xlat_sdiv$bits"), Type.Ptr),
                Seq(left, right)
              ),
              Next.None
            ),
            Inst.Ret(Val.Local(Local(3), ty))
          )
        )
    }
    val code = AgbccCodeGen.emit(definitions)
    assertFalse(code, code.contains("__xlat_sdiv"))
    assertTrue(code, code.contains(" / (sn_i64)"))
    assertTrue(code, code.contains(" / (sn_i32)"))
    code
  }

  @Test def emitsArmSignedDivisionWithoutJvmGuards(): Unit = {
    armDivisionCode
    ()
  }

  @Test def runsEntryAndCurrentStackProbesOnGba(): Unit = {
    assumeTrue(
      "set GBA_TESTROM_DIR, GBA_TEST_RUNNER and AGBCC for the executable frame contract",
      Seq("GBA_TESTROM_DIR", "GBA_TEST_RUNNER", "AGBCC").forall(
        sys.env.contains
      )
    )
    val directory = Files.createTempDirectory("agbcc-stack-rom")
    val arm = sys.env.getOrElse("ARM_PREFIX", "arm-none-eabi-")
    def write(name: String, text: String): Path = Files.write(
      directory.resolve(name),
      text.getBytes(StandardCharsets.UTF_8)
    )
    def run(command: Seq[String]): Unit = assertEquals(
      command.mkString(" "),
      0,
      Process(command, directory.toFile).!
    )
    val root = Paths.get(sys.env("GBA_TESTROM_DIR"))
    val helper = write(
      "caller.s",
      """
        |.syntax unified
        |.thumb
        |.text
        |.global read_sp
        |.thumb_func
        |read_sp:
        | mov r0, sp
        | bx lr
        |.global run_case
        |.thumb_func
        |run_case:
        | push {r4, r5, lr}
        | mov r4, sp
        | movs r5, r0
        | movs r0, r1
        | bl .Lcall
        | cmp r0, r4
        | beq .Lok
        | movs r0, #0
        | b .Ldone
        |.Lok:
        | movs r0, #1
        |.Ldone:
        | pop {r4, r5}
        | pop {r1}
        | bx r1
        |.Lcall:
        | bx r5
        |.global run_protected
        |.thumb_func
        |run_protected:
        | push {r4, r5, r6, lr}
        | mov r6, sp
        | movs r4, #41
        | movs r5, #43
        | mov r1, sp
        | subs r1, #12
        | ldr r0, =0x1234
        | str r0, [r1]
        | bl protected_local
        | ldr r1, =0x1234
        | cmp r0, r1
        | bne .Lbadguard
        | cmp r4, #41
        | bne .Lbadguard
        | cmp r5, #43
        | bne .Lbadguard
        | mov r1, sp
        | cmp r1, r6
        | bne .Lbadguard
        | subs r1, #12
        | ldr r0, [r1]
        | ldr r1, =0x5678
        | cmp r0, r1
        | bne .Lbadguard
        | movs r0, #1
        | b .Lguarddone
        |.Lbadguard:
        | movs r0, #0
        |.Lguarddone:
        | pop {r4, r5, r6}
        | pop {r1}
        | bx r1
        |""".stripMargin
    )
    val scoped = scopedBufferCode
    val source =
      scoped + (unsignedComparisonCode + armDivisionCode).linesIterator
        .filterNot(_.startsWith("typedef "))
        .mkString("\n") +
        """
          |void __gccmain(void) {}
          |extern int read_sp(void);
          |extern int run_case(void *, int);
          |int entry_leaf(void) { int f; __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(f) : : "memory", "cc"); return f + 8; }
          |int entry_fixed(int n) {
          |  volatile int a[40]; int f, g, s, observed;
          |  __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(f) : : "memory", "cc"); a[0] = n; a[39] = n + 1; __asm__ volatile("@sn_xlat_stackptr %0" : "=l"(s) : : "memory", "cc"); observed = read_sp(); __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(g) : : "memory", "cc");
          |  if (f != g || s != observed || a[0] + 1 != a[39]) return 0;
          |  return g + 8;
          |}
          |int entry_dynamic(int n) {
          |  int f, g, s, observed; volatile char *a;
          |  __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(f) : : "memory", "cc"); a = __builtin_alloca(n); a[0] = 7; a[n-1] = 9;
          |  __asm__ volatile("@sn_xlat_stackptr %0" : "=l"(s) : : "memory", "cc"); observed = read_sp(); __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(g) : : "memory", "cc");
          |  if (f != g || s != observed || a[0] != 7 || a[n-1] != 9) return 0;
          |  return g + 8;
          |}
          |int entry_large(int n) {
          |  volatile int a[1024]; int f, s;
          |  a[0] = n; a[1023] = n + 1; __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(f) : : "memory", "cc"); __asm__ volatile("@sn_xlat_stackptr %0" : "=l"(s) : : "memory", "cc");
          |  if (s != read_sp() || a[0] + 1 != a[1023]) return 0;
          |  return f + 8;
          |}
          |void clobber_stack(void) {
          |  volatile unsigned char bytes[512]; int i;
          |  for (i = 0; i < 512; ++i) bytes[i] = 0xa5;
          |}
          |int run_protected(void);
          |int protected_local(void) {
          |  volatile int a[16]; int f, old; volatile int *slot;
          |  __asm__ volatile("@sn_xlat_protect_stack 12" ::: "memory");
          |  __asm__ volatile("@sn_xlat_frameaddr %0" : "=l"(f) : : "memory", "cc");
          |  __asm__ volatile("" ::: "r4", "r5", "r6");
          |  slot = (volatile int *)(f - 4); old = *slot;
          |  a[0] = old; clobber_stack(); *slot = 0x5678;
          |  return a[0];
          |}
          |int compare_values(void) {
          |  static const unsigned values[] = {0, 32766, 32767, 32768, 65535};
          |  int i;
          |  for (i = 0; i < 5; ++i)
          |    if (compare_u16(values[i]) != (values[i] >= 32767U)) return 0;
          |  return 1;
          |}
          |int check_div32(void) {
          |  static const int cases[][3] = {{0,0,0}, {23,0,0}, {-23,0,0},
          |    {(-2147483647-1),-1,(-2147483647-1)}, {23,-3,-7}, {-23,3,-7},
          |    {-23,-3,7}, {2147483647,3,715827882}, {(-2147483647-1),3,-715827882}};
          |  int i; for (i=0; i<9; ++i) if (arm_div32(cases[i][0],cases[i][1]) != cases[i][2]) return 0;
          |  return 1;
          |}
          |int check_div64(void) {
          |  static const long long cases[][3] = {{0,0,0}, {23,0,0}, {-23,0,0},
          |    {(-9223372036854775807LL-1),-1,(-9223372036854775807LL-1)},
          |    {23,-3,-7}, {-23,3,-7}, {-23,-3,7},
          |    {9223372036854775807LL,3,3074457345618258602LL},
          |    {(-9223372036854775807LL-1),3,-3074457345618258602LL}};
          |  int i; for (i=0; i<9; ++i) if (arm_div64(cases[i][0],cases[i][1]) != cases[i][2]) return 0;
          |  return 1;
          |}
          |int main(void) {
          |  volatile unsigned *result = (volatile unsigned *)0x0203fff0;
          |  int passed = 0;
          |  passed += run_case(entry_leaf, 0);
          |  passed += run_case(entry_fixed, 3);
          |  passed += run_case(entry_dynamic, 17);
          |  passed += run_case(entry_dynamic, 1027);
          |  passed += run_case(entry_large, 5);
          |  passed += entry_scoped(0);
          |  passed += compare_values();
          |  passed += run_protected();
          |  passed += check_div32();
          |  passed += check_div64();
          |  result[0] = 0x54524f4d; result[1] = passed; result[2] = 10 - passed;
          |  result[3] = passed == 10 ? 0x50415353 : 0x4641494c;
          |  for (;;) {}
          |}
          |""".stripMargin
    run(Seq(arm + "as", "-mcpu=arm7tdmi", "-o", "caller.o", helper.toString))
    run(
      Seq(
        arm + "as",
        "-mcpu=arm7tdmi",
        "-o",
        "crt0.o",
        root.resolve("rt/crt0.s").toString
      )
    )
    for (negative <- Seq(
          "none",
          "frame",
          "allocation",
          "comparison",
          "protection",
          "division32",
          "division64"
        )) {
      val c = negative match {
        case "division32" =>
          source.replace("sn_i32 arm_div32", "sn_i16 arm_div32")
        case "division64" =>
          source.replace("sn_i64 arm_div64", "sn_i32 arm_div64")
        case "protection" => source.replace("@sn_xlat_protect_stack 12", "")
        case "frame" => source.replace("@sn_xlat_frameaddr %0", "mov %0, sp")
        case "allocation" =>
          val broken = scoped
            .replace("sn_i32 sn_stack_3[16];", "")
            .replace(
              "  sn_l3 =",
              "  __asm__ volatile(\"@sn_xlat_stackptr %0\" : \"=l\"(sn_l2) : : \"memory\"); sn_l3 ="
            )
            .replace(
              "((void *)sn_stack_3)",
              "((void *)__builtin_alloca(sizeof(sn_i32) * 16))"
            )
            .replace(
              "  clobber_stack();",
              "  __asm__ volatile(\"mov sp, %0\" : : \"r\"(sn_l2) : \"memory\"); clobber_stack();"
            )
          source.replace(scoped, broken)
        case "comparison" =>
          source.linesIterator
            .map { line =>
              if (line.contains("sn_l4 =") && line.contains(">="))
                line.replace("sn_u32", "sn_i32")
              else line
            }
            .mkString("\n")
        case _ => source
      }
      val input = write("probe.c", c)
      val text = Process(
        Seq(
          sys.env("AGBCC"),
          "-mthumb-interwork",
          "-O2",
          "-fhex-asm",
          "-o",
          "-",
          "-"
        ),
        directory.toFile
      ).#<(input.toFile).!!
      val resolved = write("probe.s", AgbccStackFrames.rewrite(text))
      run(Seq(arm + "as", "-mcpu=arm7tdmi", "-o", "probe.o", resolved.toString))
      run(
        Seq(
          arm + "ld",
          "-T",
          root.resolve("rt/gba.ld").toString,
          "-o",
          "probe.elf",
          "crt0.o",
          "caller.o",
          "probe.o",
          Paths
            .get(sys.env("AGBCC"))
            .getParent
            .getParent
            .resolve("lib/libgcc.a")
            .toString
        )
      )
      run(Seq(arm + "objcopy", "-O", "binary", "probe.elf", "probe.gba"))
      val rom = directory.resolve("probe.gba")
      val bytes = Files.readAllBytes(rom)
      bytes(0xbd) = (-(bytes.slice(0xa0, 0xbd).map(_ & 255).sum + 0x19)).toByte
      Files.write(rom, bytes)
      val status = Process(
        Seq(sys.env("GBA_TEST_RUNNER"), rom.toString, "3"),
        directory.toFile
      ).!
      if (negative != "none")
        assertNotEquals(
          "naive SP must fail the entry-stack contract",
          0,
          status
        )
      else assertEquals("entry-stack ROM", 0, status)
    }
  }
}
