package scala.scalanative.codegen.llvm

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.sys.process._

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.nir

class AgbccCodeGenControlFlowTest {
  import nir.{Bin, Comp, Conv, Defn, Inst, Local, Next, Op, Type, Val}
  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
  private implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel
  private val owner = nir.Global.Top("AgbccControlFlowFixture")
  private def global(name: String) = owner.member(nir.Sig.Extern(name))

  @Test def cyclicBlockArgumentsAreCopiedInParallel(): Unit = {
    def local(id: Int) = Val.Local(Local(id), Type.Int)
    val definition = Defn.Define(
      nir.Attrs.None,
      global("rotate"),
      Type.Function(Seq(Type.Int), Type.Int),
      Seq(
        Inst.Label(Local(0), Seq(local(1))),
        Inst.Jump(
          Next
            .Label(Local(2), Seq(local(1), Val.Int(1), Val.Int(2), Val.Int(3)))
        ),
        Inst.Label(Local(2), Seq(local(3), local(4), local(5), local(6))),
        Inst.Let(
          Local(7),
          Op.Comp(Comp.Sgt, Type.Int, local(3), Val.Int(0)),
          Next.None
        ),
        Inst.If(
          Val.Local(Local(7), Type.Bool),
          Next.Label(Local(8), Nil),
          Next.Label(Local(10), Nil)
        ),
        Inst.Label(Local(8), Nil),
        Inst.Let(
          Local(9),
          Op.Bin(Bin.Isub, Type.Int, local(3), Val.Int(1)),
          Next.None
        ),
        Inst.Jump(
          Next.Label(Local(2), Seq(local(9), local(5), local(6), local(4)))
        ),
        Inst.Label(Local(10), Nil),
        Inst.Let(
          Local(11),
          Op.Bin(Bin.Imul, Type.Int, local(4), Val.Int(100)),
          Next.None
        ),
        Inst.Let(
          Local(12),
          Op.Bin(Bin.Imul, Type.Int, local(5), Val.Int(10)),
          Next.None
        ),
        Inst.Let(
          Local(13),
          Op.Bin(Bin.Iadd, Type.Int, local(11), local(12)),
          Next.None
        ),
        Inst.Let(
          Local(14),
          Op.Bin(Bin.Iadd, Type.Int, local(13), local(6)),
          Next.None
        ),
        Inst.Ret(local(14))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val capture = "sn_edge_3 = sn_l4;"
    val assignment = "sn_l4 = sn_edge_1;"
    assertTrue(
      generated,
      generated.indexOf(capture) < generated.lastIndexOf(assignment)
    )
    val harness =
      "int main(void) { static const int expected[] = {123,231,312}; int i; " +
        "for (i = 0; i < 24; ++i) if (rotate(i) != expected[i % 3]) return 1; return 0; }"
    val directory = Files.createTempDirectory("agbcc-parallel-moves")
    for (broken <- Seq(false, true)) {
      // Reintroduce the old overwrite-before-capture on the back edge.
      val code =
        if (broken) generated.replace(capture, assignment + " " + capture)
        else generated
      val source = directory.resolve("rotation.c")
      val binary = directory.resolve("rotation")
      Files.write(source, (code + harness).getBytes(StandardCharsets.UTF_8))
      assertEquals(
        0,
        Process(
          Seq(
            sys.env.getOrElse("CC", "cc"),
            "-std=c99",
            "-O2",
            source.toString,
            "-o",
            binary.toString
          )
        ).!
      )
      val status = Process(Seq(binary.toString)).!
      if (broken)
        assertNotEquals("old cyclic move ordering must fail", 0, status)
      else assertEquals(0, status)
    }
  }

  @Test def literalDivisorCheckDoesNotPullInItsDeadErrorGraph(): Unit = {
    val arg = Val.Local(Local(1), Type.Int)
    val definition = Defn.Define(
      nir.Attrs.None.withIsExtern(true),
      global("constant_check"),
      Type.Function(Seq(Type.Int), Type.Int),
      Seq(
        Inst.Label(Local(0), Seq(arg)),
        Inst.Let(
          Local(2),
          Op.Comp(Comp.Ine, Type.Int, Val.Int(180), Val.Zero(Type.Int)),
          Next.None
        ),
        Inst.If(
          Val.Local(Local(2), Type.Bool),
          Next.Label(Local(3), Seq.empty),
          Next.Label(Local(4), Seq.empty)
        ),
        Inst.Label(Local(3), Seq.empty),
        Inst.Ret(arg),
        Inst.Label(Local(4), Seq.empty),
        Inst.Let(
          Local(5),
          Op.Call(
            Type.Function(Seq.empty, Type.Int),
            Val.Global(global("dead_error"), Type.Ptr),
            Seq.empty
          ),
          Next.None
        ),
        Inst.Ret(Val.Local(Local(5), Type.Int))
      )
    )
    val helper = definition.copy(
      name = global("dead_error"),
      insts = Seq(Inst.Label(Local(0), Seq(arg)), Inst.Ret(arg))
    )
    val prepared = AgbccCodeGen.prepareForAgbcc(definition)
    assertFalse(prepared.insts.exists {
      case Inst.Let(_, _: Op.Call, _) => true; case _ => false
    })
    assertEquals(1, prepared.insts.count(_.isInstanceOf[Inst.Label]))
    assertEquals(
      Seq(definition.name),
      AgbccCodeGen
        .selectReachable(Seq(prepared), Seq(prepared, helper))
        .map(_.name)
    )
    val dynamic = definition.copy(insts = definition.insts.map {
      case inst: Inst.Let if inst.id == Local(2) =>
        inst.copy(op = Op.Comp(Comp.Ine, Type.Int, arg, Val.Zero(Type.Int)))
      case other => other
    })
    assertTrue(AgbccCodeGen.prepareForAgbcc(dynamic).insts.exists {
      case Inst.Let(_, _: Op.Call, _) => true; case _ => false
    })
  }

  private def diamond(
      name: String,
      order: Option[nir.MemoryOrder],
      store: Boolean
  ): Defn.Define = {
    val ptr = Val.Local(Local(1), Type.Ptr)
    val choose = Val.Local(Local(2), Type.Bool)
    val loaded = Val.Local(Local(3), Type.Short)
    val joined = Val.Local(Local(10), Type.Int)
    val barrier =
      if (store)
        Inst.Let(
          Local(4),
          Op.Store(Type.Short, ptr, Val.Short(123), None),
          Next.None
        )
      else
        Inst.Let(
          Local(4),
          Op.Call(
            Type.Function(Seq(Type.Ptr), Type.Int),
            Val.Global(global("observe"), Type.Ptr),
            Seq(ptr)
          ),
          Next.None
        )
    Defn.Define(
      nir.Attrs.None,
      global(name),
      Type.Function(Seq(Type.Ptr, Type.Bool), Type.Int),
      Seq(
        Inst.Label(Local(0), Seq(ptr, choose)),
        Inst.Let(Local(3), Op.Load(Type.Short, ptr, order), Next.None),
        barrier,
        Inst.Let(Local(5), Op.Conv(Conv.Sext, Type.Int, loaded), Next.None),
        Inst.If(
          choose,
          Next.Label(Local(6), Seq.empty),
          Next.Label(Local(8), Seq.empty)
        ),
        Inst.Label(Local(6), Seq.empty),
        Inst.Let(
          Local(7),
          Op.Bin(Bin.Isub, Type.Int, Val.Int(0), Val.Local(Local(5), Type.Int)),
          Next.None
        ),
        Inst.Jump(Next.Label(Local(9), Seq(Val.Local(Local(7), Type.Int)))),
        Inst.Label(Local(8), Seq.empty),
        Inst.Jump(Next.Label(Local(9), Seq(Val.Local(Local(5), Type.Int)))),
        Inst.Label(Local(9), Seq(joined)),
        Inst.Let(Local(11), Op.Conv(Conv.Trunc, Type.Short, joined), Next.None),
        Inst.Let(
          Local(12),
          Op.Conv(Conv.Sext, Type.Int, Val.Local(Local(11), Type.Short)),
          Next.None
        ),
        Inst.Ret(Val.Local(Local(12), Type.Int))
      )
    )
  }

  @Test def narrowReturnTreesPreserveMemorySnapshotsAndScalarWidth(): Unit = {
    val definitions = Seq(
      diamond("before_call", None, false),
      diamond("before_store", None, true),
      diamond("before_unordered_call", Some(nir.MemoryOrder.Unordered), false),
      diamond("before_call_atomic", Some(nir.MemoryOrder.SeqCst), false)
    )
    val signature = Type.Function(Seq(Type.Ptr, Type.Bool), Type.Short)
    val code = AgbccCodeGen.emit(
      definitions,
      definitions,
      definitions
        .map(d =>
          d.name.sig.unmangled.asInstanceOf[nir.Sig.Extern].id -> signature
        )
        .toMap
    )
    val harness =
      """
        |static int calls;
        |sn_i32 observe(void *pointer) { ++calls; *(sn_i16 *)pointer = 123; return 0; }
        |int main(void) {
        |  static const sn_i16 values[] = {-32768, -1, 0, 1, 32767};
        |  int i, negative;
        |  for (i = 0; i < 5; ++i) for (negative = 0; negative < 2; ++negative) {
        |    sn_i16 cell, expected = (sn_i16)(negative ? -values[i] : values[i]);
        |    cell = values[i]; if (before_call(&cell, negative) != expected || cell != 123) return 1;
        |    cell = values[i]; if (before_store(&cell, negative) != expected || cell != 123) return 2;
        |    cell = values[i]; if (before_call_atomic(&cell, negative) != expected || cell != 123) return 3;
        |    cell = values[i]; if (before_unordered_call(&cell, negative) != expected || cell != 123) return 4;
        |  }
        |  return calls == 30 ? 0 : 5;
        |}
        |""".stripMargin
    val dir = Files.createTempDirectory("agbcc-return-snapshots")
    val source = dir.resolve("snapshots.c")
    val executable = dir.resolve("snapshots")
    Files.write(source, (code + harness).getBytes(StandardCharsets.UTF_8))
    assertEquals(
      0,
      Process(
        Seq(
          sys.env.getOrElse("CC", "cc"),
          "-std=c99",
          "-O2",
          source.toString,
          "-o",
          executable.toString
        )
      ).!
    )
    assertEquals(0, Process(Seq(executable.toString)).!)
    val transformed =
      AgbccCodeGen.narrowReturnTails(definitions.head, Type.Short)
    val ids = transformed.insts.collect { case Inst.Let(id, _, _) => id }
    assertEquals(ids.distinct, ids)
    assertEquals(
      definitions.head,
      AgbccCodeGen.narrowReturnTails(definitions.head, Type.Int)
    )
  }

  @Test def returnTailWithACallIsNotDuplicated(): Unit = {
    val definition = diamond("effectful_tail", None, true)
    val changed = definition.copy(insts = definition.insts.map {
      case let: Inst.Let if let.id == Local(11) =>
        let.copy(op =
          Op.Call(
            Type.Function(Seq(Type.Int), Type.Short),
            Val.Global(global("effect"), Type.Ptr),
            Seq(Val.Local(Local(10), Type.Int))
          )
        )
      case other => other
    })
    assertEquals(changed, AgbccCodeGen.narrowReturnTails(changed, Type.Short))
  }
  @Test def narrowTableReturnMatchesIndependentAgbccOracle(): Unit = {
    val arg = Val.Local(Local(1), Type.Char)
    def local(id: Long, ty: Type = Type.Int) = Val.Local(Local(id), ty)
    def binary(id: Long, bin: Bin, left: Val, right: Val) =
      Inst.Let(Local(id), Op.Bin(bin, Type.Int, left, right), Next.None)
    val definition = Defn.Define(
      nir.Attrs.None,
      global("narrow_wave"),
      Type.Function(Seq(Type.Char), Type.Short),
      Seq(
        Inst.Label(Local(0), Seq(arg)),
        Inst.Let(Local(2), Op.Conv(Conv.Zext, Type.Int, arg), Next.None),
        binary(3, Bin.Urem, local(2), Val.Int(180)),
        binary(4, Bin.Udiv, local(2), Val.Int(180)),
        binary(5, Bin.And, local(4), Val.Int(1)),
        Inst.Let(
          Local(6),
          Op.Load(
            Type.Int,
            Val.Global(global("__xlat_addr_wave_table"), Type.Ptr),
            None
          ),
          Next.None
        ),
        binary(7, Bin.Shl, local(3), Val.Int(1)),
        binary(8, Bin.Iadd, local(6), local(7)),
        Inst
          .Let(Local(9), Op.Conv(Conv.Inttoptr, Type.Ptr, local(8)), Next.None),
        Inst.Let(
          Local(10),
          Op.Load(
            Type.Short,
            local(9, Type.Ptr),
            Some(nir.MemoryOrder.Unordered)
          ),
          Next.None
        ),
        Inst.Let(
          Local(11),
          Op.Comp(Comp.Ine, Type.Int, local(5), Val.Zero(Type.Int)),
          Next.None
        ),
        Inst.If(
          local(11, Type.Bool),
          Next.Label(Local(20), Seq.empty),
          Next.Label(Local(30), Seq.empty)
        ),
        Inst.Label(Local(20), Seq.empty),
        Inst.Let(
          Local(21),
          Op.Conv(Conv.Sext, Type.Int, local(10, Type.Short)),
          Next.None
        ),
        binary(22, Bin.Isub, Val.Int(0), local(21)),
        Inst.Let(
          Local(23),
          Op.Conv(Conv.Trunc, Type.Short, local(22)),
          Next.None
        ),
        Inst.Jump(Next.Label(Local(40), Seq(local(23, Type.Short)))),
        Inst.Label(Local(30), Seq.empty),
        Inst.Jump(Next.Label(Local(40), Seq(local(10, Type.Short)))),
        Inst.Label(Local(40), Seq(local(41, Type.Short))),
        Inst.Ret(local(41, Type.Short))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val oracle =
      """
        |extern short wave_table[];
        |short narrow_wave(unsigned short angle) {
        |  int mod = angle % 180;
        |  int negate = (angle / 180) & 1;
        |  short value = wave_table[mod];
        |  if (negate) return -value;
        |  else return value;
        |}
        |""".stripMargin
    org.junit.Assume.assumeTrue(
      "set AGBCC and ARM binutils for object parity",
      sys.env.contains("AGBCC")
    )
    val dir = Files.createTempDirectory("agbcc-narrow-wave")
    val assembler = sys.env.getOrElse("ARM_AS", "arm-none-eabi-as")
    val objcopy = sys.env.getOrElse("ARM_OBJCOPY", "arm-none-eabi-objcopy")
    val readelf = sys.env.getOrElse("ARM_READELF", "arm-none-eabi-readelf")
    def compile(name: String, code: String): (Array[Byte], Seq[String]) = {
      val source = dir.resolve(name + ".c")
      val asm = dir.resolve(name + ".s")
      val obj = dir.resolve(name + ".o")
      val text = dir.resolve(name + ".bin")
      Files.write(source, code.getBytes(StandardCharsets.UTF_8))
      assertEquals(
        0,
        Process(
          Seq(
            sys.env("AGBCC"),
            "-mthumb-interwork",
            "-O2",
            "-fhex-asm",
            "-o",
            asm.toString,
            source.toString
          )
        ).!
      )
      assertEquals(
        0,
        Process(
          Seq(assembler, "-mcpu=arm7tdmi", "-o", obj.toString, asm.toString)
        ).!
      )
      assertEquals(
        0,
        Process(
          Seq(
            objcopy,
            "--only-section=.text",
            "-O",
            "binary",
            obj.toString,
            text.toString
          )
        ).!
      )
      val relocations =
        Process(Seq(readelf, "-rW", obj.toString)).!!.linesIterator
          .filter(_.contains("R_ARM_"))
          .map { line =>
            val fields = line.trim.split("\\s+")
            Seq(fields(0), fields(2), fields(4)).mkString(" ")
          }
          .toSeq
      (Files.readAllBytes(text), relocations)
    }
    val (actualText, actualRelocs) = compile("generated", generated)
    val (expectedText, expectedRelocs) = compile("oracle", oracle)
    assertArrayEquals(expectedText, actualText)
    assertEquals(expectedRelocs, actualRelocs)
  }

}
