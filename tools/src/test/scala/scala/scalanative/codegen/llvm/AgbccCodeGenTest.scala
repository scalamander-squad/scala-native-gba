package scala.scalanative.codegen.llvm

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.sys.process._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import org.junit.Assert._
import org.junit.Assume.assumeTrue
import org.junit.Test

import scala.scalanative.nir
import scala.scalanative.build.{BuildException, Config, GC, Logger, Mode, NativeConfig, ScalaNative}
import scala.scalanative.buildinfo.ScalaNativeBuildInfo
import scala.scalanative.util.Scope

class AgbccCodeGenTest {
  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
  private implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
  private val i32 = nir.Type.Int
  private val owner = nir.Global.Top("AgbccCodeGenFixture")

  private def function(
      name: String,
      args: Seq[nir.Type],
      insts: Seq[nir.Inst]
  ): nir.Defn.Define =
    nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern(name)),
      nir.Type.Function(args, i32),
      insts
    )

  private def fixture: Seq[nir.Defn] = {
    val addArgs = Seq(
      nir.Val.Local(nir.Local(1), i32),
      nir.Val.Local(nir.Local(2), i32)
    )
    val add = function(
      "agbc_match_add",
      Seq(i32, i32),
      Seq(
        nir.Inst.Label(nir.Local(0), addArgs),
        nir.Inst.Let(
          nir.Local(3),
          nir.Op.Bin(nir.Bin.Iadd, i32, addArgs(0), addArgs(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(3), i32))
      )
    )

    val leaf = owner.member(nir.Sig.Extern("agb_match_leaf"))
    val callArg = nir.Val.Local(nir.Local(11), i32)
    val call = function(
      "agbc_match_call",
      Seq(i32),
      Seq(
        nir.Inst.Label(nir.Local(10), Seq(callArg)),
        nir.Inst.Let(
          nir.Local(12),
          nir.Op.Call(
            nir.Type.Function(Seq(i32), i32),
            nir.Val.Global(leaf, nir.Type.Ptr),
            Seq(callArg)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(12), i32))
      )
    )
    val leafDecl = nir.Defn.Declare(
      nir.Attrs.None,
      leaf,
      nir.Type.Function(Seq(i32), i32)
    )
    def binary(name: String, bin: nir.Bin, id: Int): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(id + 1), i32),
        nir.Val.Local(nir.Local(id + 2), i32)
      )
      function(
        name,
        Seq(i32, i32),
        Seq(
          nir.Inst.Label(nir.Local(id), args),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Bin(bin, i32, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), i32))
        )
      )
    }
    Seq(
      add,
      call,
      binary("agbc_match_div", nir.Bin.Sdiv, 20),
      binary("agbc_match_udiv", nir.Bin.Udiv, 30),
      binary("agbc_match_rem", nir.Bin.Srem, 40),
      binary("agbc_match_urem", nir.Bin.Urem, 50),
      leafDecl
    )
  }

  @Test def emitsTypedNirAsCWithStableSymbols(): Unit = {
    val source = AgbccCodeGen.emit(fixture)
    assertTrue(source.contains("agbc_match_add(sn_i32 sn_l1, sn_i32 sn_l2)"))
    assertTrue(source.contains("agbc_match_call(sn_i32 sn_l11)"))
    assertTrue(source.contains("agbc_match_div(sn_i32 sn_l21, sn_i32 sn_l22)"))
    assertTrue(source.contains("agbc_match_udiv(sn_i32 sn_l31, sn_i32 sn_l32)"))
    assertTrue(source.contains("agb_match_leaf(sn_l11)"))
    assertTrue(source.contains("sn_l3 = ((sn_i32)((sn_u32)(sn_l1) + (sn_u32)(sn_l2)))"))
    assertTrue(source.contains("sn_l23 = ((sn_i32)(sn_l21) / (sn_i32)(sn_l22))"))
  }

  @Test def integerDivisionAndRemainderAreEmittedAndDefaultConfigStaysLlvm(): Unit = {
    val base = NativeConfig.empty
    assertFalse("agbcc backend must remain opt-in", base.agbccCodegen)
    assertTrue("default export filter must be empty", base.agbccCodegenExports.isEmpty)

    val optIn = base.withAgbccCodegenExports(Set("only_this_export"))
    assertTrue(optIn.agbccCodegen)
    assertTrue(optIn.agbccAbi)
    assertFalse(optIn.useIncrementalCompilation)
    assertEquals(Set("only_this_export"), optIn.agbccCodegenExports)

    val args = Seq(
      nir.Val.Local(nir.Local(1), i32),
      nir.Val.Local(nir.Local(2), i32)
    )
    def binary(name: String, bin: nir.Bin): nir.Defn.Define =
      function(
        name,
        Seq(i32, i32),
        Seq(
          nir.Inst.Label(nir.Local(0), args),
          nir.Inst.Let(
            nir.Local(3),
            nir.Op.Bin(bin, i32, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(3), i32))
        )
      )
    val source = AgbccCodeGen.emit(Seq(
      binary("agbc_sdiv", nir.Bin.Sdiv),
      binary("agbc_udiv", nir.Bin.Udiv),
      binary("agbc_srem", nir.Bin.Srem),
      binary("agbc_urem", nir.Bin.Urem)
    ))
    assertTrue(source, source.contains("((sn_i32)(sn_l1) / (sn_i32)(sn_l2))"))
    assertTrue(source, source.contains("((sn_u32)(sn_l1) / (sn_u32)(sn_l2))"))
    assertTrue(source, source.contains("((sn_i32)(sn_l1) % (sn_i32)(sn_l2))"))
    assertTrue(source, source.contains("((sn_u32)(sn_l1) % (sn_u32)(sn_l2))"))

    val narrowArgs = Seq(
      nir.Val.Local(nir.Local(11), nir.Type.Byte),
      nir.Val.Local(nir.Local(12), nir.Type.Byte)
    )
    val narrowDivision = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_udiv8")),
      nir.Type.Function(Seq(nir.Type.Byte, nir.Type.Byte), nir.Type.Byte),
      Seq(
        nir.Inst.Label(nir.Local(10), narrowArgs),
        nir.Inst.Let(
          nir.Local(13),
          nir.Op.Bin(nir.Bin.Udiv, nir.Type.Byte, narrowArgs(0), narrowArgs(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(13), nir.Type.Byte))
      )
    )
    val narrowSource = AgbccCodeGen.emit(Seq(narrowDivision))
    assertTrue(
      narrowSource,
      narrowSource.contains(
        "((sn_u32)(sn_u8)(sn_l11)) / ((sn_u32)(sn_u8)(sn_l12))"
      )
    )

    val wrongTarget = base
      .withTargetTriple("x86_64-unknown-linux-gnu")
      .withAgbccCodegen(true)
      .withIncrementalCompilation(false)
      .withLTO(scala.scalanative.build.LTO.none)
    try {
      AgbccCodeGen.validateConfig(wrongTarget)
      fail("agbcc code generation must reject non-GBA targets")
    } catch {
      case error: BuildException =>
        assertTrue(error.getMessage, error.getMessage.contains("armv4t-none-eabi"))
    }
  }

  @Test def generatedFunctionsMatchIndependentAgbccOracleObject(): Unit = {
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy = sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf = sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue("set AGBCC and install the ARM binutils to run object parity", agbcc.isDefined)

    val dir = Files.createTempDirectory("sn-agbcc-codegen-test")
    val generated = AgbccCodeGen.emit(fixture)
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |
        |extern sn_i32 agb_match_leaf(sn_i32);
        |extern sn_i32 agbc_match_add(sn_i32, sn_i32);
        |extern sn_i32 agbc_match_call(sn_i32);
        |extern sn_i32 agbc_match_div(sn_i32, sn_i32);
        |extern sn_i32 agbc_match_udiv(sn_i32, sn_i32);
        |extern sn_i32 agbc_match_rem(sn_i32, sn_i32);
        |extern sn_i32 agbc_match_urem(sn_i32, sn_i32);
        |
        |sn_i32 agbc_match_add(sn_i32 sn_l1, sn_i32 sn_l2) {
        |  sn_i32 sn_l3;
        |
        |sn_label_0: ;
        |  sn_l3 = ((sn_i32)((sn_u32)(sn_l1) + (sn_u32)(sn_l2)));
        |  return sn_l3;
        |}
        |
        |sn_i32 agbc_match_call(sn_i32 sn_l11) {
        |  sn_i32 sn_l12;
        |
        |sn_label_10: ;
        |  sn_l12 = agb_match_leaf(sn_l11);
        |  return sn_l12;
        |}
        |
        |sn_i32 agbc_match_div(sn_i32 sn_l21, sn_i32 sn_l22) {
        |  sn_i32 sn_l23;
        |
        |sn_label_20: ;
        |  sn_l23 = ((sn_i32)(sn_l21) / (sn_i32)(sn_l22));
        |  return sn_l23;
        |}
        |
        |sn_i32 agbc_match_rem(sn_i32 sn_l41, sn_i32 sn_l42) {
        |  sn_i32 sn_l43;
        |
        |sn_label_40: ;
        |  sn_l43 = ((sn_i32)(sn_l41) % (sn_i32)(sn_l42));
        |  return sn_l43;
        |}
        |
        |sn_i32 agbc_match_udiv(sn_i32 sn_l31, sn_i32 sn_l32) {
        |  sn_i32 sn_l33;
        |
        |sn_label_30: ;
        |  sn_l33 = ((sn_u32)(sn_l31) / (sn_u32)(sn_l32));
        |  return sn_l33;
        |}
        |
        |sn_i32 agbc_match_urem(sn_i32 sn_l51, sn_i32 sn_l52) {
        |  sn_i32 sn_l53;
        |
        |sn_label_50: ;
        |  sn_l53 = ((sn_u32)(sn_l51) % (sn_u32)(sn_l52));
        |  return sn_l53;
        |}
        |""".stripMargin

    val generatedObject = compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText = extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(".text bytes differ", Files.readAllBytes(oracleText), Files.readAllBytes(generatedText))

    val generatedRelocs = relocationRows(readelf.get, generatedObject)
    val oracleRelocs = relocationRows(readelf.get, oracleObject)
    assertEquals("relocation rows differ", oracleRelocs, generatedRelocs)
  }

  @Test def exportedScalaIntegerFunctionFlowsThroughLoweringToAgbcc(): Unit = {
    val out = Files.createTempDirectory("sn-agbcc-scala-export")
    val nirFiles = scala.scalanative.NIRCompiler.getCompiler(out).compile(
      """
        |object AgbccScalaExportFixture {
        |  @scala.scalanative.unsafe.exported("agbc_scala_add")
        |  def add(a: Int, b: Int): Int = a + b
        |  @scala.scalanative.unsafe.exported("agbc_scala_max")
        |  def max(a: Int, b: Int): Int = if a >= b then a else b
        |  @scala.scalanative.unsafe.exported("agbc_scala_longadd")
        |  def longAdd(a: Long, b: Long): Long = a + b
        |  @scala.scalanative.unsafe.exported("agbc_scala_muladd")
        |  def mulAdd(a: Int, b: Int, c: Int): Int = a * b + c
        |}
        |@scala.scalanative.unsafe.extern
        |object AgbccRngState {
        |  @scala.scalanative.unsafe.name("gRngValue")
        |  var value: Int = scala.scalanative.unsafe.extern
        |  @scala.scalanative.unsafe.name("xlat_random_sRandCount")
        |  var count: Int = scala.scalanative.unsafe.extern
        |  @scala.scalanative.unsafe.name("xlat_random_sUnknown")
        |  var unknown: Byte = scala.scalanative.unsafe.extern
        |}
        |object AgbccRandomFixture {
        |  @scala.scalanative.unsafe.exported("Random")
        |  def random(): Int = {
        |    AgbccRngState.value = AgbccRngState.value * 1103515245 + 24691
        |    AgbccRngState.count += 1
        |    AgbccRngState.value >>> 16
        |  }
        |  @scala.scalanative.unsafe.exported("SeedRng")
        |  def seed(seed: Int): Unit = {
        |    AgbccRngState.value = seed & 0xFFFF
        |    AgbccRngState.unknown = 0
        |  }
        |}
        |""".stripMargin
    )
    assertTrue("Scala compiler did not produce NIR", nirFiles.nonEmpty)

    val runtime = ScalaNativeBuildInfo.nativeRuntimeClasspath
      .split(java.io.File.pathSeparator)
      .filter(_.nonEmpty)
      .map(java.nio.file.Paths.get(_))
      .toSeq
    val nativeConfig = scala.scalanative.build.NativeConfig.empty
      .withTargetTriple("armv4t-none-eabi")
      .withAgbccCodegenExports(
        Set(
          "agbc_scala_add",
          "agbc_scala_longadd",
          "agbc_scala_max",
          "agbc_scala_muladd",
          "Random",
          "SeedRng"
        )
      )
      .withGC(GC.none)
      .withMode(Mode.releaseFull)
      .withLTO(scala.scalanative.build.LTO.none)
      .withMultithreading(false)
    val config = Config.empty
      .withBaseDir(out)
      .withClassPath(runtime :+ out)
      .withMainClass(None)
      .withTestConfig(false)
      .withCompilerConfig(nativeConfig)
      .withLogger(Logger.nullLogger)

    implicit val scope: Scope = Scope.unsafe()
    val linked = Await.result(ScalaNative.link(config, ScalaNative.entries(config)), 2.minutes)
    val optimized = Await.result(ScalaNative.optimize(config, linked), 2.minutes)
    val generated = Await.result(ScalaNative.codegen(config, optimized), 2.minutes)
    val paths = Await.result(Future.sequence(generated), 2.minutes)
    val cSources = paths.filter(_.toString.endsWith(AgbccCodeGen.SourceExtension))
    assertEquals("expected Scala exports to enter the agbcc backend", 1, cSources.size)
    val source = new String(Files.readAllBytes(cSources.head), StandardCharsets.UTF_8)
    assertTrue(source, source.contains("agbc_scala_add"))
    assertTrue(source, source.contains("agbc_scala_max"))
    assertTrue(source, source.contains("agbc_scala_longadd"))
    assertTrue(source, source.contains("agbc_scala_muladd"))
    assertTrue(source, source.contains("Random(void)"))
    assertTrue(source, source.contains("SeedRng"))
    val agbcc = sys.env.get("AGBCC")
    assumeTrue("set AGBCC to compile the Scala-exported backend output", agbcc.isDefined)
    val objectFile = compile(source, "scala-export", out, agbcc.get,
      sys.env.getOrElse("ARM_AS", "arm-none-eabi-as"))
    assertTrue("agbcc did not produce an object for the Scala export", Files.size(objectFile) > 0)
    val oracle =
      """typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef unsigned char sn_u8;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |extern sn_u32 gRngValue;
        |extern sn_u32 xlat_random_sRandCount;
        |extern sn_u8 xlat_random_sUnknown;
        |sn_i32 Random(void) {
        |  gRngValue = (sn_u32)(gRngValue * 1103515245u + 24691u);
        |  xlat_random_sRandCount += 1;
        |  return (sn_i32)(gRngValue >> 16);
        |}
        |void SeedRng(sn_i32 seed) {
        |  sn_i32 masked = seed & 65535;
        |  gRngValue = masked;
        |  xlat_random_sUnknown = 0;
        |}
        |sn_i32 agbc_scala_add(sn_i32 a, sn_i32 b) {
        |  return (sn_i32)((sn_u32)a + (sn_u32)b);
        |}
        |sn_i64 agbc_scala_longadd(sn_i64 a, sn_i64 b) {
        |  return (sn_i64)((sn_u64)a + (sn_u64)b);
        |}
        |sn_i32 agbc_scala_max(sn_i32 a, sn_i32 b) {
        |  sn_u8 cond = (sn_u8)(a >= b);
        |  sn_i32 result;
        |  if (cond) goto take_a; else goto take_b;
        |take_a: ;
        |  { sn_i32 edge = a; result = edge; goto done; }
        |take_b: ;
        |  { sn_i32 edge = b; result = edge; goto done; }
        |done: ;
        |  return result;
        |}
        |sn_i32 agbc_scala_muladd(sn_i32 a, sn_i32 b, sn_i32 c) {
        |  return (sn_i32)((sn_u32)a * (sn_u32)b + (sn_u32)c);
        |}
        |""".stripMargin
    val oracleObject = compile(
      oracle,
      "scala-export-oracle",
      out,
      agbcc.get,
      sys.env.getOrElse("ARM_AS", "arm-none-eabi-as")
    )
    val objcopy = sys.env.getOrElse("ARM_OBJCOPY", "arm-none-eabi-objcopy")
    val readelf = sys.env.getOrElse("ARM_READELF", "arm-none-eabi-readelf")
    assertArrayEquals(
      "Scala-exported function .text differs from agbcc semantic oracle",
      Files.readAllBytes(extractSection(objcopy, oracleObject, "scala-oracle.text")),
      Files.readAllBytes(extractSection(objcopy, objectFile, "scala-export.text"))
    )
    assertEquals(
      "Scala-exported function relocations differ from agbcc semantic oracle",
      relocationRows(readelf, oracleObject),
      relocationRows(readelf, objectFile)
    )
  }

  private def compile(
      source: String,
      name: String,
      dir: Path,
      agbcc: String,
      assembler: String
  ): Path = {
    val cFile = dir.resolve(name + ".c")
    val asmFile = dir.resolve(name + ".s")
    val objFile = dir.resolve(name + ".o")
    Files.write(cFile, source.getBytes(StandardCharsets.UTF_8))
    val compiler =
      Seq(agbcc, "-mthumb-interwork", "-O2", "-fhex-asm", "-o", "-", "-")
    val compileResult =
      (Process(compiler, dir.toFile) #< cFile.toFile #> asmFile.toFile).!
    assertEquals("agbcc failed", 0, compileResult)
    val assembleResult = Process(
      Seq(assembler, "-mcpu=arm7tdmi", "-o", objFile.toString, asmFile.toString),
      dir.toFile
    ).!
    assertEquals("ARM assembler failed", 0, assembleResult)
    objFile
  }

  private def extractSection(tool: String, obj: Path, out: String): Path = {
    val section = obj.getParent.resolve(out)
    val result = Process(
      Seq(tool, "--only-section=.text", "-O", "binary", obj.toString, section.toString)
    ).!
    assertEquals("objcopy failed", 0, result)
    section
  }

  private def relocationRows(tool: String, obj: Path): Seq[String] = {
    val output = new StringBuilder
    val result = Process(Seq(tool, "-rW", obj.toString)).!(ProcessLogger(
      line => output.append(line).append('\n'),
      line => output.append(line).append('\n')
    ))
    assertEquals("readelf failed", 0, result)
    output.toString.linesIterator
      .map(_.trim)
      .filter(_.matches("[0-9a-fA-F]+\\s+[0-9a-fA-F]+\\s+R_ARM_[A-Z0-9_]+\\s+.*"))
      .toSeq
  }
}
