package scala.scalanative.codegen.llvm

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.sys.process._

import org.junit.Assert._
import org.junit.Assume.assumeTrue
import org.junit.Test

import scala.scalanative.build.{
  BuildException, Config, GC, Logger, Mode, NativeConfig, ScalaNative
}
import scala.scalanative.buildinfo.ScalaNativeBuildInfo
import scala.scalanative.nir
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
    def pointerComparison(
        name: String,
        comp: nir.Comp,
        id: Int
    ): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(id + 1), nir.Type.Ptr),
        nir.Val.Local(nir.Local(id + 2), nir.Type.Ptr)
      )
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(nir.Type.Ptr, nir.Type.Ptr), nir.Type.Bool),
        Seq(
          nir.Inst.Label(nir.Local(id), args),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Comp(comp, nir.Type.Ptr, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), nir.Type.Bool))
        )
      )
    }
    def integerComparison(
        name: String,
        comp: nir.Comp,
        ty: nir.Type,
        id: Int
    ): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(id + 1), ty),
        nir.Val.Local(nir.Local(id + 2), ty)
      )
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), nir.Type.Bool),
        Seq(
          nir.Inst.Label(nir.Local(id), args),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Comp(comp, ty, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), nir.Type.Bool))
        )
      )
    }
    def floatingBinary(
        name: String,
        bin: nir.Bin,
        ty: nir.Type,
        id: Int
    ): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(id + 1), ty),
        nir.Val.Local(nir.Local(id + 2), ty)
      )
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), ty),
        Seq(
          nir.Inst.Label(nir.Local(id), args),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Bin(bin, ty, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), ty))
        )
      )
    }
    def floatingComparison(
        name: String,
        comp: nir.Comp,
        ty: nir.Type,
        id: Int
    ): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(id + 1), ty),
        nir.Val.Local(nir.Local(id + 2), ty)
      )
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), nir.Type.Bool),
        Seq(
          nir.Inst.Label(nir.Local(id), args),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Comp(comp, ty, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), nir.Type.Bool))
        )
      )
    }
    def numericConversion(
        name: String,
        conv: nir.Conv,
        from: nir.Type,
        to: nir.Type,
        id: Int
    ): nir.Defn.Define = {
      val input = nir.Val.Local(nir.Local(id + 1), from)
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(from), to),
        Seq(
          nir.Inst.Label(nir.Local(id), Seq(input)),
          nir.Inst.Let(
            nir.Local(id + 2),
            nir.Op.Conv(conv, to, input),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 2), to))
        )
      )
    }
    val base = nir.Val.Local(nir.Local(82), nir.Type.Ptr)
    val index = nir.Val.Local(nir.Local(83), nir.Type.Int)
    val structElementPointer = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_elem_struct")),
      nir.Type.Function(Seq(nir.Type.Ptr, nir.Type.Int), nir.Type.Ptr),
      Seq(
        nir.Inst.Label(nir.Local(81), Seq(base, index)),
        nir.Inst.Let(
          nir.Local(84),
          nir.Op.Elem(
            nir.Type.StructValue(Seq(nir.Type.Byte, nir.Type.Int)),
            base,
            Seq(index, nir.Val.Int(1))
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(84), nir.Type.Ptr))
      )
    )
    val stackCount = nir.Val.Local(nir.Local(92), nir.Type.Int)
    val stackalloc = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_stackalloc")),
      nir.Type.Function(Seq(nir.Type.Int), nir.Type.Ptr),
      Seq(
        nir.Inst.Label(nir.Local(91), Seq(stackCount)),
        nir.Inst.Let(
          nir.Local(93),
          nir.Op.Stackalloc(nir.Type.Int, stackCount),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(93), nir.Type.Ptr))
      )
    )
    val pairType = nir.Type.StructValue(Seq(nir.Type.Byte, nir.Type.Int))
    val pair = nir.Val.Local(nir.Local(102), pairType)
    val structExtract = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_struct_extract")),
      nir.Type.Function(Seq(pairType), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(101), Seq(pair)),
        nir.Inst.Let(
          nir.Local(103),
          nir.Op.Extract(pair, Seq(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(103), nir.Type.Int))
      )
    )
    val pairInput = nir.Val.Local(nir.Local(112), pairType)
    val replacement = nir.Val.Local(nir.Local(113), nir.Type.Int)
    val structInsert = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_struct_insert")),
      nir.Type.Function(Seq(pairType, nir.Type.Int), pairType),
      Seq(
        nir.Inst.Label(nir.Local(111), Seq(pairInput, replacement)),
        nir.Inst.Let(
          nir.Local(114),
          nir.Op.Insert(pairInput, replacement, Seq(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(114), pairType))
      )
    )
    def aggregateMeasure(
        name: String,
        op: nir.Op,
        id: Int
    ): nir.Defn.Define =
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq.empty, nir.Type.Size),
        Seq(
          nir.Inst.Label(nir.Local(id), Seq.empty),
          nir.Inst.Let(nir.Local(id + 1), op, nir.Next.None),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 1), nir.Type.Size))
        )
      )
    val pairSize =
      aggregateMeasure("agbc_match_size_pair", nir.Op.SizeOf(pairType), 171)
    val pairAlignment = aggregateMeasure(
      "agbc_match_align_pair",
      nir.Op.AlignmentOf(pairType),
      181
    )
    val floatInput = nir.Val.Local(nir.Local(232), nir.Type.Float)
    val floatMultiplyConstant = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_fmul_const")),
      nir.Type.Function(Seq(nir.Type.Float), nir.Type.Float),
      Seq(
        nir.Inst.Label(nir.Local(231), Seq(floatInput)),
        nir.Inst.Let(
          nir.Local(233),
          nir.Op.Bin(
            nir.Bin.Fmul,
            nir.Type.Float,
            floatInput,
            nir.Val.Float(0.5f)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(233), nir.Type.Float))
      )
    )
    val wordsType = nir.Type.ArrayValue(nir.Type.Int, 2)
    val words = nir.Val.Local(nir.Local(122), wordsType)
    val arrayExtract = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_array_extract")),
      nir.Type.Function(Seq(wordsType), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(121), Seq(words)),
        nir.Inst.Let(
          nir.Local(123),
          nir.Op.Extract(words, Seq(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(123), nir.Type.Int))
      )
    )
    val wordsInput = nir.Val.Local(nir.Local(132), wordsType)
    val wordReplacement = nir.Val.Local(nir.Local(133), nir.Type.Int)
    val arrayInsert = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_array_insert")),
      nir.Type.Function(Seq(wordsType, nir.Type.Int), wordsType),
      Seq(
        nir.Inst.Label(nir.Local(131), Seq(wordsInput, wordReplacement)),
        nir.Inst.Let(
          nir.Local(134),
          nir.Op.Insert(wordsInput, wordReplacement, Seq(1)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(134), wordsType))
      )
    )
    val arrayZero = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_array_zero")),
      nir.Type.Function(Seq.empty, wordsType),
      Seq(
        nir.Inst.Label(nir.Local(141), Seq.empty),
        nir.Inst.Let(
          nir.Local(142),
          nir.Op.Copy(nir.Val.Zero(wordsType)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(142), wordsType))
      )
    )
    Seq(
      add,
      arrayExtract,
      arrayInsert,
      arrayZero,
      call,
      binary("agbc_match_div", nir.Bin.Sdiv, 20),
      floatingBinary("agbc_match_fadd", nir.Bin.Fadd, nir.Type.Float, 201),
      floatingBinary("agbc_match_fdiv", nir.Bin.Fdiv, nir.Type.Double, 211),
      floatMultiplyConstant,
      floatingComparison("agbc_match_flt", nir.Comp.Flt, nir.Type.Float, 221),
      integerComparison("agbc_match_ule16", nir.Comp.Ule, nir.Type.Short, 150),
      integerComparison("agbc_match_ult8", nir.Comp.Ult, nir.Type.Byte, 160),
      numericConversion(
        "agbc_match_z_fptosi",
        nir.Conv.Fptosi,
        nir.Type.Float,
        nir.Type.Int,
        251
      ),
      numericConversion(
        "agbc_match_z_sitofp",
        nir.Conv.Sitofp,
        nir.Type.Int,
        nir.Type.Float,
        261
      ),
      pointerComparison("agbc_match_ptr_eq", nir.Comp.Ieq, 60),
      pointerComparison("agbc_match_ptr_ne", nir.Comp.Ine, 70),
      structElementPointer,
      stackalloc,
      structExtract,
      structInsert,
      pairSize,
      pairAlignment,
      binary("agbc_match_udiv", nir.Bin.Udiv, 30),
      binary("agbc_match_rem", nir.Bin.Srem, 40),
      binary("agbc_match_urem", nir.Bin.Urem, 50),
      leafDecl
    )
  }

  @Test def ignoresAggregateTypesOutsideSelectedDefinitions(): Unit = {
    val unusedType = nir.Type.ArrayValue(nir.Type.Int, 4)
    val unused = nir.Defn.Const(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_unused_array")),
      unusedType,
      nir.Val.Zero(unusedType)
    )
    val source = AgbccCodeGen.emit(Seq(fixture.head), fixture :+ unused)
    assertFalse(source.contains("sn_values[4]"))
    assertFalse(source.contains("agbc_unused_array"))
  }

  @Test def rejectsNonFiniteFloatingConstants(): Unit = {
    Seq(
      nir.Val.Float(Float.NaN),
      nir.Val.Float(Float.PositiveInfinity),
      nir.Val.Double(Double.NaN),
      nir.Val.Double(Double.NegativeInfinity)
    ).zipWithIndex.foreach {
      case (constant, index) =>
        val result = nir.Local(300 + index * 2)
        val definition = nir.Defn.Define(
          nir.Attrs.None,
          owner.member(nir.Sig.Extern(s"agbc_non_finite_$index")),
          nir.Type.Function(Seq.empty, constant.ty),
          Seq(
            nir.Inst.Label(nir.Local(299 + index * 2), Seq.empty),
            nir.Inst.Let(result, nir.Op.Copy(constant), nir.Next.None),
            nir.Inst.Ret(nir.Val.Local(result, constant.ty))
          )
        )
        try {
          AgbccCodeGen.emit(Seq(definition))
          fail(s"expected non-finite constant $constant to be rejected")
        } catch {
          case error: BuildException =>
            assertTrue(
              error.getMessage,
              error.getMessage.contains("non-finite")
            )
        }
    }
  }

  @Test def emitsTypedNirAsCWithStableSymbols(): Unit = {
    val source = AgbccCodeGen.emit(fixture)
    assertTrue(source.contains("agbc_match_add(sn_i32 sn_l1, sn_i32 sn_l2)"))
    assertTrue(source.contains("agbc_match_call(sn_i32 sn_l11)"))
    assertTrue(source.contains("agbc_match_div(sn_i32 sn_l21, sn_i32 sn_l22)"))
    assertTrue(source.contains("agbc_match_udiv(sn_i32 sn_l31, sn_i32 sn_l32)"))
    assertTrue(
      source.contains("agbc_match_ptr_eq(void * sn_l61, void * sn_l62)")
    )
    assertTrue(source.contains("agb_match_leaf(sn_l11)"))
    assertTrue(
      source.contains("sn_l3 = ((sn_i32)((sn_u32)(sn_l1) + (sn_u32)(sn_l2)))")
    )
    assertTrue(
      source.contains("sn_l23 = ((sn_i32)(sn_l21) / (sn_i32)(sn_l22))")
    )
    assertTrue(
      source.contains("sn_l63 = ((sn_u8)((sn_l61) == (sn_l62)))")
    )
    assertTrue(
      source.contains("sn_l73 = ((sn_u8)((sn_l71) != (sn_l72)))")
    )
  }

  @Test def integerDivisionAndRemainderAreEmittedAndDefaultConfigStaysLlvm()
      : Unit = {
    val base = NativeConfig.empty
    assertFalse("agbcc backend must remain opt-in", base.agbccCodegen)
    assertTrue(
      "default export filter must be empty",
      base.agbccCodegenExports.isEmpty
    )

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
    val source = AgbccCodeGen.emit(
      Seq(
        binary("agbc_sdiv", nir.Bin.Sdiv),
        binary("agbc_udiv", nir.Bin.Udiv),
        binary("agbc_srem", nir.Bin.Srem),
        binary("agbc_urem", nir.Bin.Urem)
      )
    )
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
        assertTrue(
          error.getMessage,
          error.getMessage.contains("armv4t-none-eabi")
        )
    }
  }

  @Test def generatedFunctionsMatchIndependentAgbccOracleObject(): Unit = {
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and install the ARM binutils to run object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-codegen-test")
    val generated = AgbccCodeGen.emit(fixture)
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |typedef struct { sn_i8 f0; sn_i32 f1; } sn_pair;
        |typedef struct { sn_i32 sn_values[2]; } sn_words;
        |
        |extern sn_i32 agb_match_leaf(sn_i32);
        |extern sn_i32 agbc_match_add(sn_i32, sn_i32);
        |extern sn_u32 agbc_match_align_pair(void);
        |extern sn_i32 agbc_match_array_extract(sn_words);
        |extern sn_words agbc_match_array_insert(sn_words, sn_i32);
        |extern sn_words agbc_match_array_zero(void);
        |extern sn_i32 agbc_match_call(sn_i32);
        |extern sn_i32 agbc_match_div(sn_i32, sn_i32);
        |extern sn_f32 agbc_match_fadd(sn_f32, sn_f32);
        |extern sn_f64 agbc_match_fdiv(sn_f64, sn_f64);
        |extern sn_f32 agbc_match_fmul_const(sn_f32);
        |extern sn_u8 agbc_match_flt(sn_f32, sn_f32);
        |extern sn_i32 agbc_match_udiv(sn_i32, sn_i32);
        |extern sn_i32 agbc_match_rem(sn_i32, sn_i32);
        |extern sn_u32 agbc_match_size_pair(void);
        |extern sn_i32 agbc_match_urem(sn_i32, sn_i32);
        |extern sn_u8 agbc_match_ule16(sn_i16, sn_i16);
        |extern sn_u8 agbc_match_ult8(sn_i8, sn_i8);
        |extern sn_i32 agbc_match_z_fptosi(sn_f32);
        |extern sn_f32 agbc_match_z_sitofp(sn_i32);
        |extern sn_u8 agbc_match_ptr_eq(void *, void *);
        |extern sn_u8 agbc_match_ptr_ne(void *, void *);
        |extern void * agbc_match_elem_struct(void *, sn_i32);
        |extern sn_i32 agbc_match_struct_extract(sn_pair);
        |extern sn_pair agbc_match_struct_insert(sn_pair, sn_i32);
        |
        |sn_i32 agbc_match_add(sn_i32 sn_l1, sn_i32 sn_l2) {
        |  sn_i32 sn_l3;
        |
        |sn_label_0: ;
        |  sn_l3 = ((sn_i32)((sn_u32)(sn_l1) + (sn_u32)(sn_l2)));
        |  return sn_l3;
        |}
        |
        |/* AGBC_ALIGN_PAIR_ORACLE */
        |/* AGBC_ARRAY_EXTRACT_ORACLE */
        |/* AGBC_ARRAY_INSERT_ORACLE */
        |/* AGBC_ARRAY_ZERO_ORACLE */
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
        |/* AGBC_ELEM_ORACLE */
        |/* AGBC_FADD_ORACLE */
        |/* AGBC_FDIV_ORACLE */
        |/* AGBC_FLT_ORACLE */
        |/* AGBC_FMUL_CONST_ORACLE */
        |
        |
        |sn_u8 agbc_match_ptr_eq(void * sn_l61, void * sn_l62) {
        |  sn_u8 sn_l63;
        |
        |sn_label_60: ;
        |  sn_l63 = ((sn_u8)((sn_l61) == (sn_l62)));
        |  return sn_l63;
        |}
        |
        |sn_u8 agbc_match_ptr_ne(void * sn_l71, void * sn_l72) {
        |  sn_u8 sn_l73;
        |
        |sn_label_70: ;
        |  sn_l73 = ((sn_u8)((sn_l71) != (sn_l72)));
        |  return sn_l73;
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
        |/* AGBC_SIZE_PAIR_ORACLE */
        |/* AGBC_STACKALLOC_ORACLE */
        |/* AGBC_STRUCT_ORACLE */
        |/* AGBC_STRUCT_INSERT_ORACLE */
        |
        |sn_i32 agbc_match_udiv(sn_i32 sn_l31, sn_i32 sn_l32) {
        |  sn_i32 sn_l33;
        |
        |sn_label_30: ;
        |  sn_l33 = ((sn_u32)(sn_l31) / (sn_u32)(sn_l32));
        |  return sn_l33;
        |}
        |
        |/* AGBC_ULE16_ORACLE */
        |/* AGBC_ULT8_ORACLE */
        |
        |sn_i32 agbc_match_urem(sn_i32 sn_l51, sn_i32 sn_l52) {
        |  sn_i32 sn_l53;
        |
        |sn_label_50: ;
        |  sn_l53 = ((sn_u32)(sn_l51) % (sn_u32)(sn_l52));
        |  return sn_l53;
        |}
        |
        |/* AGBC_Z_FPTOSI_ORACLE */
        |/* AGBC_Z_SITOFP_ORACLE */
        |""".stripMargin

    val oracleWithElem = oracle.replace(
      "/* AGBC_ELEM_ORACLE */",
      """typedef struct { sn_i8 f0; sn_i32 f1; } sn_elem_struct;
        |void * agbc_match_elem_struct(void * base, sn_i32 index) {
        |  return &((sn_elem_struct *)base)[index].f1;
        |}""".stripMargin
    )
    val oracleWithPairAlignment = oracleWithElem.replace(
      "/* AGBC_ALIGN_PAIR_ORACLE */",
      """sn_u32 agbc_match_align_pair(void) {
        |  sn_u32 sn_l182;
        |
        |sn_label_181: ;
        |  sn_l182 = ((sn_i32)__alignof__(sn_pair));
        |  return sn_l182;
        |}""".stripMargin
    )
    val oracleWithFloatAdd = oracleWithPairAlignment.replace(
      "/* AGBC_FADD_ORACLE */",
      """sn_f32 agbc_match_fadd(sn_f32 sn_l202, sn_f32 sn_l203) {
        |  sn_f32 sn_l204;
        |
        |sn_label_201: ;
        |  sn_l204 = ((sn_f32)(sn_l202 + sn_l203));
        |  return sn_l204;
        |}""".stripMargin
    )
    val oracleWithFloatDiv = oracleWithFloatAdd.replace(
      "/* AGBC_FDIV_ORACLE */",
      """sn_f64 agbc_match_fdiv(sn_f64 sn_l212, sn_f64 sn_l213) {
        |  sn_f64 sn_l214;
        |
        |sn_label_211: ;
        |  sn_l214 = ((sn_f64)(sn_l212 / sn_l213));
        |  return sn_l214;
        |}""".stripMargin
    )
    val oracleWithFloatConst = oracleWithFloatDiv.replace(
      "/* AGBC_FMUL_CONST_ORACLE */",
      """sn_f32 agbc_match_fmul_const(sn_f32 sn_l232) {
        |  sn_f32 sn_l233;
        |
        |sn_label_231: ;
        |  sn_l233 = ((sn_f32)(sn_l232 * 0.5f));
        |  return sn_l233;
        |}""".stripMargin
    )
    val oracleWithFloatCompare = oracleWithFloatConst.replace(
      "/* AGBC_FLT_ORACLE */",
      """sn_u8 agbc_match_flt(sn_f32 sn_l222, sn_f32 sn_l223) {
        |  sn_u8 sn_l224;
        |
        |sn_label_221: ;
        |  sn_l224 = ((sn_u8)((sn_l222) < (sn_l223)));
        |  return sn_l224;
        |}""".stripMargin
    )
    val oracleWithStackalloc = oracleWithFloatCompare.replace(
      "/* AGBC_STACKALLOC_ORACLE */",
      """void * agbc_match_stackalloc(sn_i32 count) {
        |  return __builtin_alloca(sizeof(sn_i32) * count);
        |}""".stripMargin
    )
    val oracleWithStruct = oracleWithStackalloc.replace(
      "/* AGBC_STRUCT_ORACLE */",
      """sn_i32 agbc_match_struct_extract(sn_pair value) {
        |  return value.f1;
        |}""".stripMargin
    )
    val oracleWithStructInsert = oracleWithStruct.replace(
      "/* AGBC_STRUCT_INSERT_ORACLE */",
      """sn_pair agbc_match_struct_insert(sn_pair value, sn_i32 replacement) {
        |  sn_pair result = value;
        |  result.f1 = replacement;
        |  return result;
        |}""".stripMargin
    )
    val oracleWithSizePair = oracleWithStructInsert.replace(
      "/* AGBC_SIZE_PAIR_ORACLE */",
      """sn_u32 agbc_match_size_pair(void) {
        |  sn_u32 sn_l172;
        |
        |sn_label_171: ;
        |  sn_l172 = ((sn_i32)sizeof(sn_pair));
        |  return sn_l172;
        |}""".stripMargin
    )
    val oracleWithArrayExtract = oracleWithSizePair.replace(
      "/* AGBC_ARRAY_EXTRACT_ORACLE */",
      """sn_i32 agbc_match_array_extract(sn_words value) {
        |  return value.sn_values[1];
        |}""".stripMargin
    )
    val oracleWithArrayInsert = oracleWithArrayExtract.replace(
      "/* AGBC_ARRAY_INSERT_ORACLE */",
      """sn_words agbc_match_array_insert(sn_words value, sn_i32 replacement) {
        |  sn_words result = value;
        |  result.sn_values[1] = replacement;
        |  return result;
        |}""".stripMargin
    )
    val oracleWithArrayZero = oracleWithArrayInsert.replace(
      "/* AGBC_ARRAY_ZERO_ORACLE */",
      """sn_words agbc_match_array_zero(void) {
        |  sn_words sn_l142;
        |
        |sn_label_141: ;
        |  sn_l142 = ((sn_words){0});
        |  return sn_l142;
        |}""".stripMargin
    )
    val oracleWithUle16 = oracleWithArrayZero.replace(
      "/* AGBC_ULE16_ORACLE */",
      """sn_u8 agbc_match_ule16(sn_i16 sn_l151, sn_i16 sn_l152) {
        |  sn_u8 sn_l153;
        |
        |sn_label_150: ;
        |  sn_l153 = ((sn_u8)(((sn_u32)(sn_u16)(sn_l151)) <= ((sn_u32)(sn_u16)(sn_l152))));
        |  return sn_l153;
        |}""".stripMargin
    )
    val oracleWithUlt8 = oracleWithUle16.replace(
      "/* AGBC_ULT8_ORACLE */",
      """sn_u8 agbc_match_ult8(sn_i8 sn_l161, sn_i8 sn_l162) {
        |  sn_u8 sn_l163;
        |
        |sn_label_160: ;
        |  sn_l163 = ((sn_u8)(((sn_u32)(sn_u8)(sn_l161)) < ((sn_u32)(sn_u8)(sn_l162))));
        |  return sn_l163;
        |}""".stripMargin
    )
    val oracleWithFptosi = oracleWithUlt8.replace(
      "/* AGBC_Z_FPTOSI_ORACLE */",
      """sn_i32 agbc_match_z_fptosi(sn_f32 sn_l252) {
        |  sn_i32 sn_l253;
        |
        |sn_label_251: ;
        |  sn_l253 = ((sn_i32)(sn_l252));
        |  return sn_l253;
        |}""".stripMargin
    )
    val oracleWithSitofp = oracleWithFptosi.replace(
      "/* AGBC_Z_SITOFP_ORACLE */",
      """sn_f32 agbc_match_z_sitofp(sn_i32 sn_l262) {
        |  sn_f32 sn_l263;
        |
        |sn_label_261: ;
        |  sn_l263 = ((sn_f32)(sn_l262));
        |  return sn_l263;
        |}""".stripMargin
    )
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject =
      compile(oracleWithSitofp, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )

    val generatedRelocs = relocationRows(readelf.get, generatedObject)
    val oracleRelocs = relocationRows(readelf.get, oracleObject)
    assertEquals("relocation rows differ", oracleRelocs, generatedRelocs)
  }

  @Test def exportedScalaIntegerFunctionFlowsThroughLoweringToAgbcc(): Unit = {
    val out = Files.createTempDirectory("sn-agbcc-scala-export")
    val nirFiles = scala.scalanative.NIRCompiler
      .getCompiler(out)
      .compile(
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
    val linked = Await.result(
      ScalaNative.link(config, ScalaNative.entries(config)),
      2.minutes
    )
    val optimized =
      Await.result(ScalaNative.optimize(config, linked), 2.minutes)
    val generated =
      Await.result(ScalaNative.codegen(config, optimized), 2.minutes)
    val paths = Await.result(Future.sequence(generated), 2.minutes)
    val cSources =
      paths.filter(_.toString.endsWith(AgbccCodeGen.SourceExtension))
    assertEquals(
      "expected Scala exports to enter the agbcc backend",
      1,
      cSources.size
    )
    val source =
      new String(Files.readAllBytes(cSources.head), StandardCharsets.UTF_8)
    assertTrue(source, source.contains("agbc_scala_add"))
    assertTrue(source, source.contains("agbc_scala_max"))
    assertTrue(source, source.contains("agbc_scala_longadd"))
    assertTrue(source, source.contains("agbc_scala_muladd"))
    assertTrue(source, source.contains("Random(void)"))
    assertTrue(source, source.contains("SeedRng"))
    val agbcc = sys.env.get("AGBCC")
    assumeTrue(
      "set AGBCC to compile the Scala-exported backend output",
      agbcc.isDefined
    )
    val objectFile = compile(
      source,
      "scala-export",
      out,
      agbcc.get,
      sys.env.getOrElse("ARM_AS", "arm-none-eabi-as")
    )
    assertTrue(
      "agbcc did not produce an object for the Scala export",
      Files.size(objectFile) > 0
    )
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
      Files.readAllBytes(
        extractSection(objcopy, oracleObject, "scala-oracle.text")
      ),
      Files.readAllBytes(
        extractSection(objcopy, objectFile, "scala-export.text")
      )
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
      Seq(
        assembler,
        "-mcpu=arm7tdmi",
        "-o",
        objFile.toString,
        asmFile.toString
      ),
      dir.toFile
    ).!
    assertEquals("ARM assembler failed", 0, assembleResult)
    objFile
  }

  private def extractSection(tool: String, obj: Path, out: String): Path = {
    val section = obj.getParent.resolve(out)
    val result = Process(
      Seq(
        tool,
        "--only-section=.text",
        "-O",
        "binary",
        obj.toString,
        section.toString
      )
    ).!
    assertEquals("objcopy failed", 0, result)
    section
  }

  private def relocationRows(tool: String, obj: Path): Seq[String] = {
    val output = new StringBuilder
    val result = Process(Seq(tool, "-rW", obj.toString)).!(
      ProcessLogger(
        line => output.append(line).append('\n'),
        line => output.append(line).append('\n')
      )
    )
    assertEquals("readelf failed", 0, result)
    output.toString.linesIterator
      .map(_.trim)
      .filter(
        _.matches("[0-9a-fA-F]+\\s+[0-9a-fA-F]+\\s+R_ARM_[A-Z0-9_]+\\s+.*")
      )
      .toSeq
  }
}
