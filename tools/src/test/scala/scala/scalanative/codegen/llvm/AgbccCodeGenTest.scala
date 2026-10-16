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

  @Test def emitsNonFiniteFloatingConstantsAsRawBits(): Unit = {
    Seq(
      (nir.Val.Float(Float.NaN), "0x7fc00000u"),
      (nir.Val.Float(Float.PositiveInfinity), "0x7f800000u"),
      (nir.Val.Double(Double.NaN), "0x7ff8000000000000ULL"),
      (nir.Val.Double(Double.NegativeInfinity), "0xfff0000000000000ULL")
    ).zipWithIndex.foreach {
      case ((constant, expectedBits), index) =>
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
        val generated = AgbccCodeGen.emit(Seq(definition))
        assertTrue(
          s"non-finite constant $constant lost its IEEE bits",
          generated.contains(expectedBits)
        )
    }
  }

  @Test def aggregateLoadsAndStoresMatchIndependentAgbccOracle(): Unit = {
    val aggregate = nir.Type.StructValue(Seq(nir.Type.Byte, nir.Type.Int))
    val loadPointer = nir.Val.Local(nir.Local(401), nir.Type.Ptr)
    val load = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_aggr_load")),
      nir.Type.Function(Seq(nir.Type.Ptr), aggregate),
      Seq(
        nir.Inst.Label(nir.Local(400), Seq(loadPointer)),
        nir.Inst.Let(
          nir.Local(402),
          nir.Op.Load(aggregate, loadPointer, None),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(402), aggregate))
      )
    )
    val storePointer = nir.Val.Local(nir.Local(411), nir.Type.Ptr)
    val storeValue = nir.Val.Local(nir.Local(412), aggregate)
    val store = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_aggr_store")),
      nir.Type.Function(Seq(nir.Type.Ptr, aggregate), nir.Type.Unit),
      Seq(
        nir.Inst.Label(nir.Local(410), Seq(storePointer, storeValue)),
        nir.Inst.Let(
          nir.Local(413),
          nir.Op.Store(aggregate, storePointer, storeValue, None),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Unit)
      )
    )
    val generated = AgbccCodeGen.emit(Seq(load, store))
    val oracle =
      """typedef signed char sn_i8;
        |typedef signed int sn_i32;
        |typedef struct { sn_i8 sn_f0; sn_i32 sn_f1; } sn_record;
        |
        |sn_record agbc_match_aggr_load(void * sn_l401) {
        |  sn_record sn_l402;
        |
        |sn_label_400: ;
        |  sn_l402 = (*(sn_record *) (sn_l401));
        |  return sn_l402;
        |}
        |
        |void agbc_match_aggr_store(void * sn_l411, sn_record sn_l412) {
        |
        |sn_label_410: ;
        |  *(sn_record *) (sn_l411) = sn_l412;
        |  return;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for aggregate object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-aggregate-memory-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def indirectCallsMatchIndependentAgbccOracle(): Unit = {
    val callee = nir.Val.Local(nir.Local(501), nir.Type.Ptr)
    val argument = nir.Val.Local(nir.Local(502), nir.Type.Int)
    val definition = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_indirect_call")),
      nir.Type.Function(Seq(nir.Type.Ptr, nir.Type.Int), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(500), Seq(callee, argument)),
        nir.Inst.Let(
          nir.Local(503),
          nir.Op.Call(
            nir.Type.Function(Seq(nir.Type.Int), nir.Type.Int),
            callee,
            Seq(argument)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(503), nir.Type.Int))
      )
    )
    val varargCallee = nir.Val.Local(nir.Local(521), nir.Type.Ptr)
    val fixedArgument = nir.Val.Local(nir.Local(522), nir.Type.Int)
    val extraArgument = nir.Val.Local(nir.Local(523), nir.Type.Int)
    val varargCall = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_indirect_varargs")),
      nir.Type.Function(
        Seq(nir.Type.Ptr, nir.Type.Int, nir.Type.Int),
        nir.Type.Int
      ),
      Seq(
        nir.Inst.Label(
          nir.Local(520),
          Seq(varargCallee, fixedArgument, extraArgument)
        ),
        nir.Inst.Let(
          nir.Local(524),
          nir.Op.Call(
            nir.Type.Function(Seq(nir.Type.Int, nir.Type.Vararg), nir.Type.Int),
            varargCallee,
            Seq(fixedArgument, extraArgument)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(524), nir.Type.Int))
      )
    )
    val varargTarget = owner.member(nir.Sig.Extern("agb_vararg_target"))
    val varargSignature = nir.Type.Function(
      Seq(nir.Type.Int, nir.Type.Vararg),
      nir.Type.Int
    )
    val varargDeclaration = nir.Defn.Declare(
      nir.Attrs.None,
      varargTarget,
      varargSignature
    )
    val directFixed = nir.Val.Local(nir.Local(532), nir.Type.Int)
    val directExtra = nir.Val.Local(nir.Local(533), nir.Type.Int)
    val directVarargCall = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_direct_varargs")),
      nir.Type.Function(Seq(nir.Type.Int, nir.Type.Int), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(531), Seq(directFixed, directExtra)),
        nir.Inst.Let(
          nir.Local(534),
          nir.Op.Call(
            varargSignature,
            nir.Val.Global(varargTarget, nir.Type.Ptr),
            Seq(directFixed, directExtra)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(534), nir.Type.Int))
      )
    )
    val generated = AgbccCodeGen.emit(
      Seq(definition, directVarargCall, varargCall, varargDeclaration)
    )
    val oracle =
      """typedef signed int sn_i32;
        |extern sn_i32 agb_vararg_target(sn_i32, ...);
        |
        |sn_i32 agbc_match_direct_varargs(sn_i32 sn_l532, sn_i32 sn_l533) {
        |  sn_i32 sn_l534;
        |
        |sn_label_531: ;
        |  sn_l534 = agb_vararg_target(sn_l532, sn_l533);
        |  return sn_l534;
        |}
        |
        |sn_i32 agbc_match_indirect_call(void * sn_l501, sn_i32 sn_l502) {
        |  sn_i32 sn_l503;
        |
        |sn_label_500: ;
        |  sn_l503 = ((sn_i32 (*)(sn_i32))(sn_l501))(sn_l502);
        |  return sn_l503;
        |}
        |
        |sn_i32 agbc_match_indirect_varargs(void * sn_l521, sn_i32 sn_l522, sn_i32 sn_l523) {
        |  sn_i32 sn_l524;
        |
        |sn_label_520: ;
        |  sn_l524 = ((sn_i32 (*)(sn_i32, ...))(sn_l521))(sn_l522, sn_l523);
        |  return sn_l524;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for indirect-call parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-indirect-call-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def integerBitwiseShiftsAndComparisonsMatchAgbccOracle(): Unit = {
    def binary(
        name: String,
        bin: nir.Bin,
        id: Int,
        ty: nir.Type = nir.Type.Int
    ): nir.Defn.Define = {
      val left = nir.Val.Local(nir.Local(id + 1), ty)
      val right = nir.Val.Local(nir.Local(id + 2), ty)
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), ty),
        Seq(
          nir.Inst.Label(nir.Local(id), Seq(left, right)),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Bin(bin, ty, left, right),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), ty))
        )
      )
    }
    def comparison(
        name: String,
        comp: nir.Comp,
        id: Int,
        ty: nir.Type = nir.Type.Int
    ): nir.Defn.Define = {
      val left = nir.Val.Local(nir.Local(id + 1), ty)
      val right = nir.Val.Local(nir.Local(id + 2), ty)
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), nir.Type.Bool),
        Seq(
          nir.Inst.Label(nir.Local(id), Seq(left, right)),
          nir.Inst.Let(
            nir.Local(id + 3),
            nir.Op.Comp(comp, ty, left, right),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(id + 3), nir.Type.Bool))
        )
      )
    }
    val definitions = Seq(
      comparison("agbc_match_a_cmp_sgt", nir.Comp.Sgt, 600),
      comparison("agbc_match_a_cmp_ule", nir.Comp.Ule, 610),
      binary("agbc_match_bit_and", nir.Bin.And, 620),
      binary("agbc_match_bit_or", nir.Bin.Or, 630),
      binary("agbc_match_bit_xor", nir.Bin.Xor, 640),
      binary("agbc_match_shift_ashr", nir.Bin.Ashr, 650),
      binary("agbc_match_shift_lshr", nir.Bin.Lshr, 660),
      binary("agbc_match_shift_shl", nir.Bin.Shl, 670),
      binary("agbc_match_byte_lshr", nir.Bin.Lshr, 680, nir.Type.Byte),
      binary("agbc_match_short_lshr", nir.Bin.Lshr, 690, nir.Type.Short),
      binary("agbc_match_char_ashr", nir.Bin.Ashr, 700, nir.Type.Char),
      binary("agbc_match_size_ashr", nir.Bin.Ashr, 710, nir.Type.Size),
      binary("agbc_match_char_sdiv", nir.Bin.Sdiv, 720, nir.Type.Char),
      binary("agbc_match_char_srem", nir.Bin.Srem, 730, nir.Type.Char),
      comparison("agbc_match_char_sgt", nir.Comp.Sgt, 740, nir.Type.Char),
      comparison("agbc_match_size_slt", nir.Comp.Slt, 750, nir.Type.Size),
      comparison("agbc_match_byte_ugt", nir.Comp.Ugt, 760, nir.Type.Byte)
    )
    val generated = AgbccCodeGen.emit(definitions)
    val oracle =
      """typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef unsigned char sn_u8;
        |typedef signed char sn_i8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |
        |sn_u8 agbc_match_a_cmp_sgt(sn_i32 sn_l601, sn_i32 sn_l602) {
        |  sn_u8 sn_l603;
        |
        |sn_label_600: ;
        |  sn_l603 = ((sn_u8)(((sn_i32)(sn_l601)) > ((sn_i32)(sn_l602))));
        |  return sn_l603;
        |}
        |
        |sn_u8 agbc_match_a_cmp_ule(sn_i32 sn_l611, sn_i32 sn_l612) {
        |  sn_u8 sn_l613;
        |
        |sn_label_610: ;
        |  sn_l613 = ((sn_u8)(((sn_u32)(sn_l611)) <= ((sn_u32)(sn_l612))));
        |  return sn_l613;
        |}
        |
        |sn_i32 agbc_match_bit_and(sn_i32 sn_l621, sn_i32 sn_l622) {
        |  sn_i32 sn_l623;
        |
        |sn_label_620: ;
        |  sn_l623 = (sn_l621 & sn_l622);
        |  return sn_l623;
        |}
        |
        |sn_i32 agbc_match_bit_or(sn_i32 sn_l631, sn_i32 sn_l632) {
        |  sn_i32 sn_l633;
        |
        |sn_label_630: ;
        |  sn_l633 = (sn_l631 | sn_l632);
        |  return sn_l633;
        |}
        |
        |sn_i32 agbc_match_bit_xor(sn_i32 sn_l641, sn_i32 sn_l642) {
        |  sn_i32 sn_l643;
        |
        |sn_label_640: ;
        |  sn_l643 = (sn_l641 ^ sn_l642);
        |  return sn_l643;
        |}
        |
        |sn_i8 agbc_match_byte_lshr(sn_i8 sn_l681, sn_i8 sn_l682) {
        |  sn_i8 sn_l683;
        |
        |sn_label_680: ;
        |  sn_l683 = ((sn_u32)(sn_u8)(sn_l681) >> (sn_l682));
        |  return sn_l683;
        |}
        |
        |sn_u8 agbc_match_byte_ugt(sn_i8 sn_l761, sn_i8 sn_l762) {
        |  sn_u8 sn_l763;
        |
        |sn_label_760: ;
        |  sn_l763 = ((sn_u8)(((sn_u32)(sn_u8)(sn_l761)) > ((sn_u32)(sn_u8)(sn_l762))));
        |  return sn_l763;
        |}
        |
        |sn_u16 agbc_match_char_ashr(sn_u16 sn_l701, sn_u16 sn_l702) {
        |  sn_u16 sn_l703;
        |
        |sn_label_700: ;
        |  sn_l703 = ((sn_i32)((sn_i16)(sn_l701)) >> (sn_l702));
        |  return sn_l703;
        |}
        |
        |sn_u16 agbc_match_char_sdiv(sn_u16 sn_l721, sn_u16 sn_l722) {
        |  sn_u16 sn_l723;
        |
        |sn_label_720: ;
        |  sn_l723 = ((sn_i32)((sn_i16)(sn_l721)) / (sn_i32)((sn_i16)(sn_l722)));
        |  return sn_l723;
        |}
        |
        |sn_u8 agbc_match_char_sgt(sn_u16 sn_l741, sn_u16 sn_l742) {
        |  sn_u8 sn_l743;
        |
        |sn_label_740: ;
        |  sn_l743 = ((sn_u8)(((sn_i32)((sn_i16)(sn_l741))) > ((sn_i32)((sn_i16)(sn_l742)))));
        |  return sn_l743;
        |}
        |
        |sn_u16 agbc_match_char_srem(sn_u16 sn_l731, sn_u16 sn_l732) {
        |  sn_u16 sn_l733;
        |
        |sn_label_730: ;
        |  sn_l733 = ((sn_i32)((sn_i16)(sn_l731)) % (sn_i32)((sn_i16)(sn_l732)));
        |  return sn_l733;
        |}
        |
        |sn_i32 agbc_match_shift_ashr(sn_i32 sn_l651, sn_i32 sn_l652) {
        |  sn_i32 sn_l653;
        |
        |sn_label_650: ;
        |  sn_l653 = ((sn_i32)(sn_l651) >> (sn_l652));
        |  return sn_l653;
        |}
        |
        |sn_i32 agbc_match_shift_lshr(sn_i32 sn_l661, sn_i32 sn_l662) {
        |  sn_i32 sn_l663;
        |
        |sn_label_660: ;
        |  sn_l663 = ((sn_u32)(sn_l661) >> (sn_l662));
        |  return sn_l663;
        |}
        |
        |sn_i32 agbc_match_shift_shl(sn_i32 sn_l671, sn_i32 sn_l672) {
        |  sn_i32 sn_l673;
        |
        |sn_label_670: ;
        |  sn_l673 = ((sn_u32)(sn_l671) << (sn_l672));
        |  return sn_l673;
        |}
        |
        |sn_i16 agbc_match_short_lshr(sn_i16 sn_l691, sn_i16 sn_l692) {
        |  sn_i16 sn_l693;
        |
        |sn_label_690: ;
        |  sn_l693 = ((sn_u32)(sn_u16)(sn_l691) >> (sn_l692));
        |  return sn_l693;
        |}
        |
        |sn_u32 agbc_match_size_ashr(sn_u32 sn_l711, sn_u32 sn_l712) {
        |  sn_u32 sn_l713;
        |
        |sn_label_710: ;
        |  sn_l713 = ((sn_i32)(sn_l711) >> (sn_l712));
        |  return sn_l713;
        |}
        |
        |sn_u8 agbc_match_size_slt(sn_u32 sn_l751, sn_u32 sn_l752) {
        |  sn_u8 sn_l753;
        |
        |sn_label_750: ;
        |  sn_l753 = ((sn_u8)(((sn_i32)(sn_l751)) < ((sn_i32)(sn_l752))));
        |  return sn_l753;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for integer object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-integer-ops-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )

    val harness =
      """int main(void) {
        |  if (agbc_match_byte_lshr((sn_i8)-1, 1) != 127) return 1;
        |  if (agbc_match_short_lshr((sn_i16)-1, 1) != 32767) return 2;
        |  if (agbc_match_char_ashr(0x8000u, 1) != 0xc000u) return 3;
        |  if (agbc_match_size_ashr(0x80000000u, 1) != 0xc0000000u) return 4;
        |  if (agbc_match_char_sdiv(0xfffeu, 2) != 0xffffu) return 5;
        |  if (agbc_match_char_srem(0xffffu, 2) != 0xffffu) return 6;
        |  if (agbc_match_char_sgt(0xffffu, 0) != 0) return 7;
        |  if (agbc_match_size_slt(0xffffffffu, 0) != 1) return 8;
        |  if (agbc_match_byte_ugt((sn_i8)-1, 1) != 1) return 9;
        |  return 0;
        |}
        |""".stripMargin
    val hostSource = dir.resolve("integer-width-harness.c")
    val executable = dir.resolve("integer-width-harness")
    Files.write(
      hostSource,
      (generated + "\n" + harness).getBytes(StandardCharsets.UTF_8)
    )
    val cCompiler = sys.env.getOrElse("CC", "cc")
    assertEquals(
      "host C compiler failed",
      0,
      Process(
        Seq(
          cCompiler,
          "-std=c99",
          "-O2",
          hostSource.toString,
          "-o",
          executable.toString
        ),
        dir.toFile
      ).!
    )
    assertEquals(
      "integer width/sign semantics are incorrect",
      0,
      Process(Seq(executable.toString), dir.toFile).!
    )
  }

  @Test def switchControlFlowMatchesIndependentAgbccOracle(): Unit = {
    val input = nir.Val.Local(nir.Local(701), nir.Type.Int)
    val definition = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_switch")),
      nir.Type.Function(Seq(nir.Type.Int), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(700), Seq(input)),
        nir.Inst.Switch(
          input,
          nir.Next.Label(nir.Local(704), Seq.empty),
          Seq(
            nir.Next.Case(
              nir.Val.Int(0),
              nir.Next.Label(nir.Local(702), Seq.empty)
            ),
            nir.Next.Case(
              nir.Val.Int(1),
              nir.Next.Label(nir.Local(703), Seq.empty)
            )
          )
        ),
        nir.Inst.Label(nir.Local(702), Seq.empty),
        nir.Inst.Ret(nir.Val.Int(10)),
        nir.Inst.Label(nir.Local(703), Seq.empty),
        nir.Inst.Ret(nir.Val.Int(20)),
        nir.Inst.Label(nir.Local(704), Seq.empty),
        nir.Inst.Ret(nir.Val.Int(30))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val oracle =
      """typedef signed int sn_i32;
        |
        |sn_i32 agbc_match_switch(sn_i32 sn_l701) {
        |
        |sn_label_700: ;
        |  switch (sn_l701) {
        |    case 0: goto sn_label_702;
        |    case 1: goto sn_label_703;
        |    default: goto sn_label_704;
        |  }
        |sn_label_702: ;
        |  return 10;
        |sn_label_703: ;
        |  return 20;
        |sn_label_704: ;
        |  return 30;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for switch object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-switch-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def floatingBitcastsMatchIndependentAgbccOracle(): Unit = {
    def bitcast(
        name: String,
        from: nir.Type,
        to: nir.Type,
        entryId: Int
    ): nir.Defn.Define = {
      val input = nir.Val.Local(nir.Local(entryId + 1), from)
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(from), to),
        Seq(
          nir.Inst.Label(nir.Local(entryId), Seq(input)),
          nir.Inst.Let(
            nir.Local(entryId + 2),
            nir.Op.Conv(nir.Conv.Bitcast, to, input),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(entryId + 2), to))
        )
      )
    }

    val definitions = Seq(
      bitcast("agbc_match_f32_to_i32", nir.Type.Float, nir.Type.Int, 1100),
      bitcast("agbc_match_f32_to_u32", nir.Type.Float, nir.Type.Size, 1140),
      bitcast("agbc_match_i32_to_f32", nir.Type.Int, nir.Type.Float, 1110),
      bitcast("agbc_match_u32_to_f32", nir.Type.Size, nir.Type.Float, 1150),
      bitcast("agbc_match_f64_to_i64", nir.Type.Double, nir.Type.Long, 1120),
      bitcast("agbc_match_i64_to_f64", nir.Type.Long, nir.Type.Double, 1130)
    )
    val generated = AgbccCodeGen.emit(definitions)
    val oracle =
      """typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef signed long long sn_i64;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |
        |sn_i32 agbc_match_f32_to_i32(sn_f32 value) {
        |  union { sn_f32 from; sn_i32 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}
        |sn_u32 agbc_match_f32_to_u32(sn_f32 value) {
        |  union { sn_f32 from; sn_u32 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}
        |sn_i64 agbc_match_f64_to_i64(sn_f64 value) {
        |  union { sn_f64 from; sn_i64 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}
        |sn_f32 agbc_match_i32_to_f32(sn_i32 value) {
        |  union { sn_i32 from; sn_f32 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}
        |sn_f64 agbc_match_i64_to_f64(sn_i64 value) {
        |  union { sn_i64 from; sn_f64 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}
        |sn_f32 agbc_match_u32_to_f32(sn_u32 value) {
        |  union { sn_u32 from; sn_f32 to; } bits;
        |  bits.from = value;
        |  return bits.to;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for floating bitcast object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-bitcast-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def memoryFenceMatchesIndependentAgbccOracle(): Unit = {
    val definition = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_fence")),
      nir.Type.Function(Seq.empty, nir.Type.Unit),
      Seq(
        nir.Inst.Label(nir.Local(1200), Seq.empty),
        nir.Inst.Let(
          nir.Local(1201),
          nir.Op.Fence(nir.MemoryOrder.SeqCst),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Unit)
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val oracle =
      """void agbc_match_fence(void) {
        |sn_label_1200: ;
        |  __asm__ volatile("" ::: "memory");
        |  return;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for memory fence object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-fence-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def atomicScalarAccessesMatchVolatileAgbccOracle(): Unit = {
    val pointer = nir.Val.Local(nir.Local(1301), nir.Type.Ptr)
    val stored = nir.Val.Local(nir.Local(1302), nir.Type.Int)
    val loaded = nir.Local(1303)
    val definition = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_atomic_access")),
      nir.Type.Function(Seq(nir.Type.Ptr, nir.Type.Int), nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(1300), Seq(pointer, stored)),
        nir.Inst.Let(
          loaded,
          nir.Op.Load(nir.Type.Int, pointer, Some(nir.MemoryOrder.SeqCst)),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(1304),
          nir.Op.Store(
            nir.Type.Int,
            pointer,
            stored,
            Some(nir.MemoryOrder.Release)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(loaded, nir.Type.Int))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val oracle =
      """typedef signed int sn_i32;
        |sn_i32 agbc_match_atomic_access(void * ptr, sn_i32 stored) {
        |  sn_i32 loaded = *(volatile sn_i32 *)(ptr);
        |  *(volatile sn_i32 *)(ptr) = stored;
        |  return loaded;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for atomic access object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-atomic-access-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def floatingRemainderUsesSelfContainedAgbccHelper(): Unit = {
    def remainder(
        name: String,
        ty: nir.Type,
        entryId: Int
    ): nir.Defn.Define = {
      val args = Seq(
        nir.Val.Local(nir.Local(entryId + 1), ty),
        nir.Val.Local(nir.Local(entryId + 2), ty)
      )
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq(ty, ty), ty),
        Seq(
          nir.Inst.Label(nir.Local(entryId), args),
          nir.Inst.Let(
            nir.Local(entryId + 3),
            nir.Op.Bin(nir.Bin.Frem, ty, args(0), args(1)),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Local(nir.Local(entryId + 3), ty))
        )
      )
    }
    val definitions = Seq(
      remainder("agbc_match_frem", nir.Type.Float, 1400),
      remainder("agbc_match_drem", nir.Type.Double, 1410)
    )
    val generated = AgbccCodeGen.emit(definitions)
    val oracle =
      """typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |""".stripMargin + AgbccCodeGen.FloatingRemainderSupport +
        """
          |sn_f64 agbc_match_drem(sn_f64 sn_l1411, sn_f64 sn_l1412) {
          |  sn_f64 sn_l1413;
          |
          |sn_label_1410: ;
          |  sn_l1413 = sn_agbcc_fmod64(sn_l1411, sn_l1412);
          |  return sn_l1413;
          |}
          |sn_f32 agbc_match_frem(sn_f32 sn_l1401, sn_f32 sn_l1402) {
          |  sn_f32 sn_l1403;
          |
          |sn_label_1400: ;
          |  sn_l1403 = sn_agbcc_fmod32(sn_l1401, sn_l1402);
          |  return sn_l1403;
          |}""".stripMargin
    assertFalse(generated.contains("fmodf("))
    assertFalse(generated.contains("= fmod("))
    assertTrue(generated.contains("static sn_f64 sn_agbcc_fmod("))
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for floating remainder object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-frem-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject =
      compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    val generatedText =
      extractSection(objcopy.get, generatedObject, "generated.text")
    val oracleText = extractSection(objcopy.get, oracleObject, "oracle.text")
    assertArrayEquals(
      ".text bytes differ",
      Files.readAllBytes(oracleText),
      Files.readAllBytes(generatedText)
    )
    assertEquals(
      "relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
    assertFalse(
      "generated object must not depend on a missing libm fmod symbol",
      relocationRows(readelf.get, generatedObject).exists(_.contains("fmod"))
    )

    val semanticHarness =
      """#include <math.h>
        |#include <stdio.h>
        |typedef union { double value; unsigned long long bits; } sn_test_f64;
        |typedef union { float value; unsigned int bits; } sn_test_f32;
        |int main(void) {
        |  static const double values[] = {
        |    5.3, -5.3, 1.0e300, -1.0e300, 1.0e-300, -1.0e-300,
        |    2.2250738585072014e-308, 1.7976931348623157e308,
        |    0x0.0000000000001p-1022, 0x0.fffffffffffffp-1022, -0.0
        |  };
        |  static const double divisors[] = { 2.0, 2.0, 3.0, 7.0, 3.0,
        |    7.0, 1.0e-308, 1.0e292, 0x0.0000000000002p-1022,
        |    0x0.0000000000001p-1022, 1.0 };
        |  sn_test_f64 actual64, expected64;
        |  sn_test_f32 actual32, expected32;
        |  sn_test_f64 x64, y64;
        |  sn_test_f32 x32, y32;
        |  unsigned long long state = 0x6a09e667f3bcc909ULL;
        |  int i;
        |  for (i = 0; i < 11; ++i) {
        |    actual64.value = agbc_match_drem(values[i], divisors[i]);
        |    expected64.value = fmod(values[i], divisors[i]);
        |    if (actual64.bits != expected64.bits) return 10 + i;
        |  }
        |  actual64.value = agbc_match_drem(-4.0, 2.0);
        |  expected64.value = fmod(-4.0, 2.0);
        |  if (actual64.bits != expected64.bits) return 20;
        |  actual64.value = agbc_match_drem(1.0, INFINITY);
        |  if (actual64.value != 1.0) return 21;
        |  actual64.value = agbc_match_drem(1.0, 0.0);
        |  if (!isnan(actual64.value)) return 22;
        |  actual32.value = agbc_match_frem(-5.3f, 2.0f);
        |  expected32.value = fmodf(-5.3f, 2.0f);
        |  if (actual32.bits != expected32.bits) return 23;
        |  actual32.value = agbc_match_frem(-0.0f, 1.0f);
        |  expected32.value = fmodf(-0.0f, 1.0f);
        |  if (actual32.bits != expected32.bits) return 25;
        |  actual32.value = agbc_match_frem(1.0f, 0.0f);
        |  if (!isnan(actual32.value)) return 24;
        |  for (i = 0; i < 4096; ++i) {
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    x64.bits = state;
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    x64.bits = (x64.bits & 0x800fffffffffffffULL) |
        |      ((state % 2047ULL) << 52);
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    y64.bits = state;
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    y64.bits = (y64.bits & 0x800fffffffffffffULL) |
        |      ((state % 2047ULL) << 52);
        |    if ((y64.bits & 0x7fffffffffffffffULL) == 0) y64.bits = 1;
        |    actual64.value = agbc_match_drem(x64.value, y64.value);
        |    expected64.value = fmod(x64.value, y64.value);
        |    if (actual64.bits != expected64.bits) return 30;
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    x32.bits = (unsigned int)state;
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    x32.bits = (x32.bits & 0x807fffffu) |
        |      ((unsigned int)(state % 255ULL) << 23);
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    y32.bits = (unsigned int)state;
        |    state = state * 6364136223846793005ULL + 1442695040888963407ULL;
        |    y32.bits = (y32.bits & 0x807fffffu) |
        |      ((unsigned int)(state % 255ULL) << 23);
        |    if ((y32.bits & 0x7fffffffu) == 0) y32.bits = 1;
        |    actual32.value = agbc_match_frem(x32.value, y32.value);
        |    expected32.value = fmodf(x32.value, y32.value);
        |    if (actual32.bits != expected32.bits) return 31;
        |  }
        |  puts("fmod helper semantic checks passed");
        |  return 0;
        |}
        |""".stripMargin
    val harness = dir.resolve("fmod-harness.c")
    val executable = dir.resolve("fmod-harness")
    Files.write(
      harness,
      (generated + "\n" + semanticHarness).getBytes(StandardCharsets.UTF_8)
    )
    val cCompiler = sys.env.getOrElse("CC", "cc")
    assertEquals(
      "host C compiler failed",
      0,
      Process(
        Seq(
          cCompiler,
          "-std=c99",
          "-O2",
          harness.toString,
          "-lm",
          "-o",
          executable.toString
        ),
        dir.toFile
      ).!
    )
    assertEquals(
      "fmod helper disagrees with the host C library",
      0,
      Process(Seq(executable.toString), dir.toFile).!
    )
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

  @Test def integerExtensionsPreserveNirSignedness(): Unit = {
    val cases = Seq(
      (
        "agbc_conv_zext_byte_int",
        nir.Type.Byte,
        nir.Type.Int,
        nir.Conv.Zext,
        1700,
        "sn_i8",
        "sn_i32",
        "sn_u8"
      ),
      (
        "agbc_conv_zext_short_long",
        nir.Type.Short,
        nir.Type.Long,
        nir.Conv.Zext,
        1710,
        "sn_i16",
        "sn_i64",
        "sn_u16"
      ),
      (
        "agbc_conv_sext_byte_int",
        nir.Type.Byte,
        nir.Type.Int,
        nir.Conv.Sext,
        1720,
        "sn_i8",
        "sn_i32",
        "sn_i8"
      ),
      (
        "agbc_conv_sext_char_int",
        nir.Type.Char,
        nir.Type.Int,
        nir.Conv.Sext,
        1730,
        "sn_u16",
        "sn_i32",
        "sn_i16"
      ),
      (
        "agbc_conv_zsize_byte_size",
        nir.Type.Byte,
        nir.Type.Size,
        nir.Conv.ZSizeCast,
        1740,
        "sn_i8",
        "sn_u32",
        "sn_u8"
      ),
      (
        "agbc_conv_ssize_byte_size",
        nir.Type.Byte,
        nir.Type.Size,
        nir.Conv.SSizeCast,
        1750,
        "sn_i8",
        "sn_u32",
        "sn_i8"
      ),
      (
        "agbc_conv_zsize_size_long",
        nir.Type.Size,
        nir.Type.Long,
        nir.Conv.ZSizeCast,
        1760,
        "sn_u32",
        "sn_i64",
        "sn_u32"
      ),
      (
        "agbc_conv_ssize_size_long",
        nir.Type.Size,
        nir.Type.Long,
        nir.Conv.SSizeCast,
        1770,
        "sn_u32",
        "sn_i64",
        "sn_i32"
      ),
      (
        "agbc_conv_zext_int_long",
        nir.Type.Int,
        nir.Type.Long,
        nir.Conv.Zext,
        1780,
        "sn_i32",
        "sn_i64",
        "sn_u32"
      ),
      (
        "agbc_conv_sext_int_long",
        nir.Type.Int,
        nir.Type.Long,
        nir.Conv.Sext,
        1790,
        "sn_i32",
        "sn_i64",
        "sn_i32"
      )
    )
    val definitions = cases.map {
      case (name, fromTy, toTy, conv, id, _, _, _) =>
        val arg = nir.Val.Local(nir.Local(id + 1), fromTy)
        nir.Defn.Define(
          nir.Attrs.None,
          owner.member(nir.Sig.Extern(name)),
          nir.Type.Function(Seq(fromTy), toTy),
          Seq(
            nir.Inst.Label(nir.Local(id), Seq(arg)),
            nir.Inst.Let(
              nir.Local(id + 2),
              nir.Op.Conv(conv, toTy, arg),
              nir.Next.None
            ),
            nir.Inst.Ret(nir.Val.Local(nir.Local(id + 2), toTy))
          )
        )
    }
    val generated = AgbccCodeGen.emit(definitions)
    val oracleFunctions = cases.sortBy(_._1).map {
      case (name, _, _, _, id, fromC, toC, sourceCastC) =>
        val source = s"sn_l${id + 1}"
        val result = s"sn_l${id + 2}"
        s"""$toC $name($fromC $source) {
           |  $toC $result;
           |
           |sn_label_$id: ;
           |  $result = (($toC)(($sourceCastC)($source)));
           |  return $result;
           |}""".stripMargin
    }
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |""".stripMargin + oracleFunctions.mkString("\n\n")

    assertTrue(
      generated.contains("((sn_i32)((sn_u8)(sn_l1701)))")
    )
    assertTrue(
      generated.contains("((sn_i32)((sn_i16)(sn_l1731)))")
    )
    assertTrue(
      generated.contains("((sn_i64)((sn_i32)(sn_l1771)))")
    )
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for integer extension object parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-integer-extension-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    assertArrayEquals(
      "integer conversion .text bytes differ",
      Files.readAllBytes(
        extractSection(objcopy.get, oracleObject, "oracle.text")
      ),
      Files.readAllBytes(
        extractSection(objcopy.get, generatedObject, "generated.text")
      )
    )
    assertEquals(
      "integer conversion relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )

    val harness =
      """int main(void) {
        |  if (agbc_conv_zext_byte_int((sn_i8)-1) != 255) return 1;
        |  if (agbc_conv_zext_short_long((sn_i16)-1) != 65535) return 2;
        |  if (agbc_conv_sext_byte_int((sn_i8)0xff) != -1) return 3;
        |  if (agbc_conv_sext_char_int((sn_u16)0xffff) != -1) return 4;
        |  if (agbc_conv_zsize_byte_size((sn_i8)-1) != 255u) return 5;
        |  if (agbc_conv_ssize_byte_size((sn_i8)-1) != 0xffffffffu) return 6;
        |  if (agbc_conv_zsize_size_long(0xffffffffu) != 4294967295LL) return 7;
        |  if (agbc_conv_ssize_size_long(0xffffffffu) != -1) return 8;
        |  if (agbc_conv_zext_int_long(-1) != 4294967295LL) return 9;
        |  if (agbc_conv_sext_int_long(-1) != -1) return 10;
        |  return 0;
        |}
        |""".stripMargin
    val hostSource = dir.resolve("conversion-harness.c")
    val executable = dir.resolve("conversion-harness")
    Files.write(
      hostSource,
      (generated + "\n" + harness).getBytes(StandardCharsets.UTF_8)
    )
    val cCompiler = sys.env.getOrElse("CC", "cc")
    assertEquals(
      "host C compiler failed",
      0,
      Process(
        Seq(
          cCompiler,
          "-std=c99",
          "-O2",
          hostSource.toString,
          "-o",
          executable.toString
        ),
        dir.toFile
      ).!
    )
    assertEquals(
      "integer extension helper semantics are incorrect",
      0,
      Process(Seq(executable.toString), dir.toFile).!
    )
  }

  @Test def numericConversionsPreserveOpcodeSignedness(): Unit = {
    val cases = Seq(
      (
        "agbc_uitofp_byte",
        nir.Type.Byte,
        nir.Type.Float,
        nir.Conv.Uitofp,
        1800
      ),
      (
        "agbc_sitofp_size",
        nir.Type.Size,
        nir.Type.Float,
        nir.Conv.Sitofp,
        1810
      ),
      ("agbc_fptoui_int", nir.Type.Float, nir.Type.Int, nir.Conv.Fptoui, 1820),
      (
        "agbc_fptosi_size",
        nir.Type.Double,
        nir.Type.Size,
        nir.Conv.Fptosi,
        1830
      )
    )
    val definitions = cases.map {
      case (name, from, to, conv, id) =>
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
    val generated = AgbccCodeGen.emit(definitions)
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |
        |sn_u32 agbc_fptosi_size(sn_f64 sn_l1831) {
        |  sn_u32 sn_l1832;
        |sn_label_1830: ;
        |  sn_l1832 = ((sn_u32)((sn_i32)(sn_l1831)));
        |  return sn_l1832;
        |}
        |
        |sn_i32 agbc_fptoui_int(sn_f32 sn_l1821) {
        |  sn_i32 sn_l1822;
        |sn_label_1820: ;
        |  sn_l1822 = ((sn_i32)((sn_u32)(sn_l1821)));
        |  return sn_l1822;
        |}
        |
        |sn_f32 agbc_sitofp_size(sn_u32 sn_l1811) {
        |  sn_f32 sn_l1812;
        |sn_label_1810: ;
        |  sn_l1812 = ((sn_f32)((sn_i32)(sn_l1811)));
        |  return sn_l1812;
        |}
        |
        |sn_f32 agbc_uitofp_byte(sn_i8 sn_l1801) {
        |  sn_f32 sn_l1802;
        |sn_label_1800: ;
        |  sn_l1802 = ((sn_f32)((sn_u8)(sn_l1801)));
        |  return sn_l1802;
        |}""".stripMargin
    val dir = Files.createTempDirectory("sn-agbcc-numeric-conversion-test")
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for numeric conversion parity",
      agbcc.isDefined
    )

    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    assertArrayEquals(
      "numeric conversion .text bytes differ",
      Files.readAllBytes(
        extractSection(objcopy.get, oracleObject, "oracle.text")
      ),
      Files.readAllBytes(
        extractSection(objcopy.get, generatedObject, "generated.text")
      )
    )
    assertEquals(
      "numeric conversion relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )

    val harness =
      """int main(void) {
        |  if (agbc_uitofp_byte((sn_i8)-1) != 255.0f) return 1;
        |  if (agbc_sitofp_size(0xffffffffu) != -1.0f) return 2;
        |  if ((sn_u32)agbc_fptoui_int(2147483648.0f) != 0x80000000u) return 3;
        |  if (agbc_fptosi_size(-1.0) != 0xffffffffu) return 4;
        |  return 0;
        |}""".stripMargin
    val hostSource = dir.resolve("numeric-conversion-harness.c")
    val executable = dir.resolve("numeric-conversion-harness")
    Files.write(
      hostSource,
      (generated + "\n" + harness).getBytes(StandardCharsets.UTF_8)
    )
    val cCompiler = sys.env.getOrElse("CC", "cc")
    assertEquals(
      "host C compiler failed",
      0,
      Process(
        Seq(
          cCompiler,
          "-std=c99",
          "-O2",
          hostSource.toString,
          "-o",
          executable.toString
        ),
        dir.toFile
      ).!
    )
    assertEquals(
      "numeric conversion signedness is incorrect",
      0,
      Process(Seq(executable.toString), dir.toFile).!
    )
  }

  @Test def unreachableEmitsAnInPlaceThumbLoop(): Unit = {
    val condition = nir.Val.Local(nir.Local(1901), nir.Type.Bool)
    val definition = function(
      "agbc_match_unreachable",
      Seq(nir.Type.Bool),
      Seq(
        nir.Inst.Label(nir.Local(1900), Seq(condition)),
        nir.Inst.If(
          condition,
          nir.Next.Label(nir.Local(1902), Seq.empty),
          nir.Next.Label(nir.Local(1903), Seq.empty)
        ),
        nir.Inst.Label(nir.Local(1902), Seq.empty),
        nir.Inst.Ret(nir.Val.Int(1)),
        nir.Inst.Label(nir.Local(1903), Seq.empty),
        nir.Inst.Unreachable(nir.Next.None)
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |
        |sn_i32 agbc_match_unreachable(sn_u8 sn_l1901) {
        |sn_label_1900: ;
        |  if (sn_l1901) { goto sn_label_1902; } else { goto sn_label_1903; }
        |sn_label_1902: ;
        |  return 1;
        |sn_label_1903: ;
        |  __asm__ volatile("1: b 1b");
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for unreachable parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-unreachable-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    assertArrayEquals(
      "unreachable .text bytes differ",
      Files.readAllBytes(
        extractSection(objcopy.get, oracleObject, "oracle.text")
      ),
      Files.readAllBytes(
        extractSection(objcopy.get, generatedObject, "generated.text")
      )
    )
    assertEquals(
      "unreachable relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )
  }

  @Test def nonFiniteFloatConstantsPreserveRawBits(): Unit = {
    def constant(
        name: String,
        ty: nir.Type,
        value: nir.Val,
        id: Int
    ): nir.Defn.Define =
      nir.Defn.Define(
        nir.Attrs.None,
        owner.member(nir.Sig.Extern(name)),
        nir.Type.Function(Seq.empty, ty),
        Seq(
          nir.Inst.Label(nir.Local(id), Seq.empty),
          nir.Inst.Ret(value)
        )
      )

    val definitions = Seq(
      constant(
        "agbc_const_f32_nan",
        nir.Type.Float,
        nir.Val.Float(java.lang.Float.intBitsToFloat(0x7fc12345)),
        2000
      ),
      constant(
        "agbc_const_f64_neg_inf",
        nir.Type.Double,
        nir.Val.Double(Double.NegativeInfinity),
        2010
      )
    )
    val generated = AgbccCodeGen.emit(definitions)
    val oracle =
      """typedef signed char sn_i8;
        |typedef unsigned char sn_u8;
        |typedef signed short sn_i16;
        |typedef unsigned short sn_u16;
        |typedef signed int sn_i32;
        |typedef unsigned int sn_u32;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |
        |sn_f32 agbc_const_f32_nan(void) {
        |sn_label_2000: ;
        |  return ((union { sn_u32 bits; sn_f32 value; }){ .bits = 0x7fc12345u }).value;
        |}
        |
        |sn_f64 agbc_const_f64_neg_inf(void) {
        |sn_label_2010: ;
        |  return ((union { sn_u64 bits; sn_f64 value; }){ .bits = 0xfff0000000000000ULL }).value;
        |}""".stripMargin
    val agbcc = sys.env.get("AGBCC")
    val assembler = sys.env.get("ARM_AS").orElse(Some("arm-none-eabi-as"))
    val objcopy =
      sys.env.get("ARM_OBJCOPY").orElse(Some("arm-none-eabi-objcopy"))
    val readelf =
      sys.env.get("ARM_READELF").orElse(Some("arm-none-eabi-readelf"))
    assumeTrue(
      "set AGBCC and ARM binutils for non-finite float parity",
      agbcc.isDefined
    )

    val dir = Files.createTempDirectory("sn-agbcc-non-finite-float-test")
    val generatedObject =
      compile(generated, "generated", dir, agbcc.get, assembler.get)
    val oracleObject = compile(oracle, "oracle", dir, agbcc.get, assembler.get)
    assertArrayEquals(
      "non-finite constant .text bytes differ",
      Files.readAllBytes(
        extractSection(objcopy.get, oracleObject, "oracle.text")
      ),
      Files.readAllBytes(
        extractSection(objcopy.get, generatedObject, "generated.text")
      )
    )
    assertEquals(
      "non-finite constant relocation rows differ",
      relocationRows(readelf.get, oracleObject),
      relocationRows(readelf.get, generatedObject)
    )

    val harness =
      """int main(void) {
        |  union { sn_f32 value; sn_u32 bits; } f32;
        |  union { sn_f64 value; sn_u64 bits; } f64;
        |  f32.value = agbc_const_f32_nan();
        |  f64.value = agbc_const_f64_neg_inf();
        |  if (f32.bits != 0x7fc12345u) return 1;
        |  if (f64.bits != 0xfff0000000000000ULL) return 2;
        |  return 0;
        |}""".stripMargin
    val hostSource = dir.resolve("non-finite-float-harness.c")
    val executable = dir.resolve("non-finite-float-harness")
    Files.write(
      hostSource,
      (generated + "\n" + harness).getBytes(StandardCharsets.UTF_8)
    )
    val cCompiler = sys.env.getOrElse("CC", "cc")
    assertEquals(
      "host C compiler failed",
      0,
      Process(
        Seq(
          cCompiler,
          "-std=c99",
          "-O2",
          hostSource.toString,
          "-o",
          executable.toString
        ),
        dir.toFile
      ).!
    )
    assertEquals(
      "non-finite constant bits changed",
      0,
      Process(Seq(executable.toString), dir.toFile).!
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
    val pairType = nir.Type.StructValue(Seq(nir.Type.Byte, nir.Type.Int))
    val pairsGlobal = owner.member(nir.Sig.Extern("agbc_match_pairs"))
    val pairsData = nir.Defn.Var(
      nir.Attrs.None,
      pairsGlobal,
      pairType,
      nir.Val.Zero(pairType)
    )
    val globalIndex = nir.Val.Local(nir.Local(86), nir.Type.Int)
    val globalElement = nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("agbc_match_elem_global")),
      nir.Type.Function(Seq(nir.Type.Int), nir.Type.Ptr),
      Seq(
        nir.Inst.Label(nir.Local(85), Seq(globalIndex)),
        nir.Inst.Let(
          nir.Local(87),
          nir.Op.Elem(
            pairType,
            nir.Val.Global(pairsGlobal, nir.Type.Ptr),
            Seq(globalIndex, nir.Val.Int(1))
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(87), nir.Type.Ptr))
      )
    )
    val generated = AgbccCodeGen.emit(
      fixture :+ globalElement,
      fixture ++ Seq(globalElement, pairsData)
    )
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
        |extern void * agbc_match_elem_global(sn_i32);
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
      """extern sn_pair agbc_match_pairs;
        |void * agbc_match_elem_global(sn_i32 index) {
        |  return &((sn_pair *)&agbc_match_pairs)[index].f1;
        |}
        |typedef struct { sn_i8 f0; sn_i32 f1; } sn_elem_struct;
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
          |  def max(a: Int, b: Int): Int = if (a >= b) a else b
          |  @scala.scalanative.unsafe.exported("agbc_scala_longadd")
          |  def longAdd(a: Long, b: Long): Long = a + b
          |  @scala.scalanative.unsafe.exported("agbc_scala_muladd")
          |  def mulAdd(a: Int, b: Int, c: Int): Int = a * b + c
          |  @scala.scalanative.unsafe.exported("agbc_scala_float_bits")
          |  def floatBits(value: Float): Int = java.lang.Float.floatToRawIntBits(value)
          |  @scala.scalanative.unsafe.exported("agbc_scala_bits_float")
          |  def bitsFloat(value: Int): Float = java.lang.Float.intBitsToFloat(value)
          |  @scala.scalanative.unsafe.exported("agbc_scala_floatrem")
          |  def floatRemainder(a: Float, b: Float): Float = a % b
          |  @scala.scalanative.unsafe.exported("agbc_scala_doublerem")
          |  def doubleRemainder(a: Double, b: Double): Double = a % b
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
          |  def random(): Char = {
          |    AgbccRngState.value = AgbccRngState.value * 1103515245 + 24691
          |    AgbccRngState.count += 1
          |    (AgbccRngState.value >>> 16).toChar
          |  }
          |  @scala.scalanative.unsafe.exported("SeedRng")
          |  def seed(seed: Char): Unit = {
          |    AgbccRngState.value = seed.toInt
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
          "agbc_scala_float_bits",
          "agbc_scala_bits_float",
          "agbc_scala_floatrem",
          "agbc_scala_doublerem",
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
    assertTrue(source, source.contains("agbc_scala_float_bits"))
    assertTrue(source, source.contains("agbc_scala_bits_float"))
    assertTrue(source, source.contains("agbc_scala_floatrem"))
    assertTrue(source, source.contains("agbc_scala_doublerem"))
    assertTrue(source, source.contains("sn_agbcc_fmod32("))
    assertTrue(source, source.contains("sn_agbcc_fmod64("))
    assertTrue(source, source.contains("union { sn_f32 from; sn_i32 to; }"))
    assertTrue(source, source.contains("union { sn_i32 from; sn_f32 to; }"))
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
        |typedef unsigned short sn_u16;
        |typedef unsigned char sn_u8;
        |typedef signed long long sn_i64;
        |typedef unsigned long long sn_u64;
        |typedef float sn_f32;
        |typedef double sn_f64;
        |""".stripMargin + AgbccCodeGen.FloatingRemainderSupport +
        """
          |extern sn_u32 gRngValue;
          |extern sn_u32 xlat_random_sRandCount;
          |extern sn_u8 xlat_random_sUnknown;
          |sn_u16 Random(void) {
          |  gRngValue = (sn_u32)(gRngValue * 1103515245u + 24691u);
          |  xlat_random_sRandCount += 1;
          |  return (sn_u16)(gRngValue >> 16);
          |}
          |void SeedRng(sn_u16 seed) {
          |  gRngValue = seed;
          |  xlat_random_sUnknown = 0;
          |}
          |sn_i32 agbc_scala_add(sn_i32 a, sn_i32 b) {
          |  return (sn_i32)((sn_u32)a + (sn_u32)b);
          |}
          |sn_f32 agbc_scala_bits_float(sn_i32 value) {
          |  union { sn_i32 from; sn_f32 to; } bits;
          |  bits.from = value;
          |  return bits.to;
          |}
          |sn_f64 agbc_scala_doublerem(sn_f64 a, sn_f64 b) {
          |  return sn_agbcc_fmod64(a, b);
          |}
          |sn_i32 agbc_scala_float_bits(sn_f32 value) {
          |  union { sn_f32 from; sn_i32 to; } bits;
          |  bits.from = value;
          |  return bits.to;
          |}
          |sn_f32 agbc_scala_floatrem(sn_f32 a, sn_f32 b) {
          |  return sn_agbcc_fmod32(a, b);
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
