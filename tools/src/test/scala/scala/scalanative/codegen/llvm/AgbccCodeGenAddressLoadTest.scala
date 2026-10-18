package scala.scalanative.codegen.llvm

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.nir

class AgbccCodeGenAddressLoadTest {
  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
  private implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
  private val owner = nir.Global.Top("AgbccAddressLoadFixture")

  private def function(
      name: String,
      args: Seq[nir.Type],
      insts: Seq[nir.Inst]
  ): nir.Defn.Define =
    nir.Defn.Define(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern(name)),
      nir.Type.Function(args, nir.Type.Int),
      insts
    )

  @Test def xlatAddressGlobalsLowerIntLoadsToSymbolAddresses(): Unit = {
    val cell = owner.member(nir.Sig.Extern("__xlat_addr_gSineTable"))
    val address = nir.Val.Global(cell, nir.Type.Ptr)
    val definition = function(
      "agbc_match_address_global",
      Nil,
      Seq(
        nir.Inst.Label(nir.Local(9100), Nil),
        nir.Inst.Let(
          nir.Local(9101),
          nir.Op.Load(nir.Type.Int, address, None),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(9101), nir.Type.Int))
      )
    )
    val generated = AgbccCodeGen.emit(
      Seq(definition),
      Seq(definition),
      Map.empty[String, nir.Type.Function],
      preserveDefinitionOrder = true
    )
    assertTrue(generated.contains("((sn_i32)&gSineTable)"))
    assertTrue(generated.contains("extern sn_u8 gSineTable[];"))
    assertFalse(generated.contains("extern sn_i32 __xlat_addr_gSineTable"))
  }

  @Test def addressCellsReuseKnownFunctionAndDataDeclarations(): Unit = {
    val callback = function(
      "address_callback",
      Nil,
      Seq(nir.Inst.Label(nir.Local(1), Nil), nir.Inst.Ret(nir.Val.Int(42)))
    )
    val data = nir.Defn.Var(
      nir.Attrs.None,
      owner.member(nir.Sig.Extern("address_data")),
      nir.Type.Int,
      nir.Val.Int(17)
    )
    def reader(name: String, target: String) = function(
      name,
      Nil,
      Seq(
        nir.Inst.Label(nir.Local(1), Nil),
        nir.Inst.Let(
          nir.Local(2),
          nir.Op.Load(
            nir.Type.Int,
            nir.Val.Global(
              owner.member(nir.Sig.Extern("__xlat_addr_" + target)),
              nir.Type.Ptr
            ),
            None
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(2), nir.Type.Int))
      )
    )
    val readers = Seq(
      reader("callback_address", "address_callback"),
      reader("data_address", "address_data")
    )
    val external = nir.Defn.Declare(nir.Attrs.None, callback.name, callback.ty)
    // Exercise an emitted callback body and an external callback declaration.
    for (methods <- Seq(readers :+ callback, readers)) {
      val generated = AgbccCodeGen.emit(methods, methods ++ Seq(data, external))
      assertFalse(
        generated,
        generated.contains("extern sn_u8 address_callback[]")
      )
      assertFalse(generated, generated.contains("extern sn_u8 address_data[]"))
      assertTrue(
        generated,
        generated.contains("extern sn_i32 address_callback(void);")
      )
      assertTrue(generated, generated.contains("extern sn_i32 address_data;"))
      val file = java.nio.file.Files.createTempFile("agbcc-address-types", ".c")
      java.nio.file.Files.write(
        file,
        generated.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      )
      import scala.sys.process._
      try
        assertEquals(
          0,
          Process(
            Seq(
              sys.env.getOrElse("CC", "cc"),
              "-std=c99",
              "-Wno-pointer-to-int-cast",
              "-fsyntax-only",
              file.toString
            )
          ).!
        )
      finally java.nio.file.Files.deleteIfExists(file)
    }
  }

  @Test def xlatAddressMaterializationStaysAfterEarlierCalls(): Unit = {
    val cell = owner.member(nir.Sig.Extern("__xlat_addr_gSineTable"))
    val address = nir.Val.Global(cell, nir.Type.Ptr)
    val sideEffect =
      owner.member(nir.Sig.Extern("agbc_address_load_side_effect"))
    val sideEffectDecl = nir.Defn.Declare(
      nir.Attrs.None,
      sideEffect,
      nir.Type.Function(Nil, nir.Type.Int)
    )
    val definition = function(
      "agbc_address_load_after_call",
      Nil,
      Seq(
        nir.Inst.Label(nir.Local(9300), Nil),
        nir.Inst.Let(
          nir.Local(9301),
          nir.Op.Load(nir.Type.Int, address, None),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(9302),
          nir.Op.Call(
            nir.Type.Function(Nil, nir.Type.Int),
            nir.Val.Global(sideEffect, nir.Type.Ptr),
            Nil
          ),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(9303),
          nir.Op.Elem(
            nir.Type.ArrayValue(nir.Type.Int, 1),
            nir.Val.Local(nir.Local(9301), nir.Type.Int),
            Seq(nir.Val.Int(0))
          ),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(9304),
          nir.Op.Load(
            nir.Type.Int,
            nir.Val.Local(nir.Local(9303), nir.Type.Ptr),
            None
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(9304), nir.Type.Int))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition, sideEffectDecl))
    val callIndex = generated.indexOf("agbc_address_load_side_effect()")
    val addressIndex = generated.indexOf("((sn_i32)&gSineTable)")
    assertTrue(generated, callIndex >= 0)
    assertTrue(generated, addressIndex > callIndex)
    assertTrue(
      generated,
      generated.contains("sn_l9301 = ((sn_i32)&gSineTable)")
    )
  }

  @Test def xlatAddressMaterializationStaysAfterArithmeticHelpers(): Unit = {
    val cell = owner.member(nir.Sig.Extern("__xlat_addr_gSineDegreeTable"))
    val address = nir.Val.Global(cell, nir.Type.Ptr)
    val divide = owner.member(nir.Sig.Extern("__udivsi3"))
    val divideDecl = nir.Defn.Declare(
      nir.Attrs.None,
      divide,
      nir.Type.Function(Seq(nir.Type.Int, nir.Type.Int), nir.Type.Int)
    )
    val definition = function(
      "agbc_address_load_after_division",
      Seq(nir.Type.Int),
      Seq(
        nir.Inst.Label(
          nir.Local(9500),
          Seq(nir.Val.Local(nir.Local(9501), nir.Type.Int))
        ),
        nir.Inst.Let(
          nir.Local(9502),
          nir.Op.Call(
            nir.Type.Function(Seq(nir.Type.Int, nir.Type.Int), nir.Type.Int),
            nir.Val.Global(divide, nir.Type.Ptr),
            Seq(nir.Val.Local(nir.Local(9501), nir.Type.Int), nir.Val.Int(180))
          ),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(9503),
          nir.Op.Load(nir.Type.Int, address, None),
          nir.Next.None
        ),
        nir.Inst.Let(
          nir.Local(9504),
          nir.Op.Bin(
            nir.Bin.Iadd,
            nir.Type.Int,
            nir.Val.Local(nir.Local(9503), nir.Type.Int),
            nir.Val.Local(nir.Local(9502), nir.Type.Int)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(9504), nir.Type.Int))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition, divideDecl))
    val divideIndex = generated.indexOf("__udivsi3(")
    val addressIndex = generated.indexOf("((sn_i32)&gSineDegreeTable)")
    assertTrue(generated, divideIndex >= 0)
    assertTrue(generated, addressIndex > divideIndex)
  }

  @Test def unorderedLoadsDoNotForceVolatileC(): Unit = {
    val pointer = nir.Val.Local(nir.Local(9200), nir.Type.Ptr)
    val definition = function(
      "agbc_match_unordered_load",
      Seq(nir.Type.Ptr),
      Seq(
        nir.Inst.Label(nir.Local(9199), Seq(pointer)),
        nir.Inst.Let(
          nir.Local(9201),
          nir.Op.Load(nir.Type.Short, pointer, Some(nir.MemoryOrder.Unordered)),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(9201), nir.Type.Short))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition))
    assertTrue(generated.contains("*(sn_i16 *)(sn_l9200)"))
    assertFalse(generated.contains("*(volatile sn_i16 *)(sn_l9200)"))
  }

  @Test def xlatVolatileLoadMarkersLowerToVolatileScalarReads(): Unit = {
    val pointer = nir.Val.Local(nir.Local(9400), nir.Type.Ptr)
    val load16 = owner.member(nir.Sig.Extern("__xlat_vload16"))
    val load16Decl = nir.Defn.Declare(
      nir.Attrs.None,
      load16,
      nir.Type.Function(Seq(nir.Type.Ptr), nir.Type.Int)
    )
    val definition = function(
      "agbc_xlat_volatile_load16",
      Seq(nir.Type.Ptr),
      Seq(
        nir.Inst.Label(nir.Local(9399), Seq(pointer)),
        nir.Inst.Let(
          nir.Local(9401),
          nir.Op.Call(
            nir.Type.Function(Seq(nir.Type.Ptr), nir.Type.Int),
            nir.Val.Global(load16, nir.Type.Ptr),
            Seq(pointer)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Local(nir.Local(9401), nir.Type.Int))
      )
    )
    val generated =
      AgbccCodeGen.emit(Seq(definition), Seq(definition, load16Decl))
    assertTrue(generated.contains("(*((volatile sn_u16 *)(sn_l9400)))"))
    assertFalse(generated.contains("__xlat_vload16("))
  }

  @Test def volatileStoreMarkersPreserveWidthAndTruncation(): Unit = {
    val pointer = nir.Val.Local(nir.Local(1), nir.Type.Ptr)
    val stored = nir.Val.Local(nir.Local(2), nir.Type.Int)
    val callType =
      nir.Type.Function(Seq(nir.Type.Ptr, nir.Type.Int), nir.Type.Unit)
    val definitions = Seq(8, 16, 32).map { width =>
      val marker = owner.member(nir.Sig.Extern("__xlat_vstore" + width))
      function(
        "write" + width,
        Seq(nir.Type.Ptr, nir.Type.Int),
        Seq(
          nir.Inst.Label(nir.Local(0), Seq(pointer, stored)),
          nir.Inst.Let(
            nir.Local(3),
            nir.Op.Call(
              callType,
              nir.Val.Global(marker, nir.Type.Ptr),
              Seq(pointer, stored)
            ),
            nir.Next.None
          ),
          nir.Inst.Ret(nir.Val.Int(0))
        )
      )
    }
    val code = AgbccCodeGen.emit(definitions)
    assertFalse(code, code.contains("__xlat_vstore"))
    for (ty <- Seq("sn_u8", "sn_u16", "sn_u32"))
      assertTrue(code, code.contains("*((volatile " + ty + " *)("))
    val harness =
      """
        |int main(void) {
        |  union { sn_u32 align; sn_u8 bytes[12]; } cell;
        |  sn_u32 inputs[] = {0xffffffffu, 0x12345678u, 0u};
        |  int widths[] = {1, 2, 4};
        |  int i, j, k;
        |  for (i = 0; i < 3; ++i) for (j = 0; j < 3; ++j) {
        |    for (k = 0; k < 12; ++k) cell.bytes[k] = 0xa5;
        |    if (i == 0) write8(cell.bytes + 4, inputs[j]);
        |    if (i == 1) write16(cell.bytes + 4, inputs[j]);
        |    if (i == 2) write32(cell.bytes + 4, inputs[j]);
        |    for (k = 0; k < 12; ++k) {
        |      sn_u8 expected = k >= 4 && k < 4 + widths[i]
        |        ? (sn_u8)(inputs[j] >> ((k - 4) * 8)) : 0xa5;
        |      if (cell.bytes[k] != expected) return 1;
        |    }
        |  }
        |  return 0;
        |}
        |""".stripMargin
    val dir = java.nio.file.Files.createTempDirectory("agbcc-volatile-stores")
    val file = dir.resolve("stores.c")
    val executable = dir.resolve("stores")
    java.nio.file.Files.write(
      file,
      (code + harness).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )
    import scala.sys.process._
    assertEquals(
      0,
      Process(
        Seq(
          sys.env.getOrElse("CC", "cc"),
          "-std=c99",
          "-O2",
          file.toString,
          "-o",
          executable.toString
        )
      ).!
    )
    assertEquals(0, Process(Seq(executable.toString)).!)
  }

  @Test def xlatCompilerFencePinsItsDependencyWithMemoryClobber(): Unit = {
    val fence = owner.member(nir.Sig.Extern("__xlat_compiler_fence"))
    val fenceDecl = nir.Defn.Declare(
      nir.Attrs.None,
      fence,
      nir.Type.Function(Seq(nir.Type.Int), nir.Type.Unit)
    )
    val dependency = nir.Val.Local(nir.Local(9602), nir.Type.Int)
    val definition = function(
      "agbc_xlat_compiler_fence",
      Seq(nir.Type.Int),
      Seq(
        nir.Inst.Label(nir.Local(9600), Seq(dependency)),
        nir.Inst.Let(
          nir.Local(9601),
          nir.Op.Call(
            nir.Type.Function(Seq(nir.Type.Int), nir.Type.Unit),
            nir.Val.Global(fence, nir.Type.Ptr),
            Seq(dependency)
          ),
          nir.Next.None
        ),
        nir.Inst.Ret(nir.Val.Int(0))
      )
    )
    val generated = AgbccCodeGen.emit(Seq(definition, fenceDecl))
    assertTrue(
      generated.contains(
        "__asm__ volatile(\"\" : : \"r\"(sn_l9602) : \"memory\");"
      )
    )
    assertFalse(generated.contains("__xlat_compiler_fence("))
  }

}
