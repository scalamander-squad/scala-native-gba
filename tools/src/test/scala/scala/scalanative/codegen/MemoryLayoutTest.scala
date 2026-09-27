package scala.scalanative
package codegen

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.nir

/** Sizes computed by MemoryLayout must match the struct layout LLVM derives
 *  from the target data layout, otherwise objects are allocated too small
 *  (observed as heap corruption on armv4t: `Thread$MainThread$` has a Long
 *  field, MemoryLayout said 64 bytes, LLVM stored at offset 64 of 72).
 */
class MemoryLayoutTest {
  import nir.Type._

  private def platform(arch: String, is32Bit: Boolean, windows: Boolean = false) =
    PlatformInfo(
      targetTriple = None,
      targetsWindows = windows,
      is32Bit = is32Bit,
      isMultithreadingEnabled = false,
      useOpaquePointers = true,
      useGCYieldPointTraps = false,
      useCxxExceptions = false,
      alignOfLong = PlatformInfo.alignOfLong(arch, is32Bit, windows)
    )

  private val arm32 = platform("arm", is32Bit = true)
  private val riscv32 = platform("riscv32", is32Bit = true)
  private val x86 = platform("i386", is32Bit = true)
  private val x86win = platform("i386", is32Bit = true, windows = true)
  private val x86_64 = platform("x86_64", is32Bit = false)

  // struct { ptr; i1; ptr; i64 } -- shape of the tail of Thread$MainThread$
  private val tys = Seq(Ptr, Bool, Ptr, Long)
  private val dbl = Seq(Ptr, Byte, Double)

  @Test def alignOfLong(): Unit = {
    assertEquals(8, arm32.alignOfLong)
    assertEquals(8, riscv32.alignOfLong)
    assertEquals(4, x86.alignOfLong)
    assertEquals(8, x86win.alignOfLong)
    assertEquals(8, x86_64.alignOfLong)
  }

  @Test def alignmentOfLongAndDouble(): Unit = {
    assertEquals(8L, MemoryLayout.alignmentOf(Long)(arm32))
    assertEquals(8L, MemoryLayout.alignmentOf(Double)(arm32))
    assertEquals(4L, MemoryLayout.alignmentOf(Size)(arm32))
    assertEquals(4L, MemoryLayout.alignmentOf(Ptr)(arm32))
    assertEquals(4L, MemoryLayout.alignmentOf(Long)(x86))
    assertEquals(4L, MemoryLayout.alignmentOf(Double)(x86))
    assertEquals(8L, MemoryLayout.alignmentOf(Long)(x86_64))
  }

  @Test def structSizesMatchLLVMDataLayout(): Unit = {
    // clang -target armv4t-none-eabi: {ptr,i1,ptr,i64} = 24, {ptr,i8,double} = 16
    val armLayout = MemoryLayout(tys)(arm32)
    assertEquals(24L, armLayout.size)
    assertEquals(Seq(0L, 4L, 8L, 16L), armLayout.tys.map(_.offset))
    assertEquals(16L, MemoryLayout(dbl)(arm32).size)
    assertEquals(16L, MemoryLayout(Seq(Ptr, Int, Int, ArrayValue(Long, 0)))(arm32).size)
    // clang -target i386-linux-gnu: {ptr,i1,ptr,i64} = 20, {ptr,i8,double} = 16
    val x86Layout = MemoryLayout(tys)(x86)
    assertEquals(20L, x86Layout.size)
    assertEquals(Seq(0L, 4L, 8L, 12L), x86Layout.tys.map(_.offset))
    assertEquals(12L, MemoryLayout(Seq(Ptr, Int, Int, ArrayValue(Long, 0)))(x86).size)
    // 64-bit unchanged: {ptr,i1,ptr,i64} = 32
    assertEquals(32L, MemoryLayout(tys)(x86_64).size)
    assertEquals(16L, MemoryLayout(Seq(Ptr, Int, Int, ArrayValue(Long, 0)))(x86_64).size)
  }

  @Test def sizeOfLongArrayElementsUnchanged(): Unit = {
    assertEquals(24L, MemoryLayout.sizeOf(ArrayValue(Long, 3))(arm32))
    assertEquals(8L, MemoryLayout.sizeOf(Long)(arm32))
  }
}
