package scala.scalanative.codegen

import scala.scalanative.build.{Config, Discover, TargetTriple}

private[scalanative] case class PlatformInfo(
    targetTriple: Option[String],
    targetsWindows: Boolean,
    is32Bit: Boolean,
    isMultithreadingEnabled: Boolean,
    useOpaquePointers: Boolean,
    useGCYieldPointTraps: Boolean,
    useCxxExceptions: Boolean,
    /** ABI alignment (in bytes) of 8-byte scalars (`i64`/`double`) inside
     *  aggregates, as defined by the LLVM data layout of the target. The opt-in
     *  APCS profile uses four-byte alignment on 32-bit ARM. Must match LLVM,
     *  otherwise [[MemoryLayout]] and emitted aggregate accesses diverge.
     */
    alignOfLong: Int
) {
  val sizeOfPtr = if (is32Bit) 4 else 8
  val sizeOfPtrBits = sizeOfPtr * 8
}
private[scalanative] object PlatformInfo {
  def apply(config: Config): PlatformInfo = PlatformInfo(
    targetTriple = config.compilerConfig.targetTriple,
    targetsWindows = config.targetsWindows,
    is32Bit = config.compilerConfig.is32BitPlatform,
    isMultithreadingEnabled = config.compilerConfig.multithreadingSupport,
    useOpaquePointers =
      Discover.features.opaquePointers(config.compilerConfig).isAvailable,
    useGCYieldPointTraps = config.useTrapBasedGCYieldPoints,
    useCxxExceptions = config.usingCppExceptions,
    alignOfLong = alignOfLong(
      arch = config.compilerConfig.configuredOrDetectedTriple.arch,
      is32Bit = config.compilerConfig.is32BitPlatform,
      targetsWindows = config.targetsWindows,
      agbccAbi = config.compilerConfig.agbccAbi
    )
  )

  /** LLVM data layouts: `i64:64` (and f64 default 64) for every target except
   *  32-bit x86 without Windows, which has no `i64` entry (default ABI
   *  alignment 4) and `f64:32:64`. E.g. `armv4t-none-eabi`:
   *  `e-m:e-p:32:32-Fi8-i64:64-...`, `i386-linux-gnu`:
   *  `e-m:e-p:32:32-...-f64:32:64-...`, `i686-w64-mingw32`: `...-i64:64-...`.
   *  Passing `-mabi=apcs-gnu` changes ARM to four-byte ABI alignment for both.
   */
  def alignOfLong(
      arch: String,
      is32Bit: Boolean,
      targetsWindows: Boolean,
      agbccAbi: Boolean = false
  ): Int =
    if (agbccAbi) 4
    else if (!is32Bit) 8
    else if (arch == TargetTriple.Arch.x86 && !targetsWindows) 4
    else 8
}
