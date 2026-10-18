package scala.scalanative
package codegen
package llvm

import java.nio.file.{Path, Paths}

import scala.collection.mutable

import scala.scalanative.build.{BuildException, LTO, NativeConfig}
import scala.scalanative.io.VirtualDirectory
import scala.scalanative.nir

/** A deliberately small NIR-to-C backend whose C is compiled by the original
 *  agbcc compiler. This is a separate opt-in target path; unsupported NIR is
 *  rejected rather than sent through a different compiler.
 */
private[scalanative] object AgbccCodeGen {
  final val SourceExtension = ".agbc.c"
  private final val OutputName = "agbc-module" + SourceExtension
  private val CIdentifier = "[A-Za-z_][A-Za-z0-9_]*".r

  private def intrinsicHelperName(global: nir.Global.Member): Option[String] =
    symbol(global) match {
      case "llvm.ctpop.i32" => Some("sn_agbcc_intrinsic_ctpop_i32")
      case "llvm.ctlz.i32"  => Some("sn_agbcc_intrinsic_ctlz_i32")
      case "llvm.ctlz.i64"  => Some("sn_agbcc_intrinsic_ctlz_i64")
      case "llvm.cttz.i32"  => Some("sn_agbcc_intrinsic_cttz_i32")
      case "llvm.cttz.i64"  => Some("sn_agbcc_intrinsic_cttz_i64")
      case "llvm.fabs.f64"  => Some("sn_agbcc_intrinsic_fabs_f64")
      case "llvm.log10.f64" => Some("sn_agbcc_intrinsic_log10_f64")
      case "llvm.log.f64"   => Some("sn_agbcc_intrinsic_log_f64")
      case "llvm.sqrt.f64"  => Some("sn_agbcc_intrinsic_sqrt_f64")
      case _                => None
    }

  private def isStackIntrinsic(global: nir.Global.Member): Boolean =
    symbol(global) == "llvm.stacksave" || symbol(global) == "llvm.stackrestore"

  private def xlatSignedDivisionType(
      global: nir.Global.Member
  ): Option[nir.Type] =
    symbol(global) match {
      case "__xlat_sdiv32" => Some(nir.Type.Int)
      case "__xlat_sdiv64" => Some(nir.Type.Long)
      case _               => None
    }

  private def xlatVolatileLoadType(global: nir.Global.Member): Option[String] =
    symbol(global) match {
      case "__xlat_vload8"  => Some("sn_u8")
      case "__xlat_vload16" => Some("sn_u16")
      case "__xlat_vload32" => Some("sn_u32")
      case _                => None
    }

  private def xlatVolatileStoreType(global: nir.Global.Member): Option[String] =
    symbol(global) match {
      case "__xlat_vstore8"  => Some("sn_u8")
      case "__xlat_vstore16" => Some("sn_u16")
      case "__xlat_vstore32" => Some("sn_u32")
      case _                 => None
    }

  private def isXlatStackPosition(global: nir.Global.Member): Boolean =
    symbol(global) == "__xlat_frameaddr" || symbol(global) == "__xlat_stackptr"

  private def isXlatCompilerFence(global: nir.Global.Member): Boolean =
    symbol(global) == "__xlat_compiler_fence" || symbol(
      global
    ) == "__xlat_protect_stack"

  private def intrinsicHelperDefinition(name: String): String = name match {
    case "sn_agbcc_intrinsic_ctpop_i32" =>
      """static sn_i32 sn_agbcc_intrinsic_ctpop_i32(sn_i32 value) {
        |  sn_u32 bits = (sn_u32)value;
        |  bits = bits - ((bits >> 1) & 0x55555555u);
        |  bits = (bits & 0x33333333u) + ((bits >> 2) & 0x33333333u);
        |  bits = (bits + (bits >> 4)) & 0x0f0f0f0fu;
        |  return (sn_i32)((bits * 0x01010101u) >> 24);
        |}""".stripMargin
    case "sn_agbcc_intrinsic_ctlz_i32" =>
      """static sn_i32 sn_agbcc_intrinsic_ctlz_i32(sn_i32 value, sn_u8 zero_undef) {
        |  sn_u32 bits = (sn_u32)value;
        |  sn_i32 count = 0;
        |  (void)zero_undef;
        |  if (bits == 0) return 32;
        |  while ((bits & 0x80000000u) == 0) { bits <<= 1; count += 1; }
        |  return count;
        |}""".stripMargin
    case "sn_agbcc_intrinsic_ctlz_i64" =>
      """static sn_i64 sn_agbcc_intrinsic_ctlz_i64(sn_i64 value, sn_u8 zero_undef) {
        |  sn_u64 bits = (sn_u64)value;
        |  sn_i64 count = 0;
        |  (void)zero_undef;
        |  if (bits == 0) return 64;
        |  while ((bits & 0x8000000000000000ULL) == 0) { bits <<= 1; count += 1; }
        |  return count;
        |}""".stripMargin
    case "sn_agbcc_intrinsic_cttz_i32" =>
      """static sn_i32 sn_agbcc_intrinsic_cttz_i32(sn_i32 value, sn_u8 zero_undef) {
        |  sn_u32 bits = (sn_u32)value;
        |  sn_i32 count = 0;
        |  (void)zero_undef;
        |  if (bits == 0) return 32;
        |  while ((bits & 1u) == 0) { bits >>= 1; count += 1; }
        |  return count;
        |}""".stripMargin
    case "sn_agbcc_intrinsic_cttz_i64" =>
      """static sn_i64 sn_agbcc_intrinsic_cttz_i64(sn_i64 value, sn_u8 zero_undef) {
        |  sn_u64 bits = (sn_u64)value;
        |  sn_i64 count = 0;
        |  (void)zero_undef;
        |  if (bits == 0) return 64;
        |  while ((bits & 1u) == 0) { bits >>= 1; count += 1; }
        |  return count;
        |}""".stripMargin
    case "sn_agbcc_intrinsic_fabs_f64" =>
      """static sn_f64 sn_agbcc_intrinsic_fabs_f64(sn_f64 value) {
        |  union { sn_f64 value; sn_u64 bits; } converted;
        |  converted.value = value;
        |  converted.bits &= 0x7fffffffffffffffULL;
        |  return converted.value;
        |}""".stripMargin
    case "sn_agbcc_intrinsic_log10_f64" =>
      """extern double log10(double);
        |static sn_f64 sn_agbcc_intrinsic_log10_f64(sn_f64 value) { return log10(value); }""".stripMargin
    case "sn_agbcc_intrinsic_log_f64" =>
      """extern double log(double);
        |static sn_f64 sn_agbcc_intrinsic_log_f64(sn_f64 value) { return log(value); }""".stripMargin
    case "sn_agbcc_intrinsic_sqrt_f64" =>
      """extern double sqrt(double);
        |static sn_f64 sn_agbcc_intrinsic_sqrt_f64(sn_f64 value) { return sqrt(value); }""".stripMargin
    case _ => throw new BuildException(s"missing agbcc intrinsic helper $name")
  }

  private def symbol(global: nir.Global): String = global match {
    case nir.Global.Member(_, sig) if sig.isExtern =>
      val nir.Sig.Extern(id) = sig.unmangled: @unchecked
      id
    case _: nir.Global.Member => "_S" + global.mangle
    case _                    =>
      throw new BuildException(s"agbcc backend only emits methods: $global")
  }

  // AgbccCodeGen is only enabled together with the APCS ABI. Its 32-bit
  // aggregates align Long/Double to four bytes, matching Clang's APCS layout.
  private def aggregateAlignment(ty: nir.Type): Int = ty match {
    case nir.Type.Long | nir.Type.Double => 4
    case nir.Type.StructValue(fields)    =>
      fields.map(aggregateAlignment).reduceOption(_ max _).getOrElse(1)
    case nir.Type.ArrayValue(element, _)  => aggregateAlignment(element)
    case nir.Type.Unit | nir.Type.Nothing => 1
    case nir.Type.Bool | nir.Type.Byte    => 1
    case nir.Type.Char | nir.Type.Short   => 2
    case nir.Type.Int | nir.Type.Size | nir.Type.Float | nir.Type.Ptr |
        nir.Type.Null =>
      4
    case _: nir.Type.RefKind => 4
    case _                   =>
      throw new BuildException(
        s"agbcc backend does not support alignment of ${ty.show}"
      )
  }

  private def doubleStorage(name: String): String =
    s"union { sn_u64 sn_bits; sn_f64 sn_value; } $name"

  private def doubleBits(value: Double): String =
    java.lang.Long.toUnsignedString(
      java.lang.Double.doubleToRawLongBits(value),
      16
    )

  private def doubleExpression(value: Double): String =
    s"((union { sn_u64 bits; sn_f64 value; }){ .bits = 0x${doubleBits(value)}ULL }).value"

  private def doubleInitializer(value: Double): String =
    s"{ 0x${doubleBits(value)}ULL }"

  private def aggregateArrayElement(
      ty: nir.Type,
      where: String,
      ctype: (nir.Type, String) => String
  ): String =
    if (ty == nir.Type.Double) "union { sn_u64 sn_bits; sn_f64 sn_value; }"
    else ctype(ty, where)

  private def alignedMember(
      ty: nir.Type,
      name: String,
      where: String,
      ctype: (nir.Type, String) => String
  ): String = {
    val attribute =
      if (aggregateAlignment(ty) >= 8) " __attribute__((aligned(8)))" else ""
    s"${ctype(ty, where)} $name$attribute;"
  }

  // agbcc's bundled newlib does not include libm. Implement fmod with the
  // fdlibm shift/subtract algorithm so Frem remains self-contained in ROMs.
  // ====================================================
  // Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
  // Developed at SunSoft, a Sun Microsystems, Inc. business.
  // Permission to use, copy, modify, and distribute this software is freely
  // granted, provided that this notice is preserved.
  // ====================================================
  // The word order is intentionally little-endian; this backend only accepts
  // the ARMv4T GBA target.
  private[llvm] val FloatingRemainderSupport =
    """static sn_f64 sn_agbcc_fmod_invalid(void) {
      |  union { sn_f64 value; sn_u32 words[2]; } result;
      |  result.words[0] = 0;
      |  result.words[1] = 0x7ff80000u;
      |  return result.value;
      |}
      |
      |static int sn_agbcc_fmod_exponent(sn_u32 high, sn_u32 low) {
      |  int exponent;
      |  sn_u32 scan;
      |  if (high >= 0x00100000u)
      |    return (int)(high >> 20) - 1023;
      |  scan = high & 0x000fffffu;
      |  if (scan != 0) {
      |    exponent = -1022;
      |    while ((scan & 0x00100000u) == 0) {
      |      scan <<= 1;
      |      exponent -= 1;
      |    }
      |  } else {
      |    exponent = -1043;
      |    scan = low;
      |    while ((scan & 0x80000000u) == 0) {
      |      scan <<= 1;
      |      exponent -= 1;
      |    }
      |  }
      |  return exponent;
      |}
      |
      |static sn_f64 sn_agbcc_fmod(sn_f64 x, sn_f64 y) {
      |  union { sn_f64 value; sn_u32 words[2]; } xbits, ybits, result;
      |  sn_u32 hx, lx, hy, ly, sx, lz;
      |  sn_i32 hz;
      |  int ix, iy, n, less;
      |  xbits.value = x;
      |  ybits.value = y;
      |  hx = xbits.words[1];
      |  lx = xbits.words[0];
      |  hy = ybits.words[1] & 0x7fffffffu;
      |  ly = ybits.words[0];
      |  sx = hx & 0x80000000u;
      |  hx &= 0x7fffffffu;
      |  if ((hy | ly) == 0 || hx >= 0x7ff00000u ||
      |      hy > 0x7ff00000u || (hy == 0x7ff00000u && ly != 0))
      |    return sn_agbcc_fmod_invalid();
      |  if (hx < hy || (hx == hy && lx < ly)) return x;
      |  if (hx == hy && lx == ly) {
      |    result.words[0] = 0;
      |    result.words[1] = sx;
      |    return result.value;
      |  }
      |  ix = sn_agbcc_fmod_exponent(hx, lx);
      |  iy = sn_agbcc_fmod_exponent(hy, ly);
      |  if (ix >= -1022)
      |    hx = 0x00100000u | (hx & 0x000fffffu);
      |  else {
      |    n = -1022 - ix;
      |    if (n <= 31) {
      |      hx = (hx << n) | (lx >> (32 - n));
      |      lx <<= n;
      |    } else {
      |      hx = lx << (n - 32);
      |      lx = 0;
      |    }
      |  }
      |  if (iy >= -1022)
      |    hy = 0x00100000u | (hy & 0x000fffffu);
      |  else {
      |    n = -1022 - iy;
      |    if (n <= 31) {
      |      hy = (hy << n) | (ly >> (32 - n));
      |      ly <<= n;
      |    } else {
      |      hy = ly << (n - 32);
      |      ly = 0;
      |    }
      |  }
      |  n = ix - iy;
      |  while (n > 0) {
      |    less = hx < hy || (hx == hy && lx < ly);
      |    hz = (sn_i32)hx - (sn_i32)hy;
      |    lz = lx - ly;
      |    if (lx < ly) hz -= 1;
      |    if (less) {
      |      hx = (hx << 1) | (lx >> 31);
      |      lx <<= 1;
      |    } else {
      |      if (hz == 0 && lz == 0) {
      |        result.words[0] = 0;
      |        result.words[1] = sx;
      |        return result.value;
      |      }
      |      hx = (hz << 1) | (lz >> 31);
      |      lx = lz << 1;
      |    }
      |    n -= 1;
      |  }
      |  less = hx < hy || (hx == hy && lx < ly);
      |  hz = (sn_i32)hx - (sn_i32)hy;
      |  lz = lx - ly;
      |  if (lx < ly) hz -= 1;
      |  if (!less) { hx = hz; lx = lz; }
      |  if ((hx | lx) == 0) {
      |    result.words[0] = 0;
      |    result.words[1] = sx;
      |    return result.value;
      |  }
      |  while (hx < 0x00100000u) {
      |    hx = (hx << 1) | (lx >> 31);
      |    lx <<= 1;
      |    iy -= 1;
      |  }
      |  if (iy >= -1022) {
      |    hx = (hx - 0x00100000u) | ((sn_u32)(iy + 1023) << 20);
      |    result.words[1] = hx | sx;
      |    result.words[0] = lx;
      |  } else {
      |    n = -1022 - iy;
      |    if (n <= 20) {
      |      lx = (lx >> n) | (hx << (32 - n));
      |      hx >>= n;
      |    } else if (n <= 31) {
      |      lx = (hx << (32 - n)) | (lx >> n);
      |      hx = 0;
      |    } else {
      |      lx = hx >> (n - 32);
      |      hx = 0;
      |    }
      |    result.words[1] = hx | sx;
      |    result.words[0] = lx;
      |  }
      |  return result.value;
      |}
      |
      |static sn_f64 sn_agbcc_fmod64(sn_f64 x, sn_f64 y) {
      |  return sn_agbcc_fmod(x, y);
      |}
      |
      |static sn_f32 sn_agbcc_fmod32(sn_f32 x, sn_f32 y) {
      |  return (sn_f32)sn_agbcc_fmod((sn_f64)x, (sn_f64)y);
      |}
      |""".stripMargin

  def validateConfig(config: NativeConfig): Unit = {
    if (!config.agbccAbi)
      throw new BuildException("agbcc code generation requires agbccAbi=true")
    AgbccAbiTarget.validate(config.targetTriple)
    if (config.useIncrementalCompilation)
      throw new BuildException(
        "agbcc code generation does not support incremental compilation; " +
          "set withIncrementalCompilation(false)"
      )
    if (config.lto != LTO.none)
      throw new BuildException("agbcc code generation does not support LTO")
    val exportOrder = config.agbccCodegenExportOrder
    if (exportOrder.distinct != exportOrder ||
        (exportOrder.nonEmpty &&
        exportOrder.toSet != config.agbccCodegenExports))
      throw new BuildException(
        "agbcc export order must contain every selected export exactly once"
      )
  }

  def gen(defns: Seq[nir.Defn], dir: VirtualDirectory): Path = {
    val content = emit(defns)
    dir.write(Paths.get(OutputName))(_.write(content))
  }

  def selectExported(
      defns: Seq[nir.Defn],
      requested: Set[String] = Set.empty,
      requestedOrder: Seq[String] = Seq.empty
  ): Seq[nir.Defn.Define] = {
    val exported = defns.collect {
      case defn: nir.Defn.Define
          if defn.name.sig.isExtern && defn.attrs.isExtern &&
            !defn.attrs.isAbstract && !defn.attrs.isStub =>
        defn
    }
    val selected = if (requested.nonEmpty) {
      val available = exported.flatMap { defn =>
        defn.name.sig.unmangled match {
          case nir.Sig.Extern(id) => Some(id)
          case _                  => None
        }
      }.toSet
      val missing = requested -- available
      if (missing.nonEmpty)
        throw new BuildException(
          s"agbcc backend did not find requested exported symbols: ${missing.toSeq.sorted.mkString(", ")}"
        )
      exported.filter { defn =>
        defn.name.sig.unmangled match {
          case nir.Sig.Extern(id) => requested(id)
          case _                  => false
        }
      }
    } else exported
    if (requestedOrder.nonEmpty) {
      val selectedByName = selected.flatMap { defn =>
        defn.name.sig.unmangled match {
          case nir.Sig.Extern(id) => Some(id -> defn)
          case _                  => None
        }
      }.toMap
      val missing = selectedByName.keySet -- requestedOrder.toSet
      val unknown = requestedOrder.toSet -- selectedByName.keySet
      if (requestedOrder.distinct != requestedOrder || missing.nonEmpty || unknown.nonEmpty)
        throw new BuildException(
          "agbcc export order must contain every selected export exactly once"
        )
      requestedOrder.map(selectedByName)
    } else selected
  }

  /** An empty export filter is whole-program code generation, not an implicit
   *  fallback for methods outside the exported roots' reference graph. Data is
   *  still emitted by LLVM, but every lowered method body must go through
   *  agbcc.
   */
  def selectForCodegen(
      allDefns: Seq[nir.Defn],
      requested: Set[String] = Set.empty,
      requestedOrder: Seq[String] = Seq.empty
  ): Seq[nir.Defn.Define] = {
    val roots = selectExported(allDefns, requested, requestedOrder)
    if (requested.nonEmpty) selectReachable(roots, allDefns)
    else {
      val rootNames = roots.map(_.name).toSet
      roots ++ allDefns
        .collect {
          case d: nir.Defn.Define if !rootNames(d.name) => d
        }
        .sortBy(d => symbol(d.name))
    }
  }

  /** Select exported roots and their reachable NIR method bodies.
   *
   *  Follow method references in bodies and transitively referenced global
   *  initializers, including dispatch tables and function addresses copied to
   *  locals and called indirectly. agbcc must compile the Scala method graph
   *  behind an export as one C translation unit; leaving private callees in
   *  LLVM objects would produce a mixed instruction stream and could never
   *  yield an agbcc-parity ROM, even when the exported wrapper itself is
   *  emitted by agbcc.
   */
  def selectReachable(
      roots: Seq[nir.Defn.Define],
      allDefns: Seq[nir.Defn]
  ): Seq[nir.Defn.Define] = {
    val definitions = allDefns.collect {
      case d: nir.Defn.Define => d.name -> d
    }.toMap
    val initializers = allDefns.collect {
      case d: nir.Defn.Const => d.name -> d.rhs
      case d: nir.Defn.Var   => d.name -> d.rhs
    }.toMap
    val selected =
      mutable.LinkedHashMap.empty[nir.Global.Member, nir.Defn.Define]
    val visited = mutable.HashSet.empty[nir.Global.Member]
    val pending = mutable.Queue.empty[nir.Global.Member]
    roots.foreach(root => pending.enqueue(root.name))
    def referencedMethods(value: Any): Iterator[nir.Global.Member] =
      value match {
        case nir.Val.Global(target: nir.Global.Member, _) => Iterator(target)
        case values: Iterable[_] => values.iterator.flatMap(referencedMethods)
        case product: Product    =>
          product.productIterator.flatMap(referencedMethods)
        case _ => Iterator.empty
      }
    while (pending.nonEmpty) {
      val current = pending.dequeue()
      if (visited.add(current)) {
        definitions.get(current).foreach { definition =>
          selected(current) = definition
          definition.insts.iterator
            .flatMap(referencedMethods)
            .foreach(pending.enqueue(_))
        }
        initializers.get(current).foreach { initializer =>
          referencedMethods(initializer).foreach(pending.enqueue(_))
        }
      }
    }
    selected.values.toSeq
  }

  def gen(
      selected: Seq[nir.Defn.Define],
      allDefns: Seq[nir.Defn],
      dir: VirtualDirectory,
      exportSignatures: Map[String, nir.Type.Function] = Map.empty,
      preserveDefinitionOrder: Boolean = false
  ): Path = {
    val content = emit(
      selected,
      allDefns,
      exportSignatures,
      preserveDefinitionOrder
    )
    dir.write(Paths.get(OutputName))(_.write(content))
  }

  /** Emit one C translation unit for an already lowered NIR assembly. */
  private[llvm] def emit(defns: Seq[nir.Defn]): String =
    emit(
      defns.sortBy(defn => symbol(defn.name)),
      defns,
      Map.empty,
      preserveDefinitionOrder = true
    )

  private[llvm] def emit(
      defns: Seq[nir.Defn],
      allDefns: Seq[nir.Defn]
  ): String =
    emit(
      defns.sortBy(defn => symbol(defn.name)),
      allDefns,
      Map.empty,
      preserveDefinitionOrder = true
    )

  private[llvm] def emit(
      defns: Seq[nir.Defn],
      allDefns: Seq[nir.Defn],
      exportSignatures: Map[String, nir.Type.Function]
  ): String =
    emit(
      defns.sortBy(defn => symbol(defn.name)),
      allDefns,
      exportSignatures,
      preserveDefinitionOrder = true
    )

  private[llvm] def emit(
      defns: Seq[nir.Defn],
      allDefns: Seq[nir.Defn],
      exportSignatures: Map[String, nir.Type.Function],
      preserveDefinitionOrder: Boolean
  ): String = {
    val declarations = mutable.LinkedHashMap.empty[String, nir.Type.Function]
    val declarationGlobals =
      mutable.LinkedHashMap.empty[String, nir.Global.Member]
    val definitions = mutable.ArrayBuffer.empty[nir.Defn.Define]
    val dataTypes = allDefns.collect {
      case d: nir.Defn.Var   => d.name -> d.ty
      case d: nir.Defn.Const => d.name -> d.ty
    }.toMap
    val methodSignatures = allDefns.collect {
      case d: nir.Defn.Define  => d.name -> d.ty
      case d: nir.Defn.Declare => d.name -> d.ty
    }.toMap
    val methodAddressTargets = methodSignatures.map {
      case (global, ty) => symbol(global) -> (global, ty)
    }
    val dataAddressTargets = dataTypes.map {
      case (global, ty) => symbol(global) -> (global, ty)
    }
    val aggregateTypes = mutable.LinkedHashSet.empty[nir.Type.AggregateKind]
    def noteType(ty: nir.Type): Unit = ty match {
      case aggregate: nir.Type.AggregateKind =>
        if (aggregateTypes.add(aggregate))
          aggregate match {
            case nir.Type.StructValue(fields)    => fields.foreach(noteType)
            case nir.Type.ArrayValue(element, _) => noteType(element)
          }
      case nir.Type.Function(args, ret) =>
        args.foreach(noteType)
        noteType(ret)
      case nir.Type.Var(element) => noteType(element)
      case _                     => ()
    }
    def noteDefinitionTypes(defn: nir.Defn.Define): Unit = {
      noteType(defn.ty)
      defn.insts.foreach {
        case nir.Inst.Label(_, params) => params.foreach(p => noteType(p.ty))
        case nir.Inst.Let(_, op, _)    =>
          noteType(op.resty)
          op match {
            case nir.Op.Load(ty, _, _)     => noteType(ty)
            case nir.Op.Store(ty, _, _, _) => noteType(ty)
            case nir.Op.Elem(ty, _, _)     => noteType(ty)
            case nir.Op.Stackalloc(ty, _)  => noteType(ty)
            case nir.Op.SizeOf(ty)         => noteType(ty)
            case nir.Op.AlignmentOf(ty)    => noteType(ty)
            case nir.Op.Call(ty, _, _)     => noteType(ty)
            case _                         => ()
          }
        case nir.Inst.Ret(value) => noteType(value.ty)
        case _                   => ()
      }
    }
    val dataDefinitions = dataTypes.keySet
    val referencedData = mutable.LinkedHashSet.empty[nir.Global.Member]
    val referencedAddressTargets = mutable.LinkedHashSet.empty[String]

    def cIdentifier(global: nir.Global.Member): String = {
      intrinsicHelperName(global).getOrElse {
        val name = symbol(global)
        if (CIdentifier.pattern.matcher(name).matches()) name
        else {
          val bytes = name.getBytes(java.nio.charset.StandardCharsets.UTF_8)
          "sn_sym_" + bytes.map(b => f"${b & 0xff}%02x").mkString
        }
      }
    }

    val selectedSymbols = defns.map(defn => symbol(defn.name)).toSet
    val missingSignatures = exportSignatures.keySet -- selectedSymbols
    if (missingSignatures.nonEmpty)
      throw new BuildException(
        s"agbcc ABI signatures refer to unselected exports: ${missingSignatures.toSeq.sorted.mkString(", ")}"
      )

    def integerAbiType(ty: nir.Type): Boolean = ty match {
      case nir.Type.Bool | nir.Type.Byte | nir.Type.Short | nir.Type.Char |
          nir.Type.Int | nir.Type.Size =>
        true
      case _ => false
    }

    def abiCompatible(scalaType: nir.Type, cType: nir.Type): Boolean =
      scalaType == cType || integerAbiType(scalaType) && integerAbiType(cType)

    def effectiveSignature(
        name: nir.Global.Member,
        scalaType: nir.Type.Function
    ): nir.Type.Function = {
      val id = symbol(name)
      exportSignatures.get(id) match {
        case Some(abiType) =>
          if (scalaType.args.length != abiType.args.length ||
              !abiCompatible(scalaType.ret, abiType.ret) ||
              !scalaType.args.zip(abiType.args).forall {
                case (scalaArg, cArg) => abiCompatible(scalaArg, cArg)
              })
            throw new BuildException(
              s"incompatible agbcc ABI signature for $id: Scala $scalaType, C $abiType"
            )
          abiType
        case None => scalaType
      }
    }

    def asmAlias(global: nir.Global.Member): String = {
      val name = symbol(global)
      if (intrinsicHelperName(global).isDefined || cIdentifier(global) == name)
        ""
      else {
        val escaped = name.flatMap {
          case '\\' => "\\\\"
          case '"'  => "\\\""
          case c    => c.toString
        }
        s""" __asm__("$escaped")"""
      }
    }

    def addSignature(name: nir.Global.Member, ty: nir.Type.Function): Unit = {
      if (xlatSignedDivisionType(name).isEmpty && xlatVolatileLoadType(
            name
          ).isEmpty && xlatVolatileStoreType(
            name
          ).isEmpty && !isXlatCompilerFence(name) && !isXlatStackPosition(
            name
          )) {
        val id = symbol(name)
        val cType = effectiveSignature(name, ty)
        def abiType(ty: nir.Type): nir.Type = ty match {
          case nir.Type.Ptr | nir.Type.Null                  => nir.Type.Ptr
          case ref: nir.Type.RefKind if ref != nir.Type.Unit => nir.Type.Ptr
          case nir.Type.StructValue(fields)                  =>
            nir.Type.StructValue(fields.map(abiType))
          case nir.Type.ArrayValue(element, length) =>
            nir.Type.ArrayValue(abiType(element), length)
          case nir.Type.Function(args, ret) =>
            nir.Type.Function(args.map(abiType), abiType(ret))
          case other => other
        }
        def abiFunction(ty: nir.Type.Function): nir.Type.Function =
          abiType(ty).asInstanceOf[nir.Type.Function]
        declarations.get(id) match {
          case Some(existing) if abiFunction(existing) != abiFunction(cType) =>
            throw new BuildException(
              s"conflicting agbcc signatures for $id: $existing and $cType"
            )
          case Some(_) => ()
          case _       =>
            declarations(id) = cType
            declarationGlobals(id) = name
        }
      }
    }

    def unsupported(defn: nir.Defn, what: String): Nothing =
      throw new BuildException(
        s"agbcc backend does not support $what in ${defn.name.show} at ${defn.pos.show}"
      )

    def noteData(value: nir.Val): Unit = value match {
      case nir.Val.Global(g: nir.Global.Member, _)
          if symbol(g).startsWith("__xlat_addr_") =>
        val target = symbol(g).stripPrefix("__xlat_addr_")
        if (!CIdentifier.pattern.matcher(target).matches())
          throw new BuildException(s"invalid xlat address symbol $target")
        methodAddressTargets.get(target) match {
          case Some((method, ty)) =>
            addSignature(method, ty)
            noteType(ty)
          case None =>
            dataAddressTargets.get(target) match {
              case Some((data, ty)) =>
                referencedData += data
                noteType(ty)
              case None => referencedAddressTargets += target
            }
        }
      case nir.Val.Global(g: nir.Global.Member, _) if dataDefinitions(g) =>
        referencedData += g
      case nir.Val.Global(g: nir.Global.Member, _)
          if methodSignatures.contains(g) =>
        addSignature(g, methodSignatures(g))
      case nir.Val.Const(inner)          => noteData(inner)
      case nir.Val.StructValue(values)   => values.foreach(noteData)
      case nir.Val.ArrayValue(_, values) => values.foreach(noteData)
      case _                             => ()
    }
    def noteNextData(next: nir.Next): Unit = next match {
      case nir.Next.Label(_, args)    => args.foreach(noteData)
      case nir.Next.Case(value, tail) =>
        noteData(value)
        noteNextData(tail)
      case nir.Next.Unwind(exc, tail) =>
        noteData(exc)
        noteNextData(tail)
      case _ => ()
    }

    defnLoop(defns, addSignature, definitions, unsupported)
    val selectedDefinitions = definitions.map(defn => defn.name -> defn).toMap
    definitions.foreach(noteDefinitionTypes)
    val floatRemainderTypes = definitions
      .flatMap(_.insts.collect {
        case nir.Inst.Let(
              _,
              nir.Op.Bin(nir.Bin.Frem, ty, _, _),
              _
            ) if ty == nir.Type.Float || ty == nir.Type.Double =>
          ty
      })
      .distinct
    val constantValues = mutable.LinkedHashSet.empty[nir.Val]
    def addNestedConstant(value: nir.Val): Unit = value match {
      case nir.Val.Const(inner)          => addConstant(inner)
      case nir.Val.StructValue(values)   => values.foreach(addNestedConstant)
      case nir.Val.ArrayValue(_, values) => values.foreach(addNestedConstant)
      case _                             => ()
    }
    def addConstant(value: nir.Val): Unit = value match {
      case nir.Val.Const(inner)        => addConstant(inner)
      case nir.Val.StructValue(values) =>
        values.foreach(addNestedConstant)
        constantValues += value
      case nir.Val.ArrayValue(_, values) =>
        values.foreach(addNestedConstant)
        constantValues += value
      case other => constantValues += other
    }
    def findConstants(value: Any): Unit = value match {
      case nir.Val.Const(inner) => addConstant(inner)
      case _: nir.Val           => ()
      case product: Product    => product.productIterator.foreach(findConstants)
      case values: Iterable[_] => values.foreach(findConstants)
      case _                   => ()
    }
    definitions.foreach(_.insts.foreach(findConstants))
    val constantNames = constantValues.toSeq.zipWithIndex.map {
      case (value, index) => value -> s"sn_const_$index"
    }.toMap
    constantValues.foreach(value => noteType(value.ty))

    val emittedDefinitions =
      if (preserveDefinitionOrder) definitions.toSeq
      else definitions.sortBy(defn => symbol(defn.name)).toSeq
    emittedDefinitions.foreach { defn =>
      defn.insts.foreach {
        case nir.Inst.Let(_, nir.Op.Call(ty, nir.Val.Global(g, _), args), _) =>
          g match {
            case member: nir.Global.Member if dataDefinitions(member) =>
              noteData(nir.Val.Global(member, nir.Type.Ptr))
            case member: nir.Global.Member =>
              addSignature(
                member,
                selectedDefinitions.get(member).map(_.ty).getOrElse(ty)
              )
            case _ => unsupported(defn, s"call target $g")
          }
          args.foreach {
            noteData
          }
        case nir.Inst.Let(_, nir.Op.Call(_, callee, args), _) =>
          noteData(callee)
          args.foreach(noteData)
        case nir.Inst.Let(_, nir.Op.Load(_, ptr, _), _)         => noteData(ptr)
        case nir.Inst.Let(_, nir.Op.Store(_, ptr, value, _), _) =>
          noteData(ptr)
          noteData(value)
        case nir.Inst.Let(_, nir.Op.Elem(_, ptr, indexes), _) =>
          noteData(ptr)
          indexes.foreach(noteData)
        case nir.Inst.Let(_, nir.Op.Extract(aggregate, _), _) =>
          noteData(aggregate)
        case nir.Inst.Let(_, nir.Op.Insert(aggregate, value, _), _) =>
          noteData(aggregate)
          noteData(value)
        case nir.Inst.Let(_, nir.Op.Bin(_, _, left, right), _) =>
          noteData(left)
          noteData(right)
        case nir.Inst.Let(_, nir.Op.Comp(_, _, left, right), _) =>
          noteData(left)
          noteData(right)
        case nir.Inst.Let(_, nir.Op.Conv(_, _, value), _)    => noteData(value)
        case nir.Inst.Let(_, nir.Op.Stackalloc(_, count), _) => noteData(count)
        case nir.Inst.Let(
              _,
              nir.Op.Copy(nir.Val.Global(g: nir.Global.Member, _)),
              _
            ) if dataDefinitions(g) =>
          referencedData += g
        case _ =>
      }
      defn.insts.foreach {
        case nir.Inst.Ret(value)              => noteData(value)
        case nir.Inst.Jump(next)              => noteNextData(next)
        case nir.Inst.If(value, thenp, elsep) =>
          noteData(value)
          noteNextData(thenp)
          noteNextData(elsep)
        case nir.Inst.Switch(value, default, cases) =>
          noteData(value)
          noteNextData(default)
          cases.foreach(noteNextData)
        case _ => ()
      }
    }
    declarations.values.foreach(noteType)
    referencedData.foreach(global => noteType(dataTypes(global)))

    val out = new StringBuilder
    out.append("typedef signed char sn_i8;\n")
    out.append("typedef unsigned char sn_u8;\n")
    out.append("typedef signed short sn_i16;\n")
    out.append("typedef unsigned short sn_u16;\n")
    out.append("typedef signed int sn_i32;\n")
    out.append("typedef unsigned int sn_u32;\n")
    out.append("typedef signed long long sn_i64;\n")
    out.append("typedef unsigned long long sn_u64;\n\n")
    out.append("typedef float sn_f32;\n")
    out.append("typedef double sn_f64;\n\n")
    val requiredIntrinsicHelpers = declarationGlobals.valuesIterator
      .flatMap(intrinsicHelperName)
      .toSeq
      .distinct
      .sorted
    requiredIntrinsicHelpers.foreach { name =>
      out.append(intrinsicHelperDefinition(name)).append("\n\n")
    }
    if (definitions.exists(_.insts.exists {
          case nir.Inst.Let(_, _, nir.Next.Unwind(_, nir.Next.Label(_, _))) =>
            true
          case _ => false
        }))
      out.append("extern void *scalanative_catch(void *);\n\n")
    if (floatRemainderTypes.nonEmpty)
      out.append(FloatingRemainderSupport).append('\n')

    def aggregateName(ty: nir.Type.AggregateKind): String = {
      val bytes = ty.mangle.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      "sn_aggr_" + bytes.map(b => f"${b & 0xff}%02x").mkString
    }

    def ctype(ty: nir.Type, where: String): String = ty match {
      case nir.Type.Unit | nir.Type.Nothing  => "void"
      case nir.Type.Bool                     => "sn_u8"
      case nir.Type.Byte                     => "sn_i8"
      case nir.Type.Char                     => "sn_u16"
      case nir.Type.Short                    => "sn_i16"
      case nir.Type.Int                      => "sn_i32"
      case nir.Type.Size                     => "sn_u32"
      case nir.Type.Long                     => "sn_i64"
      case nir.Type.Float                    => "sn_f32"
      case nir.Type.Double                   => "sn_f64"
      case nir.Type.Ptr | nir.Type.Null      => "void *"
      case _: nir.Type.RefKind               => "void *"
      case aggregate: nir.Type.AggregateKind => aggregateName(aggregate)
      case _                                 =>
        throw new BuildException(
          s"agbcc backend does not support type ${ty.show} in $where"
        )
    }

    def aggregateDepth(ty: nir.Type.AggregateKind): Int = ty match {
      case nir.Type.StructValue(fields) =>
        1 + fields
          .collect { case t: nir.Type.AggregateKind => aggregateDepth(t) }
          .reduceOption(_ max _)
          .getOrElse(0)
      case nir.Type.ArrayValue(element: nir.Type.AggregateKind, _) =>
        1 + aggregateDepth(element)
      case _: nir.Type.ArrayValue => 1
    }

    aggregateTypes.toSeq.sortBy(aggregateDepth).foreach {
      case nir.Type.StructValue(fields) =>
        val members = fields.zipWithIndex.map {
          case (field, index) =>
            if (field == nir.Type.Double) doubleStorage(s"sn_f$index") + ";"
            else alignedMember(field, s"sn_f$index", "aggregate field", ctype)
        }
        val alignment = aggregateAlignment(nir.Type.StructValue(fields))
        val structAttribute =
          if (alignment >= 8) " __attribute__((aligned(8)))" else ""
        out
          .append("typedef struct")
          .append(structAttribute)
          .append(" { ")
          .append(members.mkString(" "))
          .append(" } ")
          .append(aggregateName(nir.Type.StructValue(fields)))
          .append(";\n")
      case nir.Type.ArrayValue(element, length) =>
        val alignment = aggregateAlignment(nir.Type.ArrayValue(element, length))
        val structAttribute =
          if (alignment >= 8) " __attribute__((aligned(8)))" else ""
        out
          .append("typedef struct")
          .append(structAttribute)
          .append(" { ")
          .append(
            aggregateArrayElement(element, "aggregate array element", ctype)
          )
          .append(" sn_values[")
          .append(length)
          .append("]")
          .append(if (alignment >= 8) " __attribute__((aligned(8)))" else "")
          .append("; } ")
          .append(aggregateName(nir.Type.ArrayValue(element, length)))
          .append(";\n")
    }
    if (aggregateTypes.nonEmpty) out.append('\n')

    def aggregateMemberInitializer(value: nir.Val): String = value match {
      case nir.Val.Double(n)             => doubleInitializer(n)
      case nir.Val.Zero(nir.Type.Double) => "{ 0 }"
      case other                         => constantInitializer(other)
    }

    def constantInitializer(value: nir.Val): String = value match {
      case nir.Val.StructValue(values) =>
        values.map(aggregateMemberInitializer).mkString("{ ", ", ", " }")
      case nir.Val.ArrayValue(_, values) =>
        values.map(aggregateMemberInitializer).mkString("{ { ", ", ", " } }")
      case nir.Val.ByteString(bytes) =>
        (bytes.map(b => b.toInt.toString) :+ "0").mkString("{ { ", ", ", " } }")
      case nir.Val.Const(inner) =>
        constantNames.get(inner) match {
          case Some(name) => s"((void *)&$name)"
          case None       =>
            throw new BuildException(
              s"missing agbcc constant for ${inner.show}"
            )
        }
      case nir.Val.Global(g: nir.Global.Member, _) if dataDefinitions(g) =>
        s"((void *)&${cIdentifier(g)})"
      case nir.Val.Null | nir.Val.Zero(nir.Type.Ptr) => "((void *)0)"
      case nir.Val.Zero(_: nir.Type.AggregateKind)   => "{0}"
      case nir.Val.Zero(_)                           => "0"
      case nir.Val.True                              => "1"
      case nir.Val.False                             => "0"
      case nir.Val.Byte(n)                           => n.toString
      case nir.Val.Short(n)                          => n.toString
      case nir.Val.Char(n)                           => n.toInt.toString
      case nir.Val.Int(n) if n == Int.MinValue       => "(-2147483647 - 1)"
      case nir.Val.Int(n)                            => n.toString
      case nir.Val.Size(n) if n == Int.MinValue      => "(-2147483647 - 1)"
      case nir.Val.Size(n)                           => n.toString
      case nir.Val.Long(n) if n == Long.MinValue     =>
        "(-9223372036854775807LL - 1LL)"
      case nir.Val.Long(n) => s"${n}LL"
      case nir.Val.Float(n)
          if !java.lang.Float.isNaN(n) && !java.lang.Float.isInfinite(n) =>
        s"${n}f"
      case nir.Val.Double(n) => n.toString
      case other             =>
        throw new BuildException(
          s"agbcc backend does not support constant initializer ${other.show}"
        )
    }

    declarations.toSeq
      .filterNot {
        case (name, _) =>
          intrinsicHelperName(declarationGlobals(name)).isDefined ||
            isStackIntrinsic(declarationGlobals(name))
      }
      .sortBy(_._1)
      .foreach {
        case (name, ty) =>
          val args = cFunctionArguments(
            ty,
            ctype,
            "function declaration argument"
          )
          out
            .append("extern ")
            .append(ctype(ty.ret, s"function declaration return for $name"))
            .append(' ')
            .append(cIdentifier(declarationGlobals(name)))
            .append('(')
            .append(if (args.isEmpty) "void" else args.mkString(", "))
            .append(')')
            .append(asmAlias(declarationGlobals(name)))
            .append(";\n")
      }
    referencedData.toSeq.map(g => symbol(g) -> g).sortBy(_._1).foreach {
      case (_, global) =>
        out
          .append("extern ")
          .append(ctype(dataTypes(global), "global data"))
          .append(' ')
          .append(cIdentifier(global))
          .append(asmAlias(global))
          .append(";\n")
    }
    referencedAddressTargets.toSeq.sorted.foreach { target =>
      out.append("extern sn_u8 ").append(target).append("[];\n")
    }
    constantValues.foreach { value =>
      val valueType =
        if (value.ty == nir.Type.Double)
          "union { sn_u64 sn_bits; sn_f64 sn_value; }"
        else ctype(value.ty, "constant value")
      val initializer = value match {
        case nir.Val.Double(n) => doubleInitializer(n)
        case other             => constantInitializer(other)
      }
      out
        .append("static const ")
        .append(valueType)
        .append(' ')
        .append(constantNames(value))
        .append(" = ")
        .append(initializer)
        .append(";\n")
    }
    if (constantValues.nonEmpty) out.append('\n')
    if (declarations.nonEmpty) out.append('\n')

    definitions.foreach { defn =>
      out.append(
        emitDefinition(
          defn,
          symbol,
          cIdentifier,
          asmAlias,
          ctype,
          referencedData.toSet,
          methodSignatures.keySet,
          constantNames,
          effectiveSignature(defn.name, defn.ty)
        )
      )
      out.append('\n')
    }
    out.toString
  }

  private def defnLoop(
      defns: Seq[nir.Defn],
      addSignature: (nir.Global.Member, nir.Type.Function) => Unit,
      definitions: mutable.ArrayBuffer[nir.Defn.Define],
      unsupported: (nir.Defn, String) => Nothing
  ): Unit = defns.foreach {
    case defn: nir.Defn.Define =>
      addSignature(defn.name, defn.ty)
      if (defn.attrs.isAbstract || defn.attrs.isStub)
        unsupported(defn, "abstract or stub definition")
      definitions += defn
    case defn: nir.Defn.Declare =>
      addSignature(defn.name, defn.ty)
    case defn => unsupported(defn, s"definition ${defn.getClass.getSimpleName}")
  }

  private def cFunctionArguments(
      ty: nir.Type.Function,
      ctype: (nir.Type, String) => String,
      where: String
  ): Seq[String] = {
    val varargIndex = ty.args.indexOf(nir.Type.Vararg)
    if (varargIndex >= 0 && varargIndex != ty.args.length - 1)
      throw new BuildException(
        s"agbcc backend requires trailing varargs in $where"
      )
    val fixedArgs = if (varargIndex >= 0) ty.args.dropRight(1) else ty.args
    if (varargIndex >= 0 && fixedArgs.isEmpty)
      throw new BuildException(
        s"agbcc backend requires a fixed argument before varargs in $where"
      )
    val args = fixedArgs.map(t => ctype(t, where))
    if (args.contains("void"))
      throw new BuildException(
        "agbcc backend does not support Unit function arguments"
      )
    if (varargIndex >= 0) args :+ "..." else args
  }

  private final case class FloatingBitcast(
      fromCType: String,
      toCType: String
  )

  private def floatingBitcast(
      from: nir.Type,
      to: nir.Type
  ): Option[FloatingBitcast] = {
    val (fromCType, toCType) = (from, to) match {
      case (nir.Type.Float, nir.Type.Int) =>
        ("sn_f32", "sn_i32")
      case (nir.Type.Float, nir.Type.Size) =>
        ("sn_f32", "sn_u32")
      case (nir.Type.Int, nir.Type.Float) =>
        ("sn_i32", "sn_f32")
      case (nir.Type.Size, nir.Type.Float) =>
        ("sn_u32", "sn_f32")
      case (nir.Type.Double, nir.Type.Long) =>
        ("sn_f64", "sn_i64")
      case (nir.Type.Long, nir.Type.Double) =>
        ("sn_i64", "sn_f64")
      case _ => return None
    }
    Some(FloatingBitcast(fromCType, toCType))
  }

  private def integerWidth(ty: nir.Type): Option[Int] = ty match {
    case nir.Type.Bool | nir.Type.Byte  => Some(8)
    case nir.Type.Short | nir.Type.Char => Some(16)
    case nir.Type.Int | nir.Type.Size   => Some(32)
    case nir.Type.Long                  => Some(64)
    case _                              => None
  }

  private def integerCType(ty: nir.Type, signed: Boolean): Option[String] =
    integerWidth(ty).flatMap {
      case 8  => Some(if (signed) "sn_i8" else "sn_u8")
      case 16 => Some(if (signed) "sn_i16" else "sn_u16")
      case 32 => Some(if (signed) "sn_i32" else "sn_u32")
      case 64 => Some(if (signed) "sn_i64" else "sn_u64")
      case _  => None
    }

  private def atomicScalar(ty: nir.Type): Boolean = ty match {
    case nir.Type.Bool | nir.Type.Byte | nir.Type.Short | nir.Type.Char |
        nir.Type.Int | nir.Type.Long | nir.Type.Size | nir.Type.Ptr |
        nir.Type.Null | nir.Type.Float | nir.Type.Double |
        _: nir.Type.RefKind =>
      true
    case _ => false
  }

  /** Lowering inserts checks even for literal nonzero divisors. LLVM normally
   *  removes these; do the same before selecting the agbcc function graph so
   *  dead error paths do not pull a runtime into an otherwise scalar object.
   */
  private[llvm] def prepareForAgbcc(defn: nir.Defn.Define): nir.Defn.Define = {
    import nir.{Comp, Inst, Next, Op, Val}
    if (defn.hasUnwind || !defn.insts.headOption.exists(
          _.isInstanceOf[Inst.Label]
        )) return defn
    val constants = mutable.HashMap.empty[nir.Local, nir.Val]
    def constant(value: nir.Val): nir.Val = value match {
      case Val.Local(id, _) => constants.getOrElse(id, value)
      case other            => other
    }
    def number(value: nir.Val): Option[BigInt] = constant(value) match {
      case Val.True                => Some(BigInt(1))
      case Val.False               => Some(BigInt(0))
      case Val.Byte(n)             => Some(BigInt(n))
      case Val.Short(n)            => Some(BigInt(n))
      case Val.Char(n)             => Some(BigInt(n.toInt))
      case Val.Int(n)              => Some(BigInt(n))
      case Val.Long(n)             => Some(BigInt(n))
      case Val.Size(n)             => Some(BigInt(n))
      case Val.Zero(_: nir.Type.I) => Some(BigInt(0))
      case Val.Zero(nir.Type.Bool) => Some(BigInt(0))
      case _                       => None
    }
    val folded = defn.insts.map {
      case let @ Inst.Let(id, Op.Copy(value), Next.None) =>
        constants(id) = constant(value)
        let
      case let @ Inst.Let(
            id,
            Op.Comp(comp @ (Comp.Ieq | Comp.Ine), _, left, right),
            Next.None
          ) =>
        for (l <- number(left); r <- number(right)) {
          val equal = l == r
          constants(id) =
            if (equal == (comp == Comp.Ieq)) Val.True else Val.False
        }
        let
      case branch @ Inst.If(value, thenp, elsep) =>
        constant(value) match {
          case Val.True  => Inst.Jump(thenp)(branch.pos)
          case Val.False => Inst.Jump(elsep)(branch.pos)
          case _         => branch
        }
      case other => other
    }
    val blocks = mutable.LinkedHashMap.empty[nir.Local, Vector[Inst]]
    var current: Option[nir.Local] = None
    folded.foreach {
      case label: Inst.Label =>
        current = Some(label.id); blocks(label.id) = Vector(label)
      case inst => current.foreach(id => blocks(id) = blocks(id) :+ inst)
    }
    def targets(next: Next): Seq[nir.Local] = next match {
      case Next.Label(id, _)    => Seq(id)
      case Next.Case(_, next)   => targets(next)
      case Next.Unwind(_, next) => targets(next)
      case _                    => Seq.empty
    }
    def successors(block: Seq[Inst]): Seq[nir.Local] = block.flatMap {
      case Inst.Jump(next)                => targets(next)
      case Inst.If(_, thenp, elsep)       => targets(thenp) ++ targets(elsep)
      case Inst.Switch(_, default, cases) =>
        targets(default) ++ cases.flatMap(targets)
      case _ => Seq.empty
    }
    val reachable = mutable.HashSet.empty[nir.Local]
    val pending = mutable.Queue(blocks.head._1)
    while (pending.nonEmpty) {
      val id = pending.dequeue()
      if (reachable.add(id))
        blocks
          .get(id)
          .foreach(block => successors(block).foreach(pending.enqueue(_)))
    }
    blocks.keys.filterNot(reachable).toSeq.foreach(blocks.remove)
    // Coalesce empty-argument, single-predecessor edges. This exposes the pure
    // expression chains split by the now-folded checks, without rewriting phis.
    var changed = true
    while (changed) {
      changed = false
      val predecessors = blocks.valuesIterator
        .flatMap(successors)
        .toSeq
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toMap
      val candidate = blocks.iterator.collectFirst {
        case (id, block) if (block.last match {
              case Inst.Jump(Next.Label(target, args)) =>
                target != id && blocks.contains(target) &&
                  target != blocks.head._1 && args.isEmpty && predecessors
                    .getOrElse(target, 0) == 1 &&
                  blocks(target).head.asInstanceOf[Inst.Label].params.isEmpty
              case _ => false
            }) =>
          id -> block.last
            .asInstanceOf[Inst.Jump]
            .next
            .asInstanceOf[Next.Label]
            .id
      }
      candidate.foreach {
        case (id, target) =>
          blocks(id) = blocks(id).dropRight(1) ++ blocks(target).tail
          blocks.remove(target)
          changed = true
      }
    }
    val rewritten = blocks.valuesIterator.flatten.toSeq
    if (rewritten == defn.insts) defn
    else defn.copy(insts = rewritten)(defn.pos)
  }

  /** Put a small, pure narrowing return tail on its incoming edges. Keeping the
   *  narrowing in a separate phi block obscures C's narrow return mode from
   *  agbcc's tree optimizer and introduces redundant sign extensions.
   */
  private[llvm] def narrowReturnTails(
      defn: nir.Defn.Define,
      cReturnType: nir.Type
  ): nir.Defn.Define = {
    import nir.{Inst, Next, Op, Type, Val}
    if (defn.hasUnwind || !Set[Type](
          Type.Bool,
          Type.Byte,
          Type.Short,
          Type.Char
        )(cReturnType))
      return defn
    def pure(op: Op): Boolean = op match {
      case Op.Copy(_) | Op.Conv(_, _, _) | Op.Comp(_, _, _, _) => true
      case Op.Bin(_, ty, _, _) => ty != Type.Float && ty != Type.Double
      case _                   => false
    }
    val blocks = mutable.LinkedHashMap.empty[nir.Local, Vector[Inst]]
    var current: Option[nir.Local] = None
    defn.insts.foreach {
      case label: Inst.Label =>
        current = Some(label.id)
        blocks(label.id) = Vector(label)
      case inst => current.foreach(id => blocks(id) = blocks(id) :+ inst)
    }
    val tails = blocks.collect {
      case (id, block)
          if block.head.asInstanceOf[Inst.Label].params.nonEmpty &&
            block.length <= 10 && block.last.isInstanceOf[Inst.Ret] &&
            block.slice(1, block.length - 1).forall {
              case Inst.Let(_, op, Next.None) => pure(op)
              case _                          => false
            } =>
        id -> block
    }.toMap
    val fresh = nir.Fresh(defn.insts)
    val rewritten = defn.insts.flatMap {
      case jump @ Inst.Jump(Next.Label(id, args)) if tails.contains(id) =>
        val tail = tails(id)
        val parameters = tail.head.asInstanceOf[Inst.Label].params
        if (parameters.length != args.length) Seq(jump)
        else {
          val values = mutable.HashMap.empty[nir.Local, nir.Val]
          parameters.zip(args).foreach {
            case (param, arg) => values(param.id) = arg
          }
          val substitute = new nir.Transform {
            override def onVal(value: nir.Val): nir.Val = value match {
              case Val.Local(local, _) => values.getOrElse(local, value)
              case other               => super.onVal(other)
            }
          }
          tail.tail.map {
            case let: Inst.Let =>
              val newId = fresh()
              val operation = substitute.onOp(let.op)
              values(let.id) = Val.Local(newId, operation.resty)
              let.copy(id = newId, op = operation)(let.pos, let.scopeId)
            case ret: Inst.Ret =>
              ret.copy(value = substitute.onVal(ret.value))(ret.pos)
            case other => other
          }
        }
      case other => Seq(other)
    }
    defn.copy(insts = rewritten)(defn.pos)
  }

  private def emitDefinition(
      originalDefn: nir.Defn.Define,
      symbol: nir.Global => String,
      cIdentifier: nir.Global.Member => String,
      asmAlias: nir.Global.Member => String,
      ctype: (nir.Type, String) => String,
      dataSymbols: Set[nir.Global.Member],
      methodSymbols: Set[nir.Global.Member],
      constantNames: Map[nir.Val, String],
      cSignature: nir.Type.Function
  ): String = {
    import nir.{Bin, Comp, Conv, Inst, Next, Op, Type, Val}
    val defn = narrowReturnTails(originalDefn, cSignature.ret)
    val reconstructedReturn = defn.insts != originalDefn.insts

    def fail(what: String): Nothing =
      throw new BuildException(
        s"agbcc backend does not support $what in ${defn.name.show} at ${defn.pos.show}"
      )

    if (cSignature.args.exists(_ == Type.Vararg)) fail("varargs")
    if (defn.insts.isEmpty) fail("empty function body")
    val entry = defn.insts.head match {
      case label: Inst.Label => label
      case _                 => fail("function without an entry label")
    }
    if (entry.params.length != defn.ty.args.length)
      fail("entry-label parameters differing from function signature")

    val labels = defn.insts.collect { case label: Inst.Label => label }
    val labelById = labels.map(label => label.id -> label).toMap
    def pointerType(ty: nir.Type): Boolean = ty match {
      case Type.Ptr | Type.Null | _: Type.RefKind => true
      case _                                      => false
    }
    val exceptionalOperationHandlers =
      mutable.LinkedHashMap.empty[nir.Local, nir.Local]
    val localTypes = mutable.LinkedHashMap.empty[nir.Local, nir.Type]
    def bind(local: nir.Local, ty: nir.Type): Unit = {
      if (ty != Type.Unit && ty != Type.Nothing)
        ctype(ty, s"local ${local.id}")
      if (localTypes.contains(local)) fail(s"duplicate local ${local.id}")
      if (ty != Type.Unit && ty != Type.Nothing) localTypes(local) = ty
    }
    entry.params.foreach(p => bind(p.id, p.ty))
    labels.drop(1).foreach(_.params.foreach(p => bind(p.id, p.ty)))
    defn.insts.drop(1).foreach {
      case _: Inst.Label            => ()
      case Inst.Let(id, op, unwind) =>
        unwind match {
          case Next.None                                   => ()
          case Next.Unwind(_, Next.Label(handlerId, args)) =>
            val handler = labelById.getOrElse(
              handlerId,
              fail(s"unknown exceptional handler label ${handlerId.id}")
            )
            if (args.length != 1 || handler.params.length != 1 ||
                !pointerType(handler.params.head.ty))
              fail(s"unsupported exceptional handler shape ${unwind.show}")
            exceptionalOperationHandlers(id) = handlerId
          case _ =>
            fail(s"unsupported unwind edge ${unwind.show} on ${op.show}")
        }
        bind(id, op.resty)
      case _ =>
    }
    val exceptionalHandlerParams =
      exceptionalOperationHandlers.values.toSeq.distinct.map { handlerId =>
        handlerId -> labelById(handlerId).params.head.id
      }.toMap
    val handlerLabelIds = exceptionalHandlerParams.keySet
    val blocksByLabel = labels.indices.map { index =>
      val start = defn.insts.indexWhere(_ == labels(index))
      val end =
        if (index + 1 < labels.length)
          defn.insts.indexWhere(_ == labels(index + 1), start + 1)
        else defn.insts.length
      labels(index).id -> defn.insts.slice(start, end)
    }.toMap
    def localRefs(value: Any): Set[nir.Local] = value match {
      case nir.Val.Local(id, _) => Set(id)
      case product: Product => product.productIterator.flatMap(localRefs).toSet
      case values: Iterable[_] => values.flatMap(localRefs).toSet
      case _                   => Set.empty
    }
    // Reconstruct expression trees only for return diamonds simplified above.
    // Inlining pure single-use SSA lets exposes narrow C arithmetic to agbcc;
    // loads and calls remain statements so observable access order is retained.
    val scalarExpressions: Map[nir.Local, nir.Op] = {
      def scalar(ty: Type): Boolean = ty match {
        case Type.Bool | Type.Byte | Type.Short | Type.Char | Type.Int |
            Type.Size | Type.Long | Type.Ptr | Type.Null =>
          true
        case _: Type.RefKind => true
        case _               => false
      }
      def simple(op: Op): Boolean = op match {
        case Op.Copy(v)           => scalar(v.ty)
        case Op.Conv(_, ty, v)    => scalar(ty) && scalar(v.ty)
        case Op.Comp(_, ty, _, _) => scalar(ty)
        case Op.Bin(_, ty, _, _)  => scalar(ty)
        case _                    => false
      }
      def division(op: Op): Boolean = op match {
        case Op.Bin(Bin.Sdiv | Bin.Udiv | Bin.Srem | Bin.Urem, _, _, _) => true
        case _                                                          => false
      }
      def references(value: Any): Iterator[nir.Local] = value match {
        case Val.Local(id, _)    => Iterator(id)
        case values: Iterable[_] => values.iterator.flatMap(references)
        case product: Product    => product.productIterator.flatMap(references)
        case _                   => Iterator.empty
      }
      if (!reconstructedReturn) Map.empty
      else {
        val uses = mutable.HashMap.empty[nir.Local, Vector[Int]]
        val blockOf = mutable.HashMap.empty[Int, nir.Local]
        var block = entry.id
        defn.insts.zipWithIndex.foreach {
          case (label: Inst.Label, index) =>
            block = label.id; blockOf(index) = block
          case (inst, index) =>
            blockOf(index) = block
            references(inst).foreach(id =>
              uses(id) = uses.getOrElse(id, Vector.empty) :+ index
            )
        }
        val operations = defn.insts.collect {
          case Inst.Let(id, op, _) => id -> op
        }.toMap
        val candidates = defn.insts.zipWithIndex.collect {
          case (Inst.Let(id, op, Next.None), index)
              if simple(op) && uses.getOrElse(id, Vector.empty).nonEmpty && {
                val narrowMask = op match {
                  case Op.Bin(Bin.And, Type.Int, _, Val.Int(255 | 65535)) =>
                    true
                  case Op.Bin(Bin.And, Type.Int, Val.Int(255 | 65535), _) =>
                    true
                  case _ => false
                }
                val cheap = op.isInstanceOf[Op.Copy] || op
                  .isInstanceOf[Op.Conv] || narrowMask
                val positions = uses(id)
                val consumesDivision = op match {
                  case Op.Bin(Bin.And, _, _, _) =>
                    localRefs(op)
                      .exists(local => operations.get(local).exists(division))
                  case _ => false
                }
                val divisionUse = !division(op) || (positions.size == 1 && (defn
                  .insts(positions.head) match {
                  case Inst.Let(_, Op.Bin(Bin.And, _, _, _), Next.None) => true
                  case _                                                => false
                }))
                !consumesDivision && divisionUse && (cheap || (positions.size == 1 && positions.head > index &&
                blockOf(positions.head) == blockOf(index) &&
                defn.insts.slice(index + 1, positions.head).forall {
                  case Inst.Let(_, crossed, Next.None) if simple(crossed) =>
                    !division(op) || !division(crossed)
                  case Inst.Let(_, _, Next.None) => !division(op)
                  case _                         => false
                }))
              } =>
            id -> op
        }.toMap
        def depth(id: nir.Local, seen: Set[nir.Local] = Set.empty): Int = {
          if (seen(id) || seen.size >= 32) 33
          else
            candidates.get(id) match {
              case Some(op) =>
                1 + localRefs(op).iterator
                  .map(child => depth(child, seen + id))
                  .foldLeft(0)(math.max)
              case None => 0
            }
        }
        candidates.filter { case (id, _) => depth(id) <= 32 }
      }
    }

    def expandedRefs(
        value: Any,
        seen: Set[nir.Local] = Set.empty
    ): Set[nir.Local] =
      localRefs(value).flatMap { id =>
        if (!seen(id) && scalarExpressions.contains(id))
          expandedRefs(scalarExpressions(id), seen + id)
        else Set(id)
      }
    def usedBy(inst: Inst, id: nir.Local): Boolean = inst match {
      case Inst.Let(local, _, _) if scalarExpressions.contains(local) => false
      case _ => expandedRefs(inst)(id)
    }
    def scheduleAgbccAddressLoads(sourceInstructions: Seq[Inst]): Seq[Inst] = {
      // An xlat address cell is lowered to the address of its target symbol.
      // Keep that materialization at its first use: if NIR has hoisted it to
      // the block entry, agbcc may place the literal-pool load before calls
      // that precede the corresponding C global access (unlike stock agbcc
      // output, where the address is formed at the access site).
      val delayedAddressLoads = {
        var ordered = sourceInstructions
        var handled = Set.empty[nir.Local]
        var searching = true
        while (searching) {
          val candidate = ordered.zipWithIndex.collectFirst {
            case (
                  Inst.Let(
                    id,
                    Op.Load(
                      Type.Int,
                      Val.Global(g: nir.Global.Member, _),
                      None
                    ),
                    _
                  ),
                  index
                ) if !handled(id) && symbol(g).startsWith("__xlat_addr_") =>
              id -> index
          }
          candidate match {
            case Some((addressLocal, loadIndex)) =>
              val load = ordered(loadIndex).asInstanceOf[Inst.Let]
              handled += addressLocal
              val firstUse = ordered.indexWhere(
                inst =>
                  if (reconstructedReturn) usedBy(inst, addressLocal)
                  else localRefs(inst).contains(addressLocal),
                loadIndex + 1
              )
              var insertion = firstUse
              if (!reconstructedReturn && firstUse > loadIndex) {
                // Form the base before its pure index expression.
                // Never cross an observable operation.
                var dependencies = localRefs(ordered(firstUse)) - addressLocal
                var cursor = firstUse - 1
                var scanning = true
                while (cursor > loadIndex && scanning) {
                  ordered(cursor) match {
                    case Inst.Let(
                          id,
                          op @ (Op.Copy(_) | Op.Conv(_, _, _) | Op.Bin(
                            Bin.Iadd | Bin.Isub | Bin.Imul | Bin.Shl |
                            Bin.Ashr | Bin.Lshr | Bin.And | Bin.Or | Bin.Xor,
                            _,
                            _,
                            _
                          )),
                          Next.None
                        ) =>
                      if (dependencies(id)) {
                        dependencies = (dependencies - id) ++ localRefs(op)
                        insertion = cursor
                      }
                    case _ => scanning = false
                  }
                  cursor -= 1
                }
              }
              if (insertion > loadIndex + 1) {
                val withoutLoad = ordered.patch(loadIndex, Nil, 1)
                ordered = withoutLoad.patch(insertion - 1, Seq(load), 0)
              }
            case None =>
              searching = false
          }
        }
        ordered
      }
      val instructions = delayedAddressLoads
      val lets = instructions.collect { case let: Inst.Let => let }
      val definitions = lets.map(let => let.id -> let.op).toMap
      def dependsOnLoad(
          local: nir.Local,
          seen: Set[nir.Local] = Set.empty
      ): Boolean =
        if (seen(local)) false
        else
          definitions.get(local) match {
            case Some(_: Op.Load) => true
            case Some(op)         =>
              localRefs(op).exists(ref => dependsOnLoad(ref, seen + local))
            case None => false
          }
      def safePure(op: Op): Boolean = op match {
        case Op.Copy(_) | Op.Conv(_, _, _) | Op.Comp(_, _, _, _) => true
        case Op.Bin(
              Bin.Sdiv | Bin.Udiv | Bin.Srem | Bin.Urem | Bin.Frem,
              _,
              _,
              _
            ) =>
          false
        case Op.Bin(_, _, _, _) => true
        case Op.Extract(_, _) | Op.Elem(_, _, _) | Op.SizeOf(_) |
            Op.AlignmentOf(_) =>
          true
        case _ => false
      }
      def pureDependencies(
          value: nir.Val,
          seen: Set[nir.Local] = Set.empty
      ): Option[Set[nir.Local]] =
        value match {
          case nir.Val.Local(local, _) if seen(local) => Some(Set.empty)
          case nir.Val.Local(local, _)                =>
            definitions.get(local) match {
              case Some(op) if safePure(op) =>
                localRefs(op).foldLeft(Option(Set(local))) {
                  case (Some(acc), ref) =>
                    pureDependencies(
                      nir.Val.Local(ref, nir.Type.Int),
                      seen + local
                    ).map(acc ++ _)
                  case (None, _) => None
                }
              case Some(_) => None
              case None    => Some(Set.empty)
            }
          case _ => Some(Set.empty)
        }
      val multiplies = lets.collect {
        case Inst.Let(_, Op.Bin(Bin.Imul, _, left, right), _) => left -> right
      }
      val hoistIds = multiplies.flatMap {
        case (left, right) =>
          val leftLoaded = localRefs(left).exists(dependsOnLoad(_))
          val rightLoaded = localRefs(right).exists(dependsOnLoad(_))
          if (leftLoaded && !rightLoaded)
            pureDependencies(right).getOrElse(Set.empty)
          else if (rightLoaded && !leftLoaded)
            pureDependencies(left).getOrElse(Set.empty)
          else Set.empty[nir.Local]
      }.toSet
      val addressLoadIndex = instructions.indexWhere {
        case Inst.Let(
              _,
              Op.Load(Type.Int, Val.Global(g: nir.Global.Member, _), None),
              _
            ) =>
          symbol(g).startsWith("__xlat_addr_")
        case _ => false
      }
      val multiplyScheduled = if (hoistIds.nonEmpty && addressLoadIndex >= 0) {
        val hoisted = instructions.collect {
          case let @ Inst.Let(id, _, _)
              if hoistIds(id) && instructions.indexOf(let) > addressLoadIndex =>
            let
        }
        val remaining = instructions.filterNot(hoisted.contains)
        val insertion = remaining.indexWhere {
          case Inst.Let(
                _,
                Op.Load(Type.Int, Val.Global(g: nir.Global.Member, _), None),
                _
              ) =>
            symbol(g).startsWith("__xlat_addr_")
          case _ => false
        }
        if (hoisted.nonEmpty && insertion >= 0)
          remaining.patch(insertion, hoisted, 0)
        else instructions
      } else instructions

      multiplyScheduled
    }
    val xlatAddressLocals = defn.insts.collect {
      case Inst.Let(
            id,
            Op.Load(Type.Int, Val.Global(g: nir.Global.Member, _), None),
            _
          ) if symbol(g).startsWith("__xlat_addr_") =>
        val target = symbol(g).stripPrefix("__xlat_addr_")
        if (!CIdentifier.pattern.matcher(target).matches())
          fail(s"invalid xlat address symbol $target")
        id -> target
    }.toMap

    def scheduleReturnReads(instructions: Seq[Inst]): Seq[Inst] = {
      if (!reconstructedReturn) return instructions
      def movablePast(inst: Inst): Boolean = inst match {
        case Inst.Let(
              _,
              Op.Copy(_) | Op.Conv(_, _, _) | Op.Comp(_, _, _, _) |
              Op.Bin(_, _, _, _) | Op.Elem(_, _, _) | Op.Extract(_, _) |
              Op.SizeOf(_) | Op.AlignmentOf(_),
              Next.None
            ) =>
          true
        case Inst.Let(local, _, _) if xlatAddressLocals.contains(local) => true
        case _                                                          => false
      }
      var ordered = instructions
      val handled = mutable.HashSet.empty[nir.Local]
      var next = ordered.zipWithIndex.collectFirst {
        case (load @ Inst.Let(id, Op.Load(_, _, order), Next.None), index)
            if (order.isEmpty || order.contains(
              nir.MemoryOrder.Unordered
            )) && !xlatAddressLocals.contains(id) =>
          (load, index)
      }
      while (next.nonEmpty) {
        val (load, index) = next.get
        handled += load.id
        val use = ordered.indexWhere(inst => usedBy(inst, load.id), index + 1)
        val limit = if (use < 0) ordered.size - 1 else use
        var insertion = index + 1
        while (insertion < limit && movablePast(ordered(insertion)))
          insertion += 1
        if (insertion > index + 1) {
          ordered =
            ordered.patch(index, Nil, 1).patch(insertion - 1, Seq(load), 0)
        }
        next = ordered.zipWithIndex.collectFirst {
          case (load @ Inst.Let(id, Op.Load(_, _, order), Next.None), index)
              if (order.isEmpty || order.contains(
                nir.MemoryOrder.Unordered
              )) && !handled(id) && !xlatAddressLocals.contains(id) =>
            (load, index)
        }
      }
      ordered
    }
    val orderedLabels =
      Seq(entry) ++ labels.drop(1).filter(label => handlerLabelIds(label.id)) ++
        labels.drop(1).filterNot(label => handlerLabelIds(label.id))
    val emittedInstructions =
      orderedLabels.flatMap(label =>
        if (reconstructedReturn)
          scheduleAgbccAddressLoads(
            scheduleReturnReads(blocksByLabel(label.id))
          )
        else scheduleAgbccAddressLoads(blocksByLabel(label.id))
      )
    val floatingBitcastLocals = defn.insts.flatMap {
      case Inst.Let(id, Op.Conv(Conv.Bitcast, to, from), _) =>
        floatingBitcast(from.ty, to).map(id -> _)
      case _ => None
    }

    def compatibleLocalType(declared: nir.Type, used: nir.Type): Boolean =
      declared == used || pointerType(declared) && pointerType(used)

    def localName(local: nir.Local): String = s"sn_l${local.id}"

    // Recover promoted narrow operands when signed C arithmetic is safe.
    val scalarDefinitions = defn.insts.collect {
      case Inst.Let(id, op, _) => id -> op
    }.toMap
    def smallInteger(
        value: nir.Val,
        seen: Set[nir.Local] = Set.empty
    ): Boolean = value match {
      case Val.Byte(_) | Val.Short(_) | Val.Char(_) | Val.True | Val.False =>
        true
      case Val.Local(id, _) if !seen(id) =>
        scalarDefinitions.get(id) match {
          case Some(Op.Copy(v)) => smallInteger(v, seen + id)
          case Some(Op.Conv(Conv.Sext | Conv.Zext, Type.Int, v)) =>
            Set[Type](Type.Bool, Type.Byte, Type.Short, Type.Char)(v.ty)
          case _ => false
        }
      case _ => false
    }

    def nonNegativeInt(v: nir.Val, seen: Set[nir.Local] = Set.empty): Boolean =
      v match {
        case Val.Int(n)                         => n >= 0
        case Val.Size(n)                        => n >= 0 && n <= Int.MaxValue
        case Val.Byte(n)                        => n >= 0
        case Val.Short(n)                       => n >= 0
        case Val.Char(_) | Val.True | Val.False => true
        case Val.Local(id, _) if !seen(id)      =>
          scalarDefinitions.get(id) match {
            case Some(Op.Copy(value)) => nonNegativeInt(value, seen + id)
            case Some(Op.Bin(Bin.And, Type.Int, left, right)) =>
              nonNegativeInt(left, seen + id) || nonNegativeInt(
                right,
                seen + id
              )
            case Some(Op.Conv(Conv.Zext, Type.Int, value)) =>
              Set[Type](Type.Bool, Type.Byte, Type.Short, Type.Char)(value.ty)
            case _ => false
          }
        case _ => false
      }

    def value(v: nir.Val): String = v match {
      case Val.Local(id, ty) =>
        val declaredType = localTypes.get(id)
        if (!declaredType.exists(compatibleLocalType(_, ty)))
          fail(
            s"unknown or mistyped local ${id.id}: used as ${ty.show}, declared as ${declaredType.map(_.show).getOrElse("<undefined>")}"
          )
        xlatAddressLocals.get(id) match {
          case Some(_) => localName(id)
          case None    =>
            scalarExpressions.get(id) match {
              case Some(op) =>
                s"((${ctype(op.resty, "scalar expression")})(${expression(op)}))"
              case None => localName(id)
            }
        }
      case Val.True                         => "1"
      case Val.False                        => "0"
      case Val.Null                         => "((void *)0)"
      case Val.Zero(Type.Ptr)               => "((void *)0)"
      case Val.Zero(ty: Type.AggregateKind) =>
        s"((${ctype(ty, "zero aggregate")}){0})"
      case Val.Zero(_)                       => "0"
      case Val.Byte(n)                       => n.toString
      case Val.Short(n)                      => n.toString
      case Val.Char(n)                       => n.toInt.toString
      case Val.Int(n) if n == Int.MinValue   => "(-2147483647 - 1)"
      case Val.Int(n)                        => n.toString
      case Val.Size(n) if n == Int.MinValue  => "(-2147483647 - 1)"
      case Val.Size(n)                       => n.toString
      case Val.Long(n) if n == Long.MinValue => "(-9223372036854775807LL - 1LL)"
      case Val.Long(n)                       => s"${n}LL"
      case Val.Float(n)
          if java.lang.Float.isNaN(n) || java.lang.Float.isInfinite(n) =>
        val bits = java.lang.Integer.toUnsignedString(
          java.lang.Float.floatToRawIntBits(n),
          16
        )
        s"((union { sn_u32 bits; sn_f32 value; }){ .bits = 0x${bits}u }).value"
      case Val.Float(n)  => s"${n.toString}f"
      case Val.Double(n) => doubleExpression(n)
      case Val.Global(g: nir.Global.Member, _) if dataSymbols(g) =>
        s"((void *)&${cIdentifier(g)})"
      case Val.Global(g: nir.Global.Member, _) if methodSymbols(g) =>
        s"((void *)&${cIdentifier(g)})"
      case Val.Const(inner) =>
        constantNames.get(inner) match {
          case Some(name) => s"((void *)&$name)"
          case None       => fail(s"missing constant ${inner.show}")
        }
      case _ => fail(s"value ${v.show}")
    }

    def integerType(ty: nir.Type): Boolean = ty match {
      case Type.Bool | Type.Byte | Type.Short | Type.Char | Type.Int |
          Type.Size | Type.Long =>
        true
      case _ => false
    }

    def floatingType(ty: nir.Type): Boolean =
      ty == Type.Float || ty == Type.Double

    def aggregateAccess(
        root: String,
        rootType: nir.Type,
        indexes: Seq[Int]
    ): String = {
      var current = rootType
      var access = root
      indexes.foreach { index =>
        current match {
          case Type.StructValue(fields)
              if index >= 0 && index < fields.length =>
            access = s"($access).sn_f$index"
            current = fields(index)
          case Type.ArrayValue(element, length)
              if index >= 0 && index < length =>
            access = s"($access).sn_values[$index]"
            current = element
          case Type.StructValue(fields) =>
            fail(s"struct aggregate index $index of ${fields.length}")
          case Type.ArrayValue(_, length) =>
            fail(s"array aggregate index $index of $length")
          case _ => fail(s"aggregate index $index into ${current.show}")
        }
      }
      if (current == Type.Double) s"($access).sn_value" else access
    }

    def unsignedValue(v: String, ty: nir.Type, unsignedC: String): String =
      ty match {
        case Type.Byte              => s"(($unsignedC)(sn_u8)($v))"
        case Type.Short | Type.Char => s"(($unsignedC)(sn_u16)($v))"
        case _                      => s"($unsignedC)($v)"
      }

    def signedValue(v: String, ty: nir.Type, signedC: String): String = {
      val sourceC = integerCType(ty, signed = true).getOrElse(
        fail(s"signed integer operation on ${ty.show}")
      )
      if (sourceC == signedC) s"($signedC)($v)"
      else s"(($signedC)(($sourceC)($v)))"
    }

    def expression(op: nir.Op): String = op match {
      case Op.Copy(v)                     => value(v)
      case Op.Extract(aggregate, indexes) =>
        if (indexes.isEmpty) fail("empty aggregate extraction path")
        aggregateAccess(value(aggregate), aggregate.ty, indexes)
      case Op.Load(Type.Int, nir.Val.Global(g: nir.Global.Member, _), None)
          if symbol(g).startsWith("__xlat_addr_") =>
        val target = symbol(g).stripPrefix("__xlat_addr_")
        if (!CIdentifier.pattern.matcher(target).matches())
          fail(s"invalid xlat address symbol $target")
        s"((sn_i32)&$target)"
      case Op.Load(ty, ptr, None) =>
        s"(*(${ctype(ty, "load type")} *)(${value(ptr)}))"
      case Op.Load(_, _, Some(_))   => fail("atomic load")
      case Op.Stackalloc(ty, count) =>
        s"((void *)__builtin_alloca(sizeof(${ctype(ty, "stack allocation type")}) * (${value(count)})))"
      case Op.Elem(ty, ptr, indexes) =>
        if (indexes.isEmpty) fail("empty element index path")
        def aggregateDeclaration(aggregate: Type, name: String): String =
          aggregate match {
            case Type.StructValue(fields) =>
              val members = fields.zipWithIndex.map {
                case (field, i) =>
                  field match {
                    case _: Type.AggregateKind =>
                      aggregateDeclaration(field, s"sn_f$i") + ";"
                    case Type.Double => doubleStorage(s"sn_f$i") + ";"
                    case _           =>
                      alignedMember(field, s"sn_f$i", "aggregate member", ctype)
                  }
              }
              val attribute =
                if (aggregateAlignment(aggregate) >= 8)
                  " __attribute__((aligned(8)))"
                else ""
              s"struct$attribute { ${members.mkString(" ")} } $name"
            case Type.ArrayValue(element, length) =>
              val declarator =
                if (name == "*") s"(*)[$length]" else s"$name[$length]"
              val elementDeclaration =
                if (element == Type.Double)
                  s"union { sn_u64 sn_bits; sn_f64 sn_value; } $declarator"
                else aggregateDeclaration(element, declarator)
              val memberAlignment =
                if (name != "*" && aggregateAlignment(aggregate) >= 8)
                  " __attribute__((aligned(8)))"
                else ""
              elementDeclaration + memberAlignment
            case scalar => s"${ctype(scalar, "aggregate member")} $name"
          }
        var current = ty
        val pointerType = aggregateDeclaration(ty, "*")
        var location =
          s"(($pointerType)(${value(ptr)}))[${value(indexes.head)}]"
        indexes.tail.foreach { index =>
          current match {
            case Type.ArrayValue(element, _) =>
              location = s"($location)[${value(index)}]"
              current = element
            case Type.StructValue(fields) =>
              val fieldIndex = index match {
                case Val.Int(n)  => n
                case Val.Long(n) => n.toInt
                case _ => fail(s"dynamic struct element index ${index.show}")
              }
              if (fieldIndex < 0 || fieldIndex >= fields.length)
                fail(s"struct element index $fieldIndex")
              location = s"($location).sn_f$fieldIndex"
              current = fields(fieldIndex)
            case _ =>
              fail(s"element index into non-aggregate type ${current.show}")
          }
        }
        s"((void *)&($location))"
      case Op.Bin(bin, ty, left, right) if integerType(ty) =>
        val l = value(left)
        val r = value(right)
        val wide = ty == Type.Long
        val unsignedC = if (wide) "sn_u64" else "sn_u32"
        val signedC = if (wide) "sn_i64" else "sn_i32"
        bin match {
          // A promoted 8/16-bit operand cannot overflow a signed Int on negation.
          // Preserve C's signed negation tree instead of hiding it behind u32
          // subtraction, so agbcc can combine it with the narrow return mode.
          case Bin.Isub
              if ty == Type.Int && (left == Val
                .Int(0) || left == Val.Zero(Type.Int)) && smallInteger(right) =>
            s"(-($r))"
          case Bin.Iadd | Bin.Isub | Bin.Imul =>
            val operator = bin match {
              case Bin.Iadd => "+"
              case Bin.Isub => "-"
              case _        => "*"
            }
            s"((${ctype(ty, "integer operation")})(($unsignedC)($l) $operator ($unsignedC)($r)))"
          case Bin.Sdiv =>
            s"(${signedValue(l, ty, signedC)} / ${signedValue(r, ty, signedC)})"
          case Bin.Udiv =>
            s"(${unsignedValue(l, ty, unsignedC)} / ${unsignedValue(r, ty, unsignedC)})"
          case Bin.Srem =>
            s"(${signedValue(l, ty, signedC)} % ${signedValue(r, ty, signedC)})"
          case Bin.Urem =>
            s"(${unsignedValue(l, ty, unsignedC)} % ${unsignedValue(r, ty, unsignedC)})"
          case Bin.And
              if reconstructedReturn && ty == Type.Int && (right == Val.Int(
                65535
              ) || right == Val.Int(255)) =>
            val narrowed = if (right == Val.Int(65535)) "sn_u16" else "sn_u8"
            s"(($narrowed)($l))"
          case Bin.And
              if reconstructedReturn && ty == Type.Int && (left == Val.Int(
                65535
              ) || left == Val.Int(255)) =>
            val narrowed = if (left == Val.Int(65535)) "sn_u16" else "sn_u8"
            s"(($narrowed)($r))"
          case Bin.And  => s"($l & $r)"
          case Bin.Or   => s"($l | $r)"
          case Bin.Xor  => s"($l ^ $r)"
          case Bin.Shl  => s"(($unsignedC)($l) << ($r))"
          case Bin.Lshr =>
            s"(${unsignedValue(l, ty, unsignedC)} >> ($r))"
          case Bin.Ashr =>
            s"(${signedValue(l, ty, signedC)} >> ($r))"
          case _ => fail(s"binary operation ${bin.show}")
        }
      case Op.Bin(Bin.Frem, ty, left, right)
          if ty == Type.Float || ty == Type.Double =>
        val function =
          if (ty == Type.Float) "sn_agbcc_fmod32" else "sn_agbcc_fmod64"
        s"$function(${value(left)}, ${value(right)})"
      case Op.Bin(bin, ty, left, right) if floatingType(ty) =>
        val operator = bin match {
          case Bin.Fadd => "+"
          case Bin.Fsub => "-"
          case Bin.Fmul => "*"
          case Bin.Fdiv => "/"
          case _        => fail(s"floating binary operation ${bin.show}")
        }
        s"((${ctype(ty, "floating operation")})(${value(left)} $operator ${value(right)}))"
      case Op.Comp(comp, ty, left, right) if integerType(ty) =>
        val (operator, unsignedComparison) = comp match {
          case Comp.Ieq => ("==", false)
          case Comp.Ine => ("!=", false)
          case Comp.Sgt => (">", false)
          case Comp.Sge => (">=", false)
          case Comp.Slt => ("<", false)
          case Comp.Sle => ("<=", false)
          case Comp.Ugt => (">", true)
          case Comp.Uge => (">=", true)
          case Comp.Ult => ("<", true)
          case Comp.Ule => ("<=", true)
          case _        => fail(s"comparison ${comp.show}")
        }
        // GCC 2.9 can turn a promoted u16 signed comparison into a signed
        // comparison of values shifted left by 16. Preserve unsigned mode
        // when both signed operands are proven nonnegative (same ordering).
        val isUnsigned = unsignedComparison || (ty == Type.Int &&
        Set[Comp](Comp.Sgt, Comp.Sge, Comp.Slt, Comp.Sle)(comp) &&
        nonNegativeInt(left) && nonNegativeInt(right))
        val cast =
          if (isUnsigned) {
            if (ty == Type.Long) "sn_u64" else "sn_u32"
          } else if (ty == Type.Long) "sn_i64"
          else "sn_i32"
        val leftValue =
          if (isUnsigned) unsignedValue(value(left), ty, cast)
          else signedValue(value(left), ty, cast)
        val rightValue =
          if (isUnsigned) unsignedValue(value(right), ty, cast)
          else signedValue(value(right), ty, cast)
        s"((sn_u8)(($leftValue) $operator ($rightValue)))"
      case Op.Comp(comp, ty, left, right) if pointerType(ty) =>
        val operator = comp match {
          case Comp.Ieq => "=="
          case Comp.Ine => "!="
          case _        => fail(s"pointer comparison ${comp.show}")
        }
        s"((sn_u8)((${value(left)}) $operator (${value(right)})))"
      case Op.Comp(comp, ty, left, right) if floatingType(ty) =>
        val operator = comp match {
          case Comp.Feq => "=="
          case Comp.Fne => "!="
          case Comp.Fgt => ">"
          case Comp.Fge => ">="
          case Comp.Flt => "<"
          case Comp.Fle => "<="
          case _        => fail(s"floating comparison ${comp.show}")
        }
        s"((sn_u8)((${value(left)}) $operator (${value(right)})))"
      case Op.Conv(Conv.Bitcast, ty, v) =>
        floatingBitcast(v.ty, ty) match {
          case Some(_) => fail("floating bitcast outside a local instruction")
          case None
              if pointerType(ty) && pointerType(v.ty) ||
                integerWidth(ty).isDefined &&
                integerWidth(ty) == integerWidth(v.ty) =>
            s"((${ctype(ty, "bitcast")})(${value(v)}))"
          case None => fail(s"bitcast from ${v.ty.show} to ${ty.show}")
        }
      case Op.Conv(conv, ty, v)
          if integerType(ty) || ty == Type.Ptr || floatingType(ty) =>
        conv match {
          case Conv.Zext | Conv.Sext | Conv.ZSizeCast | Conv.SSizeCast =>
            val fromWidth = integerWidth(v.ty)
            val toWidth = integerWidth(ty)
            val shouldExtend = conv match {
              case Conv.Zext | Conv.Sext           => true
              case Conv.ZSizeCast | Conv.SSizeCast =>
                (for {
                  from <- fromWidth
                  to <- toWidth
                } yield from < to).getOrElse(false)
              case _ => false
            }
            if (shouldExtend) {
              val signed = conv == Conv.Sext || conv == Conv.SSizeCast
              val sourceCType = integerCType(v.ty, signed).getOrElse(
                fail(s"${conv.show} source type ${v.ty.show}")
              )
              s"((${ctype(ty, "conversion")})(($sourceCType)(${value(v)})))"
            } else s"((${ctype(ty, "conversion")})(${value(v)}))"
          case Conv.Trunc | Conv.Bitcast | Conv.Ptrtoint | Conv.Inttoptr |
              Conv.Fptrunc | Conv.Fpext =>
            s"((${ctype(ty, "conversion")})(${value(v)}))"
          case Conv.Fptoui | Conv.Fptosi =>
            val signed = conv == Conv.Fptosi
            val intermediate = integerCType(ty, signed).getOrElse(
              fail(s"${conv.show} target type ${ty.show}")
            )
            s"((${ctype(ty, "conversion")})(($intermediate)(${value(v)})))"
          case Conv.Uitofp | Conv.Sitofp =>
            val signed = conv == Conv.Sitofp
            val intermediate = integerCType(v.ty, signed).getOrElse(
              fail(s"${conv.show} source type ${v.ty.show}")
            )
            s"((${ctype(ty, "conversion")})(($intermediate)(${value(v)})))"
        }
      case Op.Call(ty, Val.Global(g: nir.Global.Member, _), args)
          if xlatSignedDivisionType(g).isDefined =>
        val operandType = xlatSignedDivisionType(g).get
        if (ty.ret != operandType || args.length != 2 || args.exists(
              _.ty != operandType
            ))
          fail(
            s"${symbol(g)} requires two ${operandType.show} operands and the same result type"
          )
        val cast = if (operandType == Type.Long) "sn_i64" else "sn_i32"
        // These explicit ARM arithmetic markers use agbcc/libgcc semantics,
        // including zero divisors and signed overflow, rather than JVM traps.
        s"(($cast)((${value(args.head)})) / ($cast)((${value(args(1))})))"
      case Op.Call(ty, Val.Global(g: nir.Global.Member, _), args)
          if xlatVolatileLoadType(g).isDefined =>
        if (ty.ret != Type.Int)
          fail(s"${symbol(g)} result type ${ty.ret.show}")
        if (args.length != 1)
          fail(s"${symbol(g)} argument count ${args.length}")
        s"(*((volatile ${xlatVolatileLoadType(g).get} *)(${value(args.head)})))"
      case Op.Call(_, Val.Global(g: nir.Global.Member, _), args)
          if !dataSymbols(g) =>
        s"${cIdentifier(g)}(${args.map(value).mkString(", ")})"
      case Op.Call(ty, callee, args) =>
        val argumentTypes =
          AgbccCodeGen.cFunctionArguments(
            ty,
            ctype,
            "function pointer argument"
          )
        val signature =
          s"${ctype(ty.ret, "function pointer return")} (*)(${if (argumentTypes.isEmpty) "void"
            else argumentTypes.mkString(", ")})"
        s"(($signature)(${value(callee)}))(${args.map(value).mkString(", ")})"
      case Op.SizeOf(ty) =>
        s"((sn_i32)sizeof(${ctype(ty, "sizeof")}))"
      case Op.AlignmentOf(ty) =>
        s"((sn_i32)__alignof__(${ctype(ty, "alignmentof")}))"
      case _ => fail(s"operation ${op.show}")
    }

    def transfer(next: nir.Next): String = next match {
      case Next.Label(id, args) =>
        val target =
          labelById.getOrElse(id, fail(s"unknown control-flow label ${id.id}"))
        if (args.length != target.params.length)
          fail(s"control-flow argument count for label ${id.id}")
        val goto = s"goto sn_label_${id.id};"
        val representedArgs = target.params.zip(args).zipWithIndex.filter {
          case ((param, _), _) =>
            param.ty != Type.Unit && param.ty != Type.Nothing
        }
        if (representedArgs.isEmpty) goto
        else {
          val declarations = representedArgs.map {
            case ((param, _), i) =>
              val where = s"block parameter ${param.id.id}"
              s"${ctype(param.ty, where)} sn_edge_$i;"
          }
          // NIR block arguments are simultaneous assignments. Capture every
          // source before replacing any destination, including cyclic edges.
          val captures = representedArgs.map {
            case ((_, arg), i) => s"sn_edge_$i = ${value(arg)};"
          }
          val assignments = representedArgs.map {
            case ((param, _), i) => s"${localName(param.id)} = sn_edge_$i;"
          }
          "{ " + (declarations ++ captures ++ assignments ++ Seq(goto))
            .mkString(" ") + " }"
        }
      case _ => fail(s"control-flow target ${next.show}")
    }

    def constantCase(v: nir.Val): String = v match {
      case Val.Byte(n)                       => n.toString
      case Val.Short(n)                      => n.toString
      case Val.Char(n)                       => n.toInt.toString
      case Val.Int(n)                        => n.toString
      case Val.Size(n)                       => n.toString
      case Val.Long(n) if n == Long.MinValue => "(-9223372036854775807LL - 1LL)"
      case Val.Long(n)                       => s"${n}LL"
      case Val.True                          => "1"
      case Val.False                         => "0"
      case _                                 => fail(s"switch case ${v.show}")
    }

    // LLVM keeps constant allocas in the entry block in the fixed frame.
    // A loop's stackrestore must not release these outer-scope buffers.
    val staticStackAllocations = blocksByLabel(entry.id).collect {
      case Inst.Let(id, Op.Stackalloc(ty, Val.Size(count)), Next.None)
          if count > 0 && count <= Int.MaxValue =>
        id -> (ty, count)
      case Inst.Let(id, Op.Stackalloc(ty, Val.Int(count)), Next.None)
          if count > 0 =>
        id -> (ty, count.toLong)
    }.toMap

    val stackSaveLocals = defn.insts.collect {
      case Inst.Let(
            id,
            Op.Call(_, Val.Global(g: nir.Global.Member, _), args),
            _
          ) if symbol(g) == "llvm.stacksave" && args.isEmpty =>
        id
    }.toSet
    val stackRestores = defn.insts.collect {
      case Inst.Let(
            id,
            Op.Call(_, Val.Global(g: nir.Global.Member, _), args),
            _
          ) if symbol(g) == "llvm.stackrestore" =>
        id -> args
    }
    val fixedStack = !defn.insts.exists {
      case Inst.Let(id, _: Op.Stackalloc, _) =>
        !staticStackAllocations.contains(id)
      case _ => false
    } && stackRestores.forall {
      case (_, Seq(Val.Local(id, _))) => stackSaveLocals(id)
      case _                          => false
    }
    val redundantStackRestores =
      if (fixedStack) stackRestores.map(_._1).toSet else Set.empty[nir.Local]

    val redundantStackSaves = if (fixedStack) stackSaveLocals.filter { saved =>
      defn.insts.forall {
        case Inst.Let(id, _, _) if redundantStackRestores(id) => true
        case inst => !localRefs(inst).contains(saved)
      }
    }
    else Set.empty[nir.Local]

    val args = entry.params.map(p => localName(p.id))
    val result = new StringBuilder
    def compilerBarrier(): Unit =
      result.append("  __asm__ volatile(\"\" ::: \"memory\");\n")

    val functionHead =
      s"${ctype(cSignature.ret, s"function return in ${defn.name.show}")} ${cIdentifier(defn.name)}(${if (args.isEmpty) "void"
        else
          cSignature.args
            .zip(args)
            .map { case (t, n) => s"${ctype(t, "argument")} $n" }
            .mkString(", ")})"
    result.setLength(0)
    result.append(functionHead).append(" {\n")
    val ehSuffix = cIdentifier(defn.name)
    exceptionalHandlerParams.keys.foreach { handlerId =>
      result
        .append("  void *sn_eh_ucb_")
        .append(handlerId.id)
        .append(";\n")
    }
    localTypes.foreach {
      case (local, ty) =>
        if (ty != Type.Unit && ty != Type.Nothing && !entry.params.exists(
              _.id == local
            ))
          result
            .append("  ")
            .append(ctype(ty, s"local ${local.id}"))
            .append(' ')
            .append(localName(local))
            .append(";\n")
    }
    staticStackAllocations.toSeq.sortBy(_._1.id).foreach {
      case (local, (ty, count)) =>
        result
          .append("  ")
          .append(ctype(ty, "fixed stack allocation"))
          .append(" sn_stack_")
          .append(local.id)
          .append('[')
          .append(count)
          .append("];\n")
    }
    floatingBitcastLocals.foreach {
      case (local, bitcast) =>
        result
          .append("  union { ")
          .append(bitcast.fromCType)
          .append(" from; ")
          .append(bitcast.toCType)
          .append(" to; } sn_bitcast_")
          .append(local.id)
          .append(";\n")
    }
    if (localTypes.nonEmpty) result.append('\n')

    if (handlerLabelIds.nonEmpty) {
      labels
        .filter(label => handlerLabelIds(label.id))
        .foreach { label =>
          result
            .append("  __asm__ volatile(\"\" : : \"X\"(&&sn_eh_label_")
            .append(label.id.id)
            .append(") : \"memory\");\n")
        }
    }

    emittedInstructions.foreach {
      case Inst.Label(id, _) =>
        exceptionalHandlerParams.get(id).foreach { param =>
          result
            // Keep the unwind-only capture shim separate from the NIR block:
            // ordinary CFG edges may also target a handler block and must
            // enter after this shim without interpreting r0 as a UCB.
            .append(s"sn_eh_label_${id.id}: ;\n")
            .append("  __asm__ volatile(\".Lsn_eh_landing_")
            .append(ehSuffix)
            .append('_')
            .append(id.id)
            .append(":\" ::: \"memory\");\n")
            .append("  __asm__ volatile(\"mov %0, r0\" : \"=l\"(sn_eh_ucb_")
            .append(id.id)
            .append(") : : \"memory\");\n")
            .append("  ")
            .append(localName(param))
            .append(" = scalanative_catch(sn_eh_ucb_")
            .append(id.id)
            .append(");\n")
        }
        result.append(s"sn_label_${id.id}: ;\n")
      case Inst.Let(id, _, _) if redundantStackSaves(id)        => ()
      case Inst.Let(id, _, _) if xlatAddressLocals.contains(id) =>
        result
          .append("  ")
          .append(localName(id))
          .append(" = ((sn_i32)&")
          .append(xlatAddressLocals(id))
          .append(");\n")
      case Inst.Let(id, _, _) if scalarExpressions.contains(id) => ()
      case Inst.Let(id, op, _)                                  =>
        val ehHandler = exceptionalOperationHandlers.get(id)
        ehHandler.foreach { handlerId =>
          result
            .append("  __asm__ volatile(\".Lsn_eh_op_")
            .append(ehSuffix)
            .append('_')
            .append(id.id)
            .append("_handler_")
            .append(handlerId.id)
            .append("_start:\" ::: \"memory\");\n")
        }
        if (op.resty == Type.Unit || op.resty == Type.Nothing) {
          op match {
            case Op.Call(_, Val.Global(g: nir.Global.Member, _), args)
                if symbol(g) == "__xlat_protect_stack" =>
              val words = cSignature.args.map {
                case Type.Long | Type.Double => 2
                case _: Type.AggregateKind   =>
                  fail("protected stack with aggregate arguments")
                case _ => 1
              }.sum
              if (words > 4 || cSignature.ret.isInstanceOf[Type.AggregateKind])
                fail("protected stack with incoming stack arguments")
              val bytes = args match {
                case Seq(Val.Int(n)) if n > 0 && n <= 4096 && n % 4 == 0 => n
                case _                                                   =>
                  fail(
                    "protected stack requires a positive aligned constant up to 4096"
                  )
              }
              result.append(
                s"  __asm__ volatile(\"@sn_xlat_protect_stack $bytes\" ::: \"memory\");\n"
              )
            case Op.Call(ty, Val.Global(g: nir.Global.Member, _), args)
                if xlatVolatileStoreType(g).isDefined =>
              if (ty.ret != Type.Unit || args.length != 2)
                fail(
                  s"${symbol(g)} expects an address and a value, returning Unit"
                )
              val storedType = xlatVolatileStoreType(g).get
              result
                .append("  *((volatile ")
                .append(storedType)
                .append(" *)(")
                .append(value(args.head))
                .append(")) = ((")
                .append(storedType)
                .append(")(")
                .append(value(args(1)))
                .append("));\n")
            case Op.Store(ty, ptr, stored, None) =>
              result
                .append("  *(")
                .append(ctype(ty, "store type"))
                .append(" *) (")
                .append(value(ptr))
                .append(") = ")
                .append(value(stored))
                .append(";\n")
            case Op.Store(ty, ptr, stored, Some(order)) =>
              if (!atomicScalar(ty))
                fail(s"atomic store of unsupported type ${ty.show}")
              order match {
                case nir.MemoryOrder.Acquire | nir.MemoryOrder.AcqRel =>
                  fail(s"invalid atomic store order ${order.show}")
                case nir.MemoryOrder.Release | nir.MemoryOrder.SeqCst =>
                  compilerBarrier()
                case _ => ()
              }
              result
                .append("  *(volatile ")
                .append(ctype(ty, "atomic store type"))
                .append(" *)(")
                .append(value(ptr))
                .append(") = ")
                .append(value(stored))
                .append(";\n")
              if (order == nir.MemoryOrder.SeqCst) compilerBarrier()
            case Op.Fence(_) =>
              result.append("  __asm__ volatile(\"\" ::: \"memory\");\n")
            case _: Op.Call if redundantStackRestores(id) => ()
            case Op.Call(_, Val.Global(g: nir.Global.Member, _), args)
                if symbol(g) == "llvm.stackrestore" =>
              if (args.length != 1 || !pointerType(args.head.ty))
                fail("llvm.stackrestore expects one pointer")
              result
                .append("  __asm__ volatile(\"mov sp, %0\" : : \"r\"(")
                .append(value(args.head))
                .append(") : \"memory\");\n")
            case Op.Call(_, Val.Global(g: nir.Global.Member, _), args)
                if isXlatCompilerFence(g) =>
              if (args.length != 1)
                fail("xlat compiler fence expects one dependency")
              result
                .append("  __asm__ volatile(\"\" : : \"r\"(")
                .append(value(args.head))
                .append(") : \"memory\");\n")
            case Op.Call(_, Val.Global(g: nir.Global.Member, _), args) =>
              result
                .append("  ")
                .append(cIdentifier(g))
                .append('(')
                .append(args.map(value).mkString(", "))
                .append(");\n")
            case call: Op.Call =>
              result.append("  ").append(expression(call)).append(";\n")
            case _ => fail(s"Unit operation ${op.show}")
          }
        } else {
          op match {
            case _: Op.Stackalloc if staticStackAllocations.contains(id) =>
              result
                .append("  ")
                .append(localName(id))
                .append(" = ((void *)sn_stack_")
                .append(id.id)
                .append(");\n")
            case Op.Call(ty, Val.Global(g: nir.Global.Member, _), args)
                if isXlatStackPosition(g) =>
              if (args.nonEmpty || ty.ret != Type.Int)
                fail(s"${symbol(g)} expects no arguments and an Int result")
              val instruction =
                if (symbol(g) == "__xlat_frameaddr") "@sn_xlat_frameaddr %0"
                else "@sn_xlat_stackptr %0"
              result
                .append("  __asm__ volatile(\"")
                .append(instruction)
                .append("\" : \"=l\"(")
                .append(localName(id))
                .append(") : : \"memory\", \"cc\");\n")
            case Op.Call(ty, Val.Global(g: nir.Global.Member, _), args)
                if symbol(g) == "llvm.stacksave" =>
              if (args.nonEmpty || !pointerType(ty.ret))
                fail("llvm.stacksave expects no arguments and a pointer result")
              result
                .append("  __asm__ volatile(\"@sn_xlat_stackptr %0\" : \"=l\"(")
                .append(localName(id))
                .append(") : : \"memory\");\n")
            case Op.Load(ty, ptr, Some(order)) =>
              if (!atomicScalar(ty))
                fail(s"atomic load of unsupported type ${ty.show}")
              order match {
                case nir.MemoryOrder.Release | nir.MemoryOrder.AcqRel =>
                  fail(s"invalid atomic load order ${order.show}")
                case nir.MemoryOrder.SeqCst => compilerBarrier()
                case _                      => ()
              }
              result
                .append("  ")
                .append(localName(id))
                .append(
                  if (order == nir.MemoryOrder.Unordered) " = *("
                  else " = *(volatile "
                )
                .append(ctype(ty, "atomic load type"))
                .append(" *)(")
                .append(value(ptr))
                .append(");\n")
              order match {
                case nir.MemoryOrder.Acquire | nir.MemoryOrder.SeqCst =>
                  compilerBarrier()
                case _ => ()
              }
            case Op.Conv(Conv.Bitcast, ty, from)
                if floatingBitcast(from.ty, ty).isDefined =>
              val temporary = s"sn_bitcast_${id.id}"
              result
                .append("  ")
                .append(temporary)
                .append(".from = ")
                .append(value(from))
                .append(";\n  ")
                .append(localName(id))
                .append(" = ")
                .append(temporary)
                .append(".to;\n")
            case Op.Insert(aggregate, stored, indexes) =>
              if (indexes.isEmpty) fail("empty aggregate insertion path")
              result
                .append("  ")
                .append(localName(id))
                .append(" = ")
                .append(value(aggregate))
                .append(";\n  ")
                .append(aggregateAccess(localName(id), aggregate.ty, indexes))
                .append(" = ")
                .append(value(stored))
                .append(";\n")
            case _ =>
              result
                .append("  ")
                .append(localName(id))
                .append(" = ")
                .append(expression(op))
                .append(";\n")
          }
        }
        ehHandler.foreach { handlerId =>
          result
            .append("  __asm__ volatile(\".Lsn_eh_op_")
            .append(ehSuffix)
            .append('_')
            .append(id.id)
            .append("_handler_")
            .append(handlerId.id)
            .append("_end:\" ::: \"memory\");\n")
        }
      case Inst.Ret(v) =>
        result.append("  return")
        if (v != Val.Unit && v.ty != Type.Nothing)
          result.append(' ').append(value(v))
        result.append(";\n")
      case Inst.Jump(next) =>
        result.append("  ").append(transfer(next)).append('\n')
      case Inst.If(cond, thenp, elsep) =>
        result
          .append("  if (")
          .append(value(cond))
          .append(") ")
          .append(transfer(thenp))
          .append(" else ")
          .append(transfer(elsep))
          .append('\n')
      case Inst.Switch(v, default, cases) =>
        result.append("  switch (").append(value(v)).append(") {\n")
        cases.foreach {
          case Next.Case(caseValue, next) =>
            result
              .append("    case ")
              .append(constantCase(caseValue))
              .append(": ")
              .append(transfer(next))
              .append('\n')
          case _ => fail("non-case switch successor")
        }
        result.append("    default: ").append(transfer(default)).append('\n')
        result.append("  }\n")
      case Inst.Unreachable(unwind) =>
        if (unwind != Next.None)
          fail(s"unreachable with unwind target ${unwind.show}")
        result.append("  __asm__ volatile(\"1: b 1b\");\n")
      case Inst.Throw(_, _)         => fail("throw instruction")
      case Inst.LinktimeIf(_, _, _) => fail("link-time branch")
    }
    result.append("}\n")
    result.toString
  }
}

private object AgbccAbiTarget {
  def validate(targetTriple: Option[String]): Unit = {
    val target = targetTriple.map(_.toLowerCase).getOrElse("")
    if (target != "armv4t-none-eabi" && target != "thumbv4t-none-eabi")
      throw new BuildException(
        s"agbcc code generation requires armv4t-none-eabi or thumbv4t-none-eabi, got '${targetTriple.getOrElse("<host>")}'"
      )
  }
}
