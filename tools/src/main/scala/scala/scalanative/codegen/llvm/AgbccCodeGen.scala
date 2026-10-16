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
  }

  def gen(defns: Seq[nir.Defn], dir: VirtualDirectory): Path = {
    val content = emit(defns)
    dir.write(Paths.get(OutputName))(_.write(content))
  }

  def selectExported(
      defns: Seq[nir.Defn],
      requested: Set[String] = Set.empty
  ): Seq[nir.Defn.Define] = {
    val exported = defns.collect {
      case defn: nir.Defn.Define
          if defn.name.sig.isExtern && defn.attrs.isExtern &&
            !defn.attrs.isAbstract && !defn.attrs.isStub =>
        defn
    }
    if (requested.nonEmpty) {
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
  }

  def gen(
      selected: Seq[nir.Defn.Define],
      allDefns: Seq[nir.Defn],
      dir: VirtualDirectory
  ): Path = {
    val content = emit(selected, allDefns)
    dir.write(Paths.get(OutputName))(_.write(content))
  }

  /** Emit one C translation unit for an already lowered NIR assembly. */
  private[llvm] def emit(defns: Seq[nir.Defn]): String = emit(defns, defns)

  private[llvm] def emit(
      defns: Seq[nir.Defn],
      allDefns: Seq[nir.Defn]
  ): String = {
    val declarations = mutable.LinkedHashMap.empty[String, nir.Type.Function]
    val declarationGlobals =
      mutable.LinkedHashMap.empty[String, nir.Global.Member]
    val definitions = mutable.ArrayBuffer.empty[nir.Defn.Define]
    val dataTypes = allDefns.collect {
      case d: nir.Defn.Var   => d.name -> d.ty
      case d: nir.Defn.Const => d.name -> d.ty
    }.toMap
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

    def symbol(global: nir.Global): String = {
      val name = global match {
        case nir.Global.Member(_, sig) if sig.isExtern =>
          val nir.Sig.Extern(id) = sig.unmangled: @unchecked
          id
        case _: nir.Global.Member => "_S" + global.mangle
        case _                    =>
          throw new BuildException(s"agbcc backend only emits methods: $global")
      }
      name
    }

    def cIdentifier(global: nir.Global.Member): String = {
      val name = symbol(global)
      if (CIdentifier.matches(name)) name
      else {
        val bytes = name.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        "sn_sym_" + bytes.map(b => f"${b & 0xff}%02x").mkString
      }
    }

    def asmAlias(global: nir.Global.Member): String = {
      val name = symbol(global)
      if (cIdentifier(global) == name) ""
      else {
        val escaped = name.flatMap {
          case '\\' => "\\\\"
          case '"'  => "\\\""
          case c    => c.toString
        }
        s" __asm__(\"$escaped\")"
      }
    }

    def addSignature(name: nir.Global.Member, ty: nir.Type.Function): Unit = {
      val id = symbol(name)
      declarations.get(id) match {
        case Some(existing) if existing != ty =>
          throw new BuildException(
            s"conflicting agbcc signatures for $id: $existing and $ty"
          )
        case _ =>
          declarations(id) = ty
          declarationGlobals(id) = name
      }
    }

    def unsupported(defn: nir.Defn, what: String): Nothing =
      throw new BuildException(
        s"agbcc backend does not support $what in ${defn.name.show} at ${defn.pos.show}"
      )

    def noteData(value: nir.Val): Unit = value match {
      case nir.Val.Global(g: nir.Global.Member, _) if dataDefinitions(g) =>
        referencedData += g
      case _ => ()
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
    definitions.foreach { defn =>
      defn.insts.foreach {
        case nir.Inst.Let(_, nir.Op.Call(ty, nir.Val.Global(g, _), args), _) =>
          g match {
            case member: nir.Global.Member if dataDefinitions(member) =>
              noteData(nir.Val.Global(member, nir.Type.Ptr))
            case member: nir.Global.Member => addSignature(member, ty)
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
    if (floatRemainderTypes.nonEmpty)
      out.append(FloatingRemainderSupport).append('\n')

    def aggregateName(ty: nir.Type.AggregateKind): String = {
      val bytes = ty.mangle.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      "sn_aggr_" + bytes.map(b => f"${b & 0xff}%02x").mkString
    }

    def ctype(ty: nir.Type, where: String): String = ty match {
      case nir.Type.Unit                     => "void"
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
          .maxOption
          .getOrElse(0)
      case nir.Type.ArrayValue(element: nir.Type.AggregateKind, _) =>
        1 + aggregateDepth(element)
      case _: nir.Type.ArrayValue => 1
    }

    aggregateTypes.toSeq.sortBy(aggregateDepth).foreach {
      case nir.Type.StructValue(fields) =>
        val members = fields.zipWithIndex.map {
          case (field, index) =>
            s"${ctype(field, "aggregate field")} sn_f$index;"
        }
        out
          .append("typedef struct { ")
          .append(members.mkString(" "))
          .append(" } ")
          .append(aggregateName(nir.Type.StructValue(fields)))
          .append(";\n")
      case nir.Type.ArrayValue(element, length) =>
        out
          .append("typedef struct { ")
          .append(ctype(element, "aggregate array element"))
          .append(" sn_values[")
          .append(length)
          .append("]; } ")
          .append(aggregateName(nir.Type.ArrayValue(element, length)))
          .append(";\n")
    }
    if (aggregateTypes.nonEmpty) out.append('\n')

    declarations.toSeq.sortBy(_._1).foreach {
      case (name, ty) =>
        val args = cFunctionArguments(
          ty,
          ctype,
          "function declaration argument"
        )
        out
          .append("extern ")
          .append(ctype(ty.ret, "function return"))
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
    if (declarations.nonEmpty) out.append('\n')

    definitions.sortBy(defn => symbol(defn.name)).foreach { defn =>
      out.append(
        emitDefinition(
          defn,
          symbol,
          cIdentifier,
          ctype,
          referencedData.toSet
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
        nir.Type.Int | nir.Type.Size | nir.Type.Ptr | nir.Type.Null |
        _: nir.Type.RefKind =>
      true
    case _ => false
  }

  private def emitDefinition(
      defn: nir.Defn.Define,
      symbol: nir.Global => String,
      cIdentifier: nir.Global.Member => String,
      ctype: (nir.Type, String) => String,
      dataSymbols: Set[nir.Global.Member]
  ): String = {
    import nir.{Bin, Comp, Conv, Inst, Next, Op, Type, Val}

    def fail(what: String): Nothing =
      throw new BuildException(
        s"agbcc backend does not support $what in ${defn.name.show} at ${defn.pos.show}"
      )

    if (defn.ty.args.exists(_ == Type.Vararg)) fail("varargs")
    if (defn.insts.isEmpty) fail("empty function body")
    val entry = defn.insts.head match {
      case label: Inst.Label => label
      case _                 => fail("function without an entry label")
    }
    if (entry.params.length != defn.ty.args.length)
      fail("entry-label parameters differing from function signature")

    val localTypes = mutable.LinkedHashMap.empty[nir.Local, nir.Type]
    def bind(local: nir.Local, ty: nir.Type): Unit = {
      if (ty != Type.Unit) ctype(ty, s"local ${local.id}")
      if (localTypes.contains(local)) fail(s"duplicate local ${local.id}")
      localTypes(local) = ty
    }
    entry.params.foreach(p => bind(p.id, p.ty))
    val labels = defn.insts.collect { case label: Inst.Label => label }
    labels.drop(1).foreach(_.params.foreach(p => bind(p.id, p.ty)))
    defn.insts.drop(1).foreach {
      case _: Inst.Label            => ()
      case Inst.Let(id, op, unwind) =>
        if (unwind != Next.None) fail(s"exceptional operation ${op.show}")
        bind(id, op.resty)
      case _ =>
    }
    val floatingBitcastLocals = defn.insts.flatMap {
      case Inst.Let(id, Op.Conv(Conv.Bitcast, to, from), _) =>
        floatingBitcast(from.ty, to).map(id -> _)
      case _ => None
    }

    def localName(local: nir.Local): String = s"sn_l${local.id}"

    def value(v: nir.Val): String = v match {
      case Val.Local(id, ty) =>
        if (!localTypes.get(id).exists(_ == ty))
          fail(s"unknown or mistyped local ${id.id}")
        localName(id)
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
        fail("non-finite float constant")
      case Val.Float(n) => s"${n.toString}f"
      case Val.Double(n)
          if java.lang.Double.isNaN(n) || java.lang.Double.isInfinite(n) =>
        fail("non-finite double constant")
      case Val.Double(n)                                         => n.toString
      case Val.Global(g: nir.Global.Member, _) if dataSymbols(g) =>
        s"((void *)&${cIdentifier(g)})"
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
      access
    }

    def pointerType(ty: nir.Type): Boolean = ty match {
      case Type.Ptr | Type.Null | _: Type.Ref | _: Type.Array => true
      case _                                                  => false
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
                  aggregateDeclaration(field, s"sn_f$i") + ";"
              }
              s"struct { ${members.mkString(" ")} } $name"
            case Type.ArrayValue(element, length) =>
              val declarator =
                if (name == "*") s"(*)[$length]" else s"$name[$length]"
              aggregateDeclaration(element, declarator)
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
        val (operator, isUnsigned) = comp match {
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
              Conv.Fptrunc | Conv.Fpext | Conv.Fptoui | Conv.Fptosi |
              Conv.Uitofp | Conv.Sitofp =>
            s"((${ctype(ty, "conversion")})(${value(v)}))"
        }
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

    val labelById = labels.map(label => label.id -> label).toMap
    def transfer(next: nir.Next): String = next match {
      case Next.Label(id, args) =>
        val target =
          labelById.getOrElse(id, fail(s"unknown control-flow label ${id.id}"))
        if (args.length != target.params.length)
          fail(s"control-flow argument count for label ${id.id}")
        val goto = s"goto sn_label_${id.id};"
        if (args.isEmpty) goto
        else {
          val declarations = target.params.zipWithIndex.map {
            case (param, i) =>
              val where = s"block parameter ${param.id.id}"
              s"${ctype(param.ty, where)} sn_edge_$i;"
          }
          val assignments = target.params.zip(args).zipWithIndex.map {
            case ((param, arg), i) =>
              s"sn_edge_$i = ${value(arg)}; ${localName(param.id)} = sn_edge_$i;"
          }
          "{ " + (declarations ++ assignments ++ Seq(goto)).mkString(" ") + " }"
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

    val args = entry.params.map(p => localName(p.id))
    val result = new StringBuilder
    def compilerBarrier(): Unit =
      result.append("  __asm__ volatile(\"\" ::: \"memory\");\n")

    val functionHead =
      s"${ctype(defn.ty.ret, "function return")} ${symbol(defn.name)}(${if (args.isEmpty) "void"
        else
          defn.ty.args
            .zip(args)
            .map { case (t, n) => s"${ctype(t, "argument")} $n" }
            .mkString(", ")})"
    result.setLength(0)
    result.append(functionHead).append(" {\n")
    localTypes.foreach {
      case (local, ty) =>
        if (ty != Type.Unit && !entry.params.exists(_.id == local))
          result
            .append("  ")
            .append(ctype(ty, s"local ${local.id}"))
            .append(' ')
            .append(localName(local))
            .append(";\n")
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

    defn.insts.foreach {
      case Inst.Label(id, _) =>
        result.append(s"sn_label_${id.id}: ;\n")
      case Inst.Let(id, op, _) =>
        if (op.resty == Type.Unit) {
          op match {
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
                .append(" = *(volatile ")
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
      case Inst.Ret(v) =>
        result.append("  return")
        if (v != Val.Unit) result.append(' ').append(value(v))
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
      case Inst.Unreachable(_)      => fail("unreachable instruction")
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
