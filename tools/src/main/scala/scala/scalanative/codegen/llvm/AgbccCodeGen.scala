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
            !defn.attrs.isAbstract && !defn.attrs.isStub => defn
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
    val declarationGlobals = mutable.LinkedHashMap.empty[String, nir.Global.Member]
    val definitions = mutable.ArrayBuffer.empty[nir.Defn.Define]
    val dataTypes = allDefns.collect {
      case d: nir.Defn.Var   => d.name -> d.ty
      case d: nir.Defn.Const => d.name -> d.ty
    }.toMap
    val dataDefinitions = dataTypes.keySet
    val referencedData = mutable.LinkedHashSet.empty[nir.Global.Member]

    def symbol(global: nir.Global): String = {
      val name = global match {
        case nir.Global.Member(_, sig) if sig.isExtern =>
          val nir.Sig.Extern(id) = sig.unmangled: @unchecked
          id
        case _: nir.Global.Member => "_S" + global.mangle
        case _ => throw new BuildException(s"agbcc backend only emits methods: $global")
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
          case c     => c.toString
        }
        s" __asm__(\"$escaped\")"
      }
    }

    def addSignature(name: nir.Global.Member, ty: nir.Type.Function): Unit = {
      val id = symbol(name)
      declarations.get(id) match {
        case Some(existing) if existing != ty =>
          throw new BuildException(s"conflicting agbcc signatures for $id: $existing and $ty")
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

    defnLoop(defns, addSignature, definitions, unsupported)
    definitions.foreach { defn =>
      defn.insts.foreach {
        case nir.Inst.Let(_, nir.Op.Call(ty, nir.Val.Global(g, _), args), _) =>
          g match {
            case member: nir.Global.Member => addSignature(member, ty)
            case _                         => unsupported(defn, s"call target $g")
          }
          args.foreach {
            noteData
          }
        case nir.Inst.Let(_, nir.Op.Load(_, ptr, _), _) => noteData(ptr)
        case nir.Inst.Let(_, nir.Op.Store(_, ptr, value, _), _) =>
          noteData(ptr)
          noteData(value)
        case nir.Inst.Let(
              _,
              nir.Op.Copy(nir.Val.Global(g: nir.Global.Member, _)),
              _
            ) if dataDefinitions(g) =>
          referencedData += g
        case _ =>
      }
    }

    val out = new StringBuilder
    out.append("typedef signed char sn_i8;\n")
    out.append("typedef unsigned char sn_u8;\n")
    out.append("typedef signed short sn_i16;\n")
    out.append("typedef unsigned short sn_u16;\n")
    out.append("typedef signed int sn_i32;\n")
    out.append("typedef unsigned int sn_u32;\n")
    out.append("typedef signed long long sn_i64;\n")
    out.append("typedef unsigned long long sn_u64;\n\n")

    def ctype(ty: nir.Type, where: String): String = ty match {
      case nir.Type.Unit  => "void"
      case nir.Type.Bool  => "sn_u8"
      case nir.Type.Byte  => "sn_i8"
      case nir.Type.Char  => "sn_u16"
      case nir.Type.Short => "sn_i16"
      case nir.Type.Int => "sn_i32"
      case nir.Type.Size => "sn_u32"
      case nir.Type.Long => "sn_i64"
      case nir.Type.Ptr | nir.Type.Null => "void *"
      case _: nir.Type.RefKind          => "void *"
      case _ =>
        throw new BuildException(s"agbcc backend does not support type ${ty.show} in $where")
    }

    declarations.toSeq.sortBy(_._1).foreach { case (name, ty) =>
      val args = ty.args.map(t => ctype(t, "function declaration argument"))
      if (args.contains("void"))
        throw new BuildException("agbcc backend does not support Unit function arguments")
      out.append("extern ").append(ctype(ty.ret, "function return")).append(' ')
        .append(cIdentifier(declarationGlobals(name))).append('(')
        .append(if (args.isEmpty) "void" else args.mkString(", "))
        .append(')').append(asmAlias(declarationGlobals(name))).append(";\n")
    }
    referencedData.toSeq.map(g => symbol(g) -> g).sortBy(_._1).foreach {
      case (_, global) =>
        out.append("extern ").append(ctype(dataTypes(global), "global data"))
          .append(' ').append(cIdentifier(global)).append(asmAlias(global)).append(";\n")
    }
    if (declarations.nonEmpty) out.append('\n')

    definitions.sortBy(defn => symbol(defn.name)).foreach { defn =>
      out.append(emitDefinition(defn, symbol, cIdentifier, ctype, referencedData.toSet))
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
      case _: Inst.Label => ()
      case Inst.Let(id, op, unwind) =>
        if (unwind != Next.None) fail(s"exceptional operation ${op.show}")
        bind(id, op.resty)
      case _ =>
    }

    def localName(local: nir.Local): String = s"sn_l${local.id}"

    def value(v: nir.Val): String = v match {
      case Val.Local(id, ty) =>
        if (!localTypes.get(id).exists(_ == ty)) fail(s"unknown or mistyped local ${id.id}")
        localName(id)
      case Val.True       => "1"
      case Val.False      => "0"
      case Val.Null       => "((void *)0)"
      case Val.Zero(Type.Ptr) => "((void *)0)"
      case Val.Zero(_)    => "0"
      case Val.Byte(n)    => n.toString
      case Val.Short(n)   => n.toString
      case Val.Char(n)    => n.toInt.toString
      case Val.Int(n) if n == Int.MinValue => "(-2147483647 - 1)"
      case Val.Int(n)     => n.toString
      case Val.Size(n) if n == Int.MinValue => "(-2147483647 - 1)"
      case Val.Size(n)    => n.toString
      case Val.Long(n) if n == Long.MinValue => "(-9223372036854775807LL - 1LL)"
      case Val.Long(n)    => s"${n}LL"
      case Val.Global(g: nir.Global.Member, _) if dataSymbols(g) =>
        s"((void *)&${cIdentifier(g)})"
      case _ => fail(s"value ${v.show}")
    }

    def integerType(ty: nir.Type): Boolean = ty match {
      case Type.Bool | Type.Byte | Type.Short | Type.Char | Type.Int | Type.Size | Type.Long => true
      case _ => false
    }

    def unsignedValue(v: String, ty: nir.Type, unsignedC: String): String =
      ty match {
        case Type.Byte => s"(($unsignedC)(sn_u8)($v))"
        case Type.Short | Type.Char => s"(($unsignedC)(sn_u16)($v))"
        case _ => s"($unsignedC)($v)"
      }

    def expression(op: nir.Op): String = op match {
      case Op.Copy(v) => value(v)
      case Op.Load(ty, ptr, None) =>
        s"(*(${ctype(ty, "load type")} *)(${value(ptr)}))"
      case Op.Load(_, _, Some(_)) => fail("atomic load")
      case Op.Bin(bin, ty, left, right) if integerType(ty) =>
        val l = value(left)
        val r = value(right)
        val unsigned = ty == Type.Char || ty == Type.Size
        val wide = ty == Type.Long
        val unsignedC = if (wide) "sn_u64" else "sn_u32"
        val signedC = if (wide) "sn_i64" else "sn_i32"
        bin match {
          case Bin.Iadd | Bin.Isub | Bin.Imul =>
            val operator = bin match {
              case Bin.Iadd => "+"
              case Bin.Isub => "-"
              case _         => "*"
            }
            s"((${ctype(ty, "integer operation")})(($unsignedC)($l) $operator ($unsignedC)($r)))"
          case Bin.Sdiv => s"(($signedC)($l) / ($signedC)($r))"
          case Bin.Udiv =>
            s"(${unsignedValue(l, ty, unsignedC)} / ${unsignedValue(r, ty, unsignedC)})"
          case Bin.Srem => s"(($signedC)($l) % ($signedC)($r))"
          case Bin.Urem =>
            s"(${unsignedValue(l, ty, unsignedC)} % ${unsignedValue(r, ty, unsignedC)})"
          case Bin.And => s"($l & $r)"
          case Bin.Or  => s"($l | $r)"
          case Bin.Xor => s"($l ^ $r)"
          case Bin.Shl => s"(($unsignedC)($l) << ($r))"
          case Bin.Lshr => s"(($unsignedC)($l) >> ($r))"
          case Bin.Ashr if !unsigned => s"(($signedC)($l) >> ($r))"
          case _ => fail(s"binary operation ${bin.show}")
        }
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
          case _ => fail(s"comparison ${comp.show}")
        }
        val cast = if (isUnsigned || ty == Type.Char || ty == Type.Size) {
          if (ty == Type.Long) "sn_u64" else "sn_u32"
        } else if (ty == Type.Long) "sn_i64"
        else "sn_i32"
        s"((sn_u8)((($cast)(${value(left)})) $operator (($cast)(${value(right)}))))"
      case Op.Conv(conv, ty, v) if integerType(ty) || ty == Type.Ptr =>
        conv match {
          case Conv.Trunc | Conv.Zext | Conv.Sext | Conv.Bitcast |
              Conv.Ptrtoint | Conv.Inttoptr | Conv.ZSizeCast | Conv.SSizeCast =>
            s"((${ctype(ty, "conversion")})(${value(v)}))"
          case _ => fail(s"conversion ${conv.show}")
        }
      case Op.Call(_, Val.Global(g: nir.Global.Member, _), args) =>
        s"${cIdentifier(g)}(${args.map(value).mkString(", ")})"
      case Op.SizeOf(ty) =>
        s"((sn_i32)sizeof(${ctype(ty, "sizeof")}))"
      case _ => fail(s"operation ${op.show}")
    }

    val labelById = labels.map(label => label.id -> label).toMap
    def transfer(next: nir.Next): String = next match {
      case Next.Label(id, args) =>
        val target = labelById.getOrElse(id, fail(s"unknown control-flow label ${id.id}"))
        if (args.length != target.params.length)
          fail(s"control-flow argument count for label ${id.id}")
        val goto = s"goto sn_label_${id.id};"
        if (args.isEmpty) goto
        else {
          val declarations = target.params.zipWithIndex.map { case (param, i) =>
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
      case Val.Byte(n)  => n.toString
      case Val.Short(n) => n.toString
      case Val.Char(n)  => n.toInt.toString
      case Val.Int(n)   => n.toString
      case Val.Size(n)  => n.toString
      case Val.Long(n) if n == Long.MinValue => "(-9223372036854775807LL - 1LL)"
      case Val.Long(n)  => s"${n}LL"
      case Val.True     => "1"
      case Val.False    => "0"
      case _            => fail(s"switch case ${v.show}")
    }

    val args = entry.params.map(p => localName(p.id))
    val result = new StringBuilder
    val functionHead =
      s"${ctype(defn.ty.ret, "function return")} ${symbol(defn.name)}(${if (args.isEmpty) "void" else defn.ty.args.zip(args).map { case (t, n) => s"${ctype(t, "argument")} $n" }.mkString(", ")})"
    result.setLength(0)
    result.append(functionHead).append(" {\n")
    localTypes.foreach { case (local, ty) =>
      if (ty != Type.Unit && !entry.params.exists(_.id == local))
        result.append("  ").append(ctype(ty, s"local ${local.id}")).append(' ')
          .append(localName(local)).append(";\n")
    }
    if (localTypes.nonEmpty) result.append('\n')

    defn.insts.foreach {
      case Inst.Label(id, _) =>
        result.append(s"sn_label_${id.id}: ;\n")
      case Inst.Let(id, op, _) =>
        if (op.resty == Type.Unit) {
          op match {
            case Op.Store(ty, ptr, stored, None) =>
              result.append("  *(").append(ctype(ty, "store type")).append(" *) (")
                .append(value(ptr)).append(") = ").append(value(stored)).append(";\n")
            case Op.Store(_, _, _, Some(_)) => fail("atomic store")
            case Op.Call(_, Val.Global(g: nir.Global.Member, _), args) =>
              result.append("  ").append(cIdentifier(g)).append('(')
                .append(args.map(value).mkString(", ")).append(");\n")
            case _ => fail(s"Unit operation ${op.show}")
          }
        } else {
          result.append("  ").append(localName(id)).append(" = ")
            .append(expression(op)).append(";\n")
        }
      case Inst.Ret(v) =>
        result.append("  return")
        if (v != Val.Unit) result.append(' ').append(value(v))
        result.append(";\n")
      case Inst.Jump(next) =>
        result.append("  ").append(transfer(next)).append('\n')
      case Inst.If(cond, thenp, elsep) =>
        result.append("  if (").append(value(cond)).append(") ")
          .append(transfer(thenp)).append(" else ").append(transfer(elsep)).append('\n')
      case Inst.Switch(v, default, cases) =>
        result.append("  switch (").append(value(v)).append(") {\n")
        cases.foreach {
          case Next.Case(caseValue, next) =>
            result.append("    case ").append(constantCase(caseValue))
              .append(": ").append(transfer(next)).append('\n')
          case _ => fail("non-case switch successor")
        }
        result.append("    default: ").append(transfer(default)).append('\n')
        result.append("  }\n")
      case Inst.Unreachable(_) => fail("unreachable instruction")
      case Inst.Throw(_, _) => fail("throw instruction")
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
