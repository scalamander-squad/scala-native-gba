package scala.scalanative.build

/** Resolve xlat's entry-SP-minus-eight contract from the actual agbcc prologue.
 *  The marker is an inline-assembly output, so its allocated register and
 *  clobbers are visible to GCC without changing the function's stack layout.
 */
private[scalanative] object AgbccStackFrames {
  private val Marker = "@sn_xlat_frameaddr\\s+(r[0-7])".r
  private val StackMarker = "@sn_xlat_stackptr\\s+(r[0-7])".r
  private val Function = "([A-Za-z_.$][\\w.$]*):(?:\\s*@.*)?".r
  private val Push = "(?:push|stmfd\\s+sp!|stmdb\\s+sp!)\\s*\\{([^}]+)\\}".r
  private val Adjust =
    "(add|sub)\\s+sp,\\s*sp,\\s*#(-?(?:0x[0-9a-fA-F]+|[0-9]+))".r
  private val RegisterAdjust = "(add|sub)\\s+sp,\\s*sp,\\s*(r[0-9]+)".r
  private val Frame =
    "(mov|add|sub)\\s+(r7|r11|fp),\\s*sp(?:,\\s*#(-?(?:0x[0-9a-fA-F]+|[0-9]+)))?".r
  private val HighMove = "mov\\s+r[0-7],\\s*(?:r(?:8|9|10|11|12)|sl|fp|ip)".r
  private val LiteralLoad = "ldr\\s+(r[0-9]+),\\s*(\\.L[\\w.$]+)".r

  private def number(value: String): Int = java.lang.Long.decode(value).toInt
  private def registers(value: String): Int = value
    .split(',')
    .map(_.trim)
    .map {
      case range if range.matches("r[0-9]+-r[0-9]+") =>
        val ends = range.split('-').map(_.drop(1).toInt)
        if (ends(1) < ends(0))
          throw new BuildException("reversed agbcc register range")
        ends(1) - ends(0) + 1
      case register if register.matches("r(?:[0-9]|1[0-5])|lr|pc|sl|fp|ip") => 1
      case register                                                         =>
        throw new BuildException(s"unknown agbcc saved register $register")
    }
    .sum

  /** Keep original locals above GCC's save area. Only register-argument
   *  functions are admitted by the emitter; outgoing stack arguments are safe.
   */
  private def protect(source: String): String = {
    if (!source.contains("@sn_xlat_protect_stack")) return source
    val function = "(?s)(\\.thumb_func\\s+[^\\n]+:\\n)(.*?)(\\.size[^\\n]*)".r
    val marker = "@sn_xlat_protect_stack\\s+([0-9]+)".r
    val transformed = function.replaceAllIn(
      source,
      m => {
        val body = m.group(2).split("\n", -1)
        val sizes = body.flatMap(line =>
          line.trim match {
            case marker(n) => Some(n.toInt)
            case text if text.contains("@sn_xlat_protect_stack") =>
              throw new BuildException("malformed protected-stack marker")
            case _ => None
          }
        )
        if (sizes.isEmpty) java.util.regex.Matcher.quoteReplacement(m.matched)
        else {
          val bytes = sizes.max
          if (bytes <= 0 || bytes > 4096 || bytes % 4 != 0)
            throw new BuildException("invalid protected-stack extent")
          def adjust(op: String): String = {
            val chunks = scala.collection.mutable.ArrayBuffer.empty[String]
            var remaining = bytes
            while (remaining > 0) {
              val chunk = math.min(remaining, 508)
              chunks += s"\t$op\tsp, sp, #$chunk"
              remaining -= chunk
            }
            chunks.mkString("\n") + "\n"
          }
          var returns = 0
          val result = new StringBuilder(m.group(1)).append(adjust("sub"))
          body.zipWithIndex.foreach {
            case (line, index) =>
              line.trim match {
                case marker(_)                                   => ()
                case text if text.matches("bx\\s+(?:lr|r[0-7])") =>
                  val reg = text.split("\\s+").last
                  val previous = body
                    .take(index)
                    .reverseIterator
                    .map(_.trim)
                    .find(t => t.nonEmpty && !t.startsWith("@"))
                  if (reg != "lr" && !previous
                        .exists(_.matches("pop\\s*\\{\\s*" + reg + "\\s*\\}")))
                    throw new BuildException(
                      "protected stack has an indirect branch"
                    )
                  returns += 1
                  result.append(adjust("add")).append(line).append('\n')
                case text
                    if text.matches(
                      "(?:pop|ldm[^ ]*)\\s+.*\\b(?:pc|r15)\\b.*|(?:mov|add|sub)\\s+(?:pc|r15),.*|b\\s+(?!\\.L)[^ ].*"
                    ) &&
                      !text.startsWith("@") =>
                  throw new BuildException(
                    "protected stack has an unsupported PC instruction"
                  )
                case _ => result.append(line).append('\n')
              }
          }
          if (returns == 0)
            throw new BuildException("protected stack has no return")
          result.append(m.group(3))
          java.util.regex.Matcher.quoteReplacement(result.toString)
        }
      }
    )
    if (transformed.contains("@sn_xlat_protect_stack"))
      throw new BuildException(
        "protected-stack marker outside an agbcc function"
      )
    transformed
  }

  def rewrite(source: String): String = {
    val protectedSource = protect(source)
    if (!protectedSource.contains("@sn_xlat_frameaddr") && !protectedSource
          .contains(
            "@sn_xlat_stackptr"
          )) return protectedSource
    val lines = protectedSource.split("\n", -1)
    val output = scala.collection.mutable.ArrayBuffer.empty[String]
    var cursor = 0
    while (cursor < lines.length) {
      if (lines(cursor).trim != ".thumb_func") {
        if (lines(cursor).contains("@sn_xlat_frameaddr") || lines(cursor)
              .contains("@sn_xlat_stackptr"))
          throw new BuildException(
            "entry-stack marker outside an agbcc function"
          )
        output += lines(cursor)
        cursor += 1
      } else {
        val start = cursor
        cursor += 1
        while (cursor < lines.length && !Function.pattern
              .matcher(lines(cursor).trim)
              .matches())
          cursor += 1
        if (cursor == lines.length)
          throw new BuildException("agbcc function has no label")
        val Function(symbol) = lines(cursor).trim: @unchecked
        val label = cursor
        val endPattern =
          ("\\.size\\s+" + java.util.regex.Pattern.quote(symbol) + "\\s*,.*").r
        cursor += 1
        while (cursor < lines.length && !endPattern.pattern
              .matcher(lines(cursor).trim)
              .matches())
          cursor += 1
        if (cursor == lines.length)
          throw new BuildException(s"agbcc function $symbol has no size")
        val end = cursor
        val hasMarker =
          lines.slice(label + 1, end).exists(_.contains("@sn_xlat_frameaddr"))
        if (!hasMarker) lines.slice(start, end + 1).foreach { line =>
          line.trim match {
            case StackMarker(register) =>
              output += "@ sn_xlat_stack_snapshot"
              output += s"\tmov\t$register, sp"
            case text if text.contains("@sn_xlat_stackptr") =>
              throw new BuildException(s"malformed stack snapshot in $symbol")
            case _ => output += line
          }
        }
        else {
          def fail(reason: String): Nothing = throw new BuildException(
            s"cannot reconstruct entry stack in $symbol: $reason"
          )
          def constantBefore(index: Int, register: String): Option[Int] = {
            if (index <= label + 1) None
            else {
              val immediate =
                ("mov\\s+" + register + ",\\s*#(-?(?:0x[0-9a-fA-F]+|[0-9]+))").r
              val shift =
                ("lsl\\s+" + register + ",\\s*" + register + ",\\s*#([0-9]+|0x[0-9a-fA-F]+)").r
              lines(index - 1).trim match {
                case immediate(n) => Some(number(n))
                case LiteralLoad(reg, target) if reg == register =>
                  val position = (label + 1 until end - 1).find(i =>
                    lines(i).trim == target + ":"
                  )
                  position.flatMap { i =>
                    val word = "\\.word\\s+(-?(?:0x[0-9a-fA-F]+|[0-9]+))".r
                    lines(i + 1).trim match {
                      case word(n) => Some(number(n)); case _ => None
                    }
                  }
                case shift(n) =>
                  constantBefore(index - 1, register).map(_ << number(n))
                case _ => None
              }
            }
          }
          def registerAdjustment(index: Int): Option[Int] = lines(
            index
          ).trim match {
            case RegisterAdjust(op, register) =>
              constantBefore(index, register).map(n =>
                if (op == "sub") -n else n
              )
            case _ => None
          }
          var depth = 0
          var fixedPad = 0
          var anchor: Option[(String, Int)] = None
          var prologue = label + 1
          var scanning = true
          while (prologue < end && scanning) {
            lines(prologue).trim match {
              case Marker(_)   => scanning = false
              case Push(saved) => depth += 4 * registers(saved); prologue += 1
              case Adjust(op, n)
                  if (if (op == "sub") -number(n) else number(n)) < 0 =>
                val allocation = -(if (op == "sub") -number(n) else number(n))
                depth += allocation
                fixedPad += allocation;
                prologue += 1
              case LiteralLoad(_, _)
                  if prologue + 1 < end && registerAdjustment(prologue + 1)
                    .exists(_ < 0) =>
                val allocation = -registerAdjustment(prologue + 1).get
                depth += allocation
                fixedPad += allocation; prologue += 2
              case HighMove() => prologue += 1
              // GCC terminates inline assembly with a .code directive. A
              // stack snapshot allocated to r7 is not a reserved frame pointer.
              case Frame(_, _, _)
                  if prologue + 1 < end &&
                    lines(prologue + 1).trim.matches("\\.code\\s+16") =>
                scanning = false
              case Frame(op, register, offset) =>
                if (anchor.nonEmpty || (op != "mov" && offset == null))
                  fail("invalid frame pointer")
                val displacement =
                  if (offset == null) 0
                  else number(offset) * (if (op == "sub") -1 else 1)
                anchor = Some(
                  (
                    if (register == "fp") "r11" else register,
                    depth - displacement
                  )
                )
                prologue += 1
              case text if text.isEmpty || text.startsWith("@") => prologue += 1
              case _ => scanning = false
            }
          }
          // A fixed C array can also place its address in r7. It is a usable
          // unwind anchor only if no body instruction overwrites it.
          anchor.foreach {
            case (register, _) =>
              val destination = ("([a-z]+)\\s+" + register + "(?=[,!])").r
              val readsOnly =
                Set("cmp", "cmn", "tst", "teq", "str", "strb", "strh", "push")
              val overwritten = (prologue until end).exists { i =>
                lines(i).trim match {
                  case Marker(output)      => output == register
                  case StackMarker(output) => output == register
                  case text                =>
                    destination
                      .findFirstMatchIn(text)
                      .exists(m => !readsOnly(m.group(1)))
                }
              }
              if (overwritten) anchor = None
          }
          // A reserved frame pointer remains stable through alloca and stack
          // restoration. Without it only a fixed frame is reconstructible.
          if (anchor.isEmpty) {
            for (i <- prologue until end) lines(i).trim match {
              case Push(_) =>
                fail("body saves registers without a frame pointer")
              case Adjust(op, n)
                  if (if (op == "sub") -number(n) else number(n)) < 0 =>
                fail("body allocates stack without a frame pointer")
              case RegisterAdjust(_, _)
                  if !registerAdjustment(i).contains(fixedPad) =>
                fail("dynamic stack without a frame pointer")
              case text if text.matches("mov\\s+sp,.*") =>
                fail("untracked stack restoration")
              case _ => ()
            }
          }
          val (base, distance) = anchor.getOrElse("sp" -> depth)
          if (distance < 0 || distance > 1024 * 1024)
            fail("invalid frame extent")
          for (i <- start to end) lines(i).trim match {
            case Marker(register) =>
              output += "@ sn_xlat_stack_snapshot"
              output += s"\tmov\t$register, $base"
              var adjustment = distance - 8
              while (adjustment != 0) {
                val chunk = math.min(math.abs(adjustment), 255)
                val operation = if (adjustment > 0) "add" else "sub"
                output += s"\t$operation\t$register, #$chunk"
                adjustment += (if (adjustment > 0) -chunk else chunk)
              }
            case StackMarker(register) =>
              output += "@ sn_xlat_stack_snapshot"
              output += s"\tmov\t$register, sp"
            case text
                if text.contains("@sn_xlat_frameaddr") || text.contains(
                  "@sn_xlat_stackptr"
                ) =>
              fail("malformed entry-stack marker")
            case _ => output += lines(i)
          }
        }
        cursor = end + 1
      }
    }
    output.mkString("\n")
  }
}
