package scala.scalanative
package build

/** Opt-in APCS-GNU ABI used by the original GBA compiler.
 *
 *  This asks Clang/LLVM to use APCS-GNU call, stack, and aggregate lowering. It
 *  does not reproduce agbcc's instruction selection, register allocation, or
 *  complete object output.
 */
private[build] object AgbccAbi {
  private final val ProfileName = "Agbcc ABI profile"

  def compileFlags(
      targetTriple: Option[String],
      isEnabled: Boolean
  ): Seq[String] =
    if (!isEnabled) Nil
    else {
      val target = targetTriple.map(_.toLowerCase).getOrElse("")
      if (target != "armv4t-none-eabi" && target != "thumbv4t-none-eabi")
        throw new BuildException(
          s"$ProfileName requires armv4t-none-eabi or thumbv4t-none-eabi, got '${targetTriple.getOrElse("<host>")}'"
        )
      Seq("-mabi=apcs-gnu")
    }
}
