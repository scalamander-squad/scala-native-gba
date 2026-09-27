package scala.scalanative
package codegen

import scala.collection.mutable

import scalanative.linker.Class

private[codegen] class ModuleArray(meta: Metadata) {

  /* Only modules that are initialised at run time get a slot. A constant
   * module (no fields and an empty constructor, which includes every module
   * made ROM-resident by `romdata`) is never loaded through its slot:
   * `Lower.genModuleOp` resolves it to its `<module>.instance` global and
   * `Generate` emits no loader for it. Its instance never points into the
   * GC heap (a constant module has no fields; a ROM module's graph only
   * reaches ROM objects), so the collector needs no root for it either.
   * Leaving these modules out keeps the RAM-resident `__modules` table (a
   * precise root area the GC scans on every collection) to the modules that
   * can actually hold heap references. */
  val index = mutable.Map.empty[Class, Int]
  val modules = mutable.UnrolledBuffer.empty[Class]
  meta.classes.foreach { cls =>
    if (cls.isModule && cls.allocated &&
        !cls.isConstantModule(meta.analysis)) {
      index(cls) = modules.size
      modules += cls
    }
  }
  val size: Int = modules.size
  val value: nir.Val =
    nir.Val.ArrayValue(nir.Type.Ptr, modules.toSeq.map(_ => nir.Val.Null))

}
