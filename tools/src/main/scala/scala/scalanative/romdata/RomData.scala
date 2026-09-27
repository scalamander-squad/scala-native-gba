package scala.scalanative
package romdata

import scala.collection.mutable

/** One object of a link-time evaluated object graph.
 *
 *  Either a regular object (`fields`, keyed by field name, values are
 *  canonical `nir.Val`s, `nir.Val.String` literals or `nir.Val.Virtual(key)`
 *  references to other objects of the graph) or an array (`arrayElem` is the
 *  element type and `elems` the values).
 */
final class RomObject(
    val key: Long,
    val cls: nir.Global.Top,
    val arrayElem: Option[nir.Type]
) {
  val fields = mutable.LinkedHashMap.empty[nir.Global.Member, nir.Val]
  var elems: Array[nir.Val] = null
  var moduleOf: Option[nir.Global.Top] = None
  /** The instance of a module whose initialiser was evaluated but that must
   *  stay a run-time module (mutable state): its scalar fields are known, but
   *  ROM graphs may not reference it.
   */
  var runtimeModule: Boolean = false
  def isArray: Boolean = arrayElem.isDefined
}

/** The object graphs produced by [[StaticInit]]: the instance of every module
 *  whose constructor was fully evaluated at link time, and every object those
 *  instances reach. Consumed by codegen (`Generate.genRomData`), which emits
 *  the graph as `Defn.Const` globals, and by `Class.isConstantModule`, which
 *  makes `Op.Module` lower to the constant's address.
 */
final class RomData {
  val objects = mutable.LinkedHashMap.empty[Long, RomObject]
  val modules = mutable.LinkedHashMap.empty[nir.Global.Top, Long]
  def isRomModule(name: nir.Global.Top): Boolean = modules.contains(name)
  def isEmpty: Boolean = modules.isEmpty

  /** Every object reachable from a ROM module instance, in a deterministic
   *  order, paired with the module through which it was first reached.
   */
  /** The global that codegen emits for each object: `<module>.instance` for module instances,
   *  `<first owner module>.rom<key>` otherwise.
   */
  lazy val globalName: Map[Long, nir.Global.Member] =
    reachable.map {
      case (obj, owner) =>
        obj.key -> (obj.moduleOf match {
          case Some(mod) => mod.member(nir.Sig.Generated("instance"))
          case None      => owner.member(nir.Sig.Generated("rom" + obj.key))
        })
    }.toMap
  lazy val byGlobal: Map[nir.Global, Long] = globalName.map(_.swap)

  /** The static type of a reference to the constant of `key` (exact, non-null). */
  def refType(key: Long): nir.Type = {
    val obj = objects(key)
    obj.arrayElem match {
      case Some(elem) => nir.Type.Array(elem, nullable = false)
      case None       => nir.Type.Ref(obj.cls, exact = true, nullable = false)
    }
  }

  lazy val reachable: Seq[(RomObject, nir.Global.Top)] = {
    val out = mutable.ArrayBuffer.empty[(RomObject, nir.Global.Top)]
    val seen = mutable.HashSet.empty[Long]
    def visit(key: Long, owner: nir.Global.Top): Unit =
      if (seen.add(key)) {
        val obj = objects(key)
        out += ((obj, owner))
        val refs = if (obj.isArray) obj.elems.toSeq else obj.fields.values.toSeq
        refs.foreach {
          case nir.Val.Virtual(k) => visit(k, owner)
          case _                  => ()
        }
      }
    modules.foreach { case (mod, key) => visit(key, mod) }
    out.toSeq
  }
}

object RomData {
  val empty: RomData = new RomData

  /** The registry of the current link (set by [[StaticInit.run]]): the reachability analysis re-run after
   *  Interflow must accept references to ROM constants, which only codegen defines.
   */
  @volatile var current: RomData = empty
}
