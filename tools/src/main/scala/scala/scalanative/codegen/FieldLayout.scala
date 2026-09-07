package scala.scalanative
package codegen

import scalanative.linker.{Class, Field}

private[codegen] class FieldLayout(cls: Class)(implicit meta: Metadata) {

  import meta.layouts.{ArrayHeader, Object, ObjectHeader}
  import meta.platform

  def index(fld: Field) = entries.indexOf(fld) + Object.ValuesOffset
  // Proxy fields due to cyclic dependency
  def entries: Seq[Field] = entries0
  def layout: MemoryLayout = layout0

  private lazy val (entries0, layout0) = {
    val entries: Seq[Field] = {
      val base = cls.parent.fold {
        Seq.empty[Field]
      } { parent => meta.layout(parent).entries }
      base ++ cls.members.collect { case f: Field => f }
    }
    val usesCustomAlignment = entries.exists(_.attrs.align.isDefined)
    val isArray = nir.Type.isArray(cls.name)
    if (usesCustomAlignment) {
      assert(!isArray) // Only regular object can have custom alignmet
      val fields = entries.sortBy(_.attrs.align.flatMap(_.group))
      val layout = MemoryLayout.ofAlignedFields(fields)
      (fields, layout)
    } else {
      val rttiHeader = if (isArray) ArrayHeader else ObjectHeader
      // For array classes the RTTI `size` is what the GC allocators add
      // `length * stride` to (`scalanative_GC_alloc_array`), i.e. it must be the
      // offset of the first element in `{ ArrayHeader, [0 x elem] }` as laid out
      // by LLVM, not just the header size: on 32-bit ARM/RISC-V/MIPS/wasm the
      // 12-byte header is padded to 16 for `i64`/`double` elements.
      val elemPadding =
        if (isArray)
          nir.Type.fromArrayClass(cls.name).map(nir.Type.ArrayValue(_, 0)).toSeq
        else Nil
      val layout =
        MemoryLayout(rttiHeader.layout +: (entries.map(_.ty) ++ elemPadding))
      (entries, layout)
    }
  }

  val struct = nir.Type.StructValue(layout.tys.map(_.ty))
  val size = layout.size
  val referenceOffsetsValue = nir.Val.Const(
    nir.Val.ArrayValue(nir.Type.Int, layout.referenceFieldsOffsets)
  )

}
