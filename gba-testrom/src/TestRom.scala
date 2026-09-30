// SPDX-License-Identifier: Apache-2.0
// gba-testrom: a self-checking Scala 3 program for the fork's freestanding GBA target (armv4t-none-eabi).
// Each check reports through tr_check (rt/rt.c: mGBA debug log + result block); rt.c prints the verdict.
package gbatest

import scala.scalanative.unsafe.*
import scala.scalanative.runtime.{Intrinsics, LongArray, DoubleArray}

@extern object Rt:
  def tr_check(name: CString, ok: CInt): Unit = extern
  def tr_value(name: CString, v: CInt): Unit = extern
  def tr_opaque(v: CInt): CInt = extern // identity the optimiser cannot see through

// ---- 32-bit layout: Long/Double fields after narrower ones, arrays of 8-byte elements ----
final class Mixed(val a: Int, var l: Long, val b: Byte, var d: Double, val s: Short) // var: classFieldRawPtr needs mutable fields

// ---- exceptions through polymorphic dispatch ----
abstract class Shape { def area(): Int }
final class Square(n: Int) extends Shape { def area(): Int = n * n }
final class Broken extends Shape { def area(): Int = throw new ArithmeticException("broken") }
final class TestFailure(msg: String) extends RuntimeException(msg)

// ---- link-time module evaluation: must be ROM-resident (build.sh passes romdata.strict=gbatest\.Rom.*) ----
final case class Entry(id: Int, name: String, weight: Long)
object RomTable:
  val primes: Vector[Int] = Vector(2, 3, 5, 7, 11, 13, 17, 19)
  val entries: List[Entry] = List(Entry(1, "one", 1L << 33), Entry(2, "two", -5L))
  val big: Long = 0x123456789AL
  val ratio: Double = 0.75

// ---- stable module fields holding lambdas ----
object Ops:
  val inc: Int => Int = _ + 1
  val twice: Int => Int = x => x * 2
  val both: Int => Int = inc.andThen(twice)

object TestRom:
  import Rt.*

  private def ok(name: CString, cond: => Boolean): Unit =
    val r = try (if cond then 1 else 0) catch case _: Throwable => 0
    tr_check(name, r)

  private def addr(o: Object): Int = Intrinsics.castRawPtrToInt(Intrinsics.castObjectToRawPtr(o))
  private def inRom(a: Int): Boolean = a >= 0x08000000 && a < 0x0a000000

  @noinline def thrower(i: Int): Int = if i > 0 then throw new IllegalStateException("x") else i

  @inline def risky(x: Int): Int = if x == 3 then throw new TestFailure("three") else x * 2
  @noinline def underTry(x: Int): Int = try risky(x) + 1 catch case _: TestFailure => -1
  @inline def riskyNested(x: Int): Int = try risky(x) finally ()
  @noinline def underTry2(x: Int): Int = try riskyNested(x) catch case _: TestFailure => -7

  def layout(): Unit =
    val ms = Array.tabulate(16)(i => new Mixed(i, 0x100000000L * i + i, i.toByte, i + 0.5, (i * 3).toShort))
    ok(c"layout.fields.roundtrip", ms.zipWithIndex.forall { (m, i) =>
      m.a == i && m.l == 0x100000000L * i + i && m.b == i.toByte && m.d == i + 0.5 && m.s == (i * 3).toShort
    })
    val m = ms(5)
    val lAddr = Intrinsics.castRawPtrToInt(Intrinsics.classFieldRawPtr(m, "l"))
    val dAddr = Intrinsics.castRawPtrToInt(Intrinsics.classFieldRawPtr(m, "d"))
    ok(c"layout.long-field.align8", (lAddr & 7) == 0)
    ok(c"layout.double-field.align8", (dAddr & 7) == 0)
    val la = new Array[Long](5); val ia = Array(0x5a5a5a5a)
    var i = 0
    while i < la.length do { la(i) = Long.MinValue + i; i += 1 }
    ok(c"layout.long-array.align8", (Intrinsics.castRawPtrToInt(la.asInstanceOf[LongArray].atRaw(0)) & 7) == 0)
    ok(c"layout.long-array.values", la(0) == Long.MinValue && la(4) == Long.MinValue + 4 && ia(0) == 0x5a5a5a5a)
    val da = Array(1.25, -2.5, 1e300)
    ok(c"layout.double-array.align8", (Intrinsics.castRawPtrToInt(da.asInstanceOf[DoubleArray].atRaw(0)) & 7) == 0)
    ok(c"layout.double-array.values", da(0) == 1.25 && da(1) == -2.5 && da(2) == 1e300)
    ok(c"long.arith", { val x = 0x7fffffffL * tr_opaque(3); x / 7 == 920350134L && x % 7 == 3 && (x >>> 33) == 0 })
    ok(c"double.arith", { val y = tr_opaque(1) / 3.0; y > 0.3333 && y < 0.3334 && math.sqrt(tr_opaque(16).toDouble) == 4.0 })

  def exceptions(): Unit =
    ok(c"exc.catch", (try thrower(tr_opaque(1)) catch case _: IllegalStateException => 42) == 42)
    ok(c"exc.no-throw", (try thrower(tr_opaque(0)) catch case _: IllegalStateException => 42) == 0)
    var fin = 0
    ok(c"exc.finally", (try try thrower(1) finally fin += 1 catch case _: IllegalStateException => fin) == 1)
    ok(c"exc.rethrow", (try (try thrower(1) catch case e: IllegalStateException => throw new TestFailure("re")) catch case _: TestFailure => 9) == 9)
    val shapes: Array[Shape] = Array(new Square(2), new Broken, new Square(3), new Broken)
    var sum = 0; var caught = 0; var i = 0
    while i < shapes.length do
      val s = shapes(tr_opaque(i))
      try sum += s.area() catch case _: ArithmeticException => caught += 1
      i += 1
    ok(c"exc.polymorphic-dispatch", sum == 13 && caught == 2)
    ok(c"exc.div-by-zero", (try 10 / tr_opaque(0) catch case _: ArithmeticException => -1) == -1)
    ok(c"exc.array-bounds", (try Array(1, 2)(tr_opaque(2)) catch case _: ArrayIndexOutOfBoundsException => -2) == -2)
    ok(c"exc.class-cast", (try { val a: Any = "s"; a.asInstanceOf[Integer].intValue } catch case _: ClassCastException => -3) == -3)
    ok(c"exc.message", (try thrower(1).toString catch case e: IllegalStateException => e.getMessage) == "x")

  def inlining(): Unit =
    ok(c"inline-under-try.value", underTry(tr_opaque(2)) == 5)
    ok(c"inline-under-try.throw", underTry(tr_opaque(3)) == -1)
    ok(c"inline-under-try.nested", underTry2(tr_opaque(3)) == -7 && underTry2(tr_opaque(4)) == 8)

  def romdata(): Unit =
    val mod = addr(RomTable)
    tr_value(c"romdata.module-addr", mod)
    ok(c"romdata.module-in-rom", inRom(mod))
    ok(c"romdata.graph-in-rom", inRom(addr(RomTable.primes)) && inRom(addr(RomTable.entries)) && inRom(addr(RomTable.entries.head.name)))
    ok(c"romdata.values", RomTable.primes.sum == 77 && RomTable.entries.map(_.weight).sum == (1L << 33) - 5 &&
      RomTable.big == 0x123456789AL && RomTable.ratio == 0.75 && RomTable.entries(1).name == "two")

  def lambdas(): Unit =
    ok(c"stable-module-lambda", Ops.inc(tr_opaque(1)) == 2 && Ops.twice(tr_opaque(4)) == 8 && Ops.both(tr_opaque(3)) == 8)
    ok(c"closure.capture", { val k = tr_opaque(5); List(1, 2, 3).map(_ + k).sum == 21 })

  def strings(): Unit =
    val sb = new java.lang.StringBuilder
    sb.append("n=").append(tr_opaque(-123)).append(',').append(Long.MinValue).append(',').append(true)
    ok(c"string.builder", sb.toString == "n=-123,-9223372036854775808,true")
    ok(c"string.concat", "a" + tr_opaque(1) + 2L + 'c' + false == "a12cfalse")
    ok(c"string.ops", { val s = "hello, gba"; s.substring(7) == "gba" && s.indexOf("gba") == 7 && s.length == 10 && s.hashCode == -1596211360 })
    ok(c"string.double", (tr_opaque(3) + 0.5).toString == "3.5")
    ok(c"string.int-parse", Integer.parseInt("-4096") == -4096 && java.lang.Long.parseLong("123456789012") == 123456789012L)
    val boxes: Array[Any] = Array(tr_opaque(1000), 2L, 3.5, 'x', true, 7.toByte)
    ok(c"boxing.match", boxes.map {
      case i: Int => i; case l: Long => l.toInt; case d: Double => d.toInt; case c: Char => c.toInt
      case b: Boolean => if b then 1 else 0; case b: Byte => b.toInt; case _ => -100000
    }.sum == 1000 + 2 + 3 + 120 + 1 + 7)
    ok(c"boxing.equals", Integer.valueOf(tr_opaque(1000)).equals(Integer.valueOf(1000)) && (boxes(0) == 1000))
    ok(c"collections.map", Map("a" -> 1, "b" -> 2).getOrElse("b", 0) == 2 && Vector.range(0, 50).filter(_ % 7 == 0).length == 8)

  def threads(): Unit =
    val t = Thread.currentThread()
    ok(c"thread.current", t != null && (t eq Thread.currentThread()))

  @exported("gbatest_main")
  def main(): CInt =
    layout(); exceptions(); inlining(); romdata(); lambdas(); strings(); threads()
    0
