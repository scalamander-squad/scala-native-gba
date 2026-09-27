package scala.scalanative
package optimizer

import org.junit.Assert._
import org.junit._

import scala.scalanative.OptimizerSpec

/** Per-instruction try/catch with inlining under the handler
 *  (`Opt.inlineUnderTry`): calls inside a `try` are inlined, the inlined
 *  instructions unwind to the call site's handler, a callee `throw` goes to
 *  it, and objects the handler cannot observe (a case class, the box of an
 *  argument passed to a callee that contains a `try`) are scalar-replaced:
 *  the method allocates exactly what the same body without `try` does. Semantics are checked by the host smoke test
 *  (spikes/scala-native-opt/hosttest), this test checks the shape.
 */
class InlineUnderTryTest extends OptimizerSpec {

  private val sources = Map(
    "Test.scala" ->
      """|object Test {
         |  final case class Pair(a: Int, b: Int)
         |  @inline def lookup(i: Int): Option[Int] = if (i >= 0 && i < 10) Some(i * 3) else None
         |  var failures = 0
         |  @noinline def underTry(i: Int, j: Int): Int =
         |    try {
         |      val v = lookup(i).getOrElse(throw new NoSuchElementException)
         |      val p = Pair(v, j)
         |      p.a + p.b
         |    } catch { case _: Throwable => failures += 1; -1 }
         |  @noinline def noTry(i: Int, j: Int): Int = {
         |    val v = lookup(i).getOrElse(throw new NoSuchElementException)
         |    val p = Pair(v, j)
         |    p.a + p.b
         |  }
         |  def withTry(x: Any): Int =
         |    try x.asInstanceOf[Int] + 1
         |    catch { case _: ClassCastException => failures += 1; 0 }
         |  @noinline def boxedArg(i: Int): Int = withTry(i)
         |  def main(args: Array[String]): Unit =
         |    println(underTry(1, 2) + underTry(20, 3) + boxedArg(4) + noTry(1, 2))
         |}
         |""".stripMargin
  )

  private def method(defns: Seq[nir.Defn], name: String): nir.Defn.Define =
    defns
      .collectFirst {
        case defn: nir.Defn.Define
            if defn.name.top == nir.Global.Top("Test$") &&
              defn.name.mangle.contains(s"D${name.length}$name") =>
          defn
      }
      .getOrElse(fail(s"method $name not found").asInstanceOf[Nothing])

  private def allocs(defn: nir.Defn.Define): Seq[String] =
    defn.insts.collect {
      case nir.Inst.Let(_, nir.Op.Classalloc(name, _), _) => name.id
      case nir.Inst.Let(_, nir.Op.Box(ty, _), _)          => "box " + ty.show
    }

  private def calls(defn: nir.Defn.Define): Seq[String] =
    defn.insts.collect {
      case nir.Inst.Let(_, nir.Op.Call(_, nir.Val.Global(name, _), _), _) =>
        name.show
    }

  @Test def inlinesUnderTry(): Unit = {
    optimize(
      setupConfig = _.withMode(scalanative.build.Mode.releaseFull),
      entry = "Test",
      sources = sources
    ) {
      case (_, result) =>
        val m = method(result.defns, "underTry")
        assertEquals(nir.Attr.DidOpt, m.attrs.opt)
        assertTrue(
          s"lookup should be inlined under the try: ${calls(m)}",
          !calls(m).exists(_.contains("lookup"))
        )
        val a = allocs(m)
        val reference = allocs(method(result.defns, "noTry"))
        assertTrue(
          s"Pair should be scalar-replaced, got $a",
          !a.exists(_.contains("Pair"))
        )
        assertEquals(
          "the try must not add allocations (same as the body without try)",
          reference.filterNot(_.contains("Exception")).sorted,
          a.filterNot(_.contains("Exception")).sorted
        )
        assertTrue(
          s"the exception is still allocated: $a",
          a.exists(_.contains("NoSuchElementException"))
        )
        // the callee's `throw` (inlined into the try) unwinds to the handler;
        // the only throw without one is the catch's rethrow of non-matches
        val throws = m.insts.collect { case nir.Inst.Throw(_, u) => u }
        assertTrue(
          s"the inlined throw must go to the handler: $throws",
          throws.exists(_.isInstanceOf[nir.Next.Unwind])
        )
        val unwinds = m.insts.count {
          case nir.Inst.Let(_, _: nir.Op.Call, _: nir.Next.Unwind) => true
          case _                                                   => false
        }
        assertTrue("calls inside the try keep the handler", unwinds > 0)

        val b = method(result.defns, "boxedArg")
        assertEquals(nir.Attr.DidOpt, b.attrs.opt)
        assertTrue(
          s"withTry should be inlined: ${calls(b)}",
          !calls(b).exists(_.contains("withTry"))
        )
        assertTrue(
          s"the argument box should be scalar-replaced: ${allocs(b)}",
          !allocs(b).exists(_.startsWith("box"))
        )
    }
  }
}

object InlineUnderTryTest {
  @BeforeClass def enable(): Unit =
    System.setProperty("scalanative.interflow.perInstructionTryCatch", "true")
}
