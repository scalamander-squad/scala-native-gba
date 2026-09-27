package scala.scalanative
package optimizer

import org.junit.Assert._
import org.junit._

import scala.scalanative.OptimizerSpec

/** Interflow specialises a method per call-site argument types
 *  (`Visit.duplicateName`). A parameter the body never reads gains nothing
 *  from a more precise type, so it is left at its declared type: a trait
 *  method that ignores `this` (like `scala.runtime.EnumValue.productElement`,
 *  which only throws) gets one copy, not one per exact receiver class. A
 *  method that reads `this` is still specialised per receiver.
 *  `-Dscalanative.interflow.duplicateUnusedParams=true` restores the old rule.
 */
class DuplicateUnusedParamsTest extends OptimizerSpec {

  private val sources = Map(
    "Test.scala" ->
      """|object Test {
         |  trait T {
         |    def k: Int
         |    @noinline def ignoresThis(n: Int): Int = {
         |      var i = 0; var s = 0
         |      while (i < n) { s += i * n; i += 1 }
         |      if (s == 42) throw new IndexOutOfBoundsException(n.toString)
         |      s
         |    }
         |    @noinline def readsThis(n: Int): Int = {
         |      var i = 0; var s = 0
         |      while (i < n) { s += i * k; i += 1 }
         |      s
         |    }
         |  }
         |  final class A extends T { def k = 1 }
         |  final class B extends T { def k = 2 }
         |  final class C extends T { def k = 3 }
         |  def main(args: Array[String]): Unit = {
         |    val a = new A; val b = new B; val c = new C
         |    println(a.ignoresThis(3) + b.ignoresThis(4) + c.ignoresThis(5))
         |    println(a.readsThis(3) + b.readsThis(4) + c.readsThis(5))
         |  }
         |}
         |""".stripMargin
  )

  private def copies(defns: Seq[nir.Defn], name: String): (Int, Int) = {
    val ms = defns.collect {
      case defn: nir.Defn.Define
          if defn.name.mangle.contains(s"D${name.length}$name") &&
            defn.name.top.id.startsWith("Test$T") =>
        defn.name
    }
    val dups = ms.count(_.sig.isDuplicate)
    (ms.size, dups)
  }

  private def run(fn: Seq[nir.Defn] => Unit): Unit =
    optimize(
      setupConfig = _.withMode(scalanative.build.Mode.releaseFull),
      entry = "Test",
      sources = sources
    ) { case (_, result) => fn(result.defns) }

  @Test def unusedReceiverIsNotSpecialised(): Unit = run { defns =>
    val (_, ignoreDups) = copies(defns, "ignoresThis")
    assertEquals("ignoresThis duplicates", 0, ignoreDups)
    val (_, readDups) = copies(defns, "readsThis")
    assertTrue(s"readsThis should still be specialised: $readDups", readDups >= 2)
  }

  @Test def oldRuleSpecialisesEveryReceiver(): Unit = {
    System.setProperty("scalanative.interflow.duplicateUnusedParams", "true")
    try run { defns =>
      val (_, ignoreDups) = copies(defns, "ignoresThis")
      assertTrue(s"ignoresThis duplicates with the old rule: $ignoreDups", ignoreDups >= 2)
    }
    finally System.clearProperty("scalanative.interflow.duplicateUnusedParams")
  }
}
