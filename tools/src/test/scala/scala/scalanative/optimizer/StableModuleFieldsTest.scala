package scala.scalanative
package optimizer

import org.junit.Assert._
import org.junit._

import scala.scalanative.OptimizerSpec

/** A module `val` holding a function literal is a stable field: loads of it
 *  get the exact type of the lambda class, the call through it devirtualises
 *  and inlines, and the tuple / box the callee returns are scalar-replaced.
 *  See interflow.StableFields and Inline.returnsFreshAllocation.
 */
class StableModuleFieldsTest extends OptimizerSpec {

  private val sources = Map(
    "Test.scala" ->
      """|object Test {
         |  final case class Rng(state: Int) {
         |    def next: (Int, Rng) = {
         |      val s = state * 1103515245 + 24691
         |      ((s >>> 16) & 0xFFFF, Rng(s))
         |    }
         |  }
         |  object Rand {
         |    val u16: Rng => (Int, Rng) = _.next
         |    val below10: Rng => (Int, Rng) = rng => { val (v, r) = u16(rng); (v % 10, r) }
         |  }
         |  var cell: Rng = Rng(0)
         |  @noinline def draw(): Int = {
         |    val (v, next) = Rand.u16(cell)
         |    cell = next
         |    v
         |  }
         |  @noinline def drawBelow10(): Int = {
         |    val (v, next) = Rand.below10(cell)
         |    cell = next
         |    v
         |  }
         |  def main(args: Array[String]): Unit = println(draw() + drawBelow10())
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
      .getOrElse {
        val members = defns.collect {
          case d: nir.Defn.Define if d.name.top == nir.Global.Top("Test$") =>
            d.name.mangle
        }
        fail(s"method $name not found among $members").asInstanceOf[Nothing]
      }

  private def check(defn: nir.Defn.Define): Unit = {
    assertEquals(nir.Attr.DidOpt, defn.attrs.opt)
    val virtualApplies = defn.insts.collect {
      case nir.Inst.Let(_, nir.Op.Method(_, sig), _)
          if sig.unmangled.asInstanceOf[nir.Sig.Method].id == "apply" =>
        sig
    }
    assertTrue(
      s"call through the module val was not devirtualised: $virtualApplies",
      virtualApplies.isEmpty
    )
    val allocs = defn.insts.collect {
      case nir.Inst.Let(_, nir.Op.Classalloc(name, _), _) => name.id
      case nir.Inst.Let(_, nir.Op.Box(ty, _), _)          => ty.show
    }
    assertEquals(
      s"only the Rng stored into the var may be allocated, got $allocs",
      Seq("Test$Rng"),
      allocs
    )
  }

  @Test def moduleValLambdaIsInlined(): Unit = {
    optimize(
      setupConfig = _.withMode(scalanative.build.Mode.releaseFull),
      entry = "Test",
      sources = sources
    ) {
      case (_, result) =>
        check(method(result.defns, "draw"))
        check(method(result.defns, "drawBelow10"))
    }
  }

}
