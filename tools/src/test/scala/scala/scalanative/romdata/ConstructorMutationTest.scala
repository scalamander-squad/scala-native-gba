package scala.scalanative
package romdata

import org.junit.Assert._
import org.junit.Test

class ConstructorMutationTest extends OptimizerSpec {
  @Test def constructorCanMutateAnExistingCompanion(): Unit = {
    val previous = sys.props.get("scalanative.romdata")
    System.setProperty("scalanative.romdata", "true")
    try {
      optimize(
        "Test",
        Map(
          "Test.scala" ->
            """
              |object Counter {
              |  private var count = 0
              |  def current: Int = count
              |}
              |final class Counter { Counter.count += 1 }
              |object SoleInit { private var value = 7; def current: Int = value }
              |final class Own(var value: Int)
              |object Stable {
              |  private val own = new Own(7)
              |  def current: Int = own.value
              |}
              |object Test {
              |  def main(args: Array[String]): Unit = {
              |    println(Counter.current)
              |    new Counter
              |    new Counter
              |    println(Counter.current + Stable.current + SoleInit.current)
              |  }
              |}
              |""".stripMargin
        )
      ) {
        case (_, result) =>
          val modules = result.romData.modules.keySet.map(_.id)
          assertFalse(
            "constructor-mutated companion must remain in RAM",
            modules("Counter$")
          )
          assertTrue(
            "initializing the constructed receiver is safe",
            modules("Stable$")
          )
          assertTrue(
            "module initialization alone is safe",
            modules("SoleInit$")
          )
      }
    } finally
      previous match {
        case Some(value) => System.setProperty("scalanative.romdata", value)
        case None        => System.clearProperty("scalanative.romdata")
      }
  }
}
