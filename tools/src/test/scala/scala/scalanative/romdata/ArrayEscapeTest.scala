package scala.scalanative
package romdata

import java.nio.file.{Files, Path}

import org.junit.Assert._
import org.junit.Test

/** The strict array-ownership gate of the link-time module evaluation: a module
 *  whose graph holds an array is ROM-resident only if no run-time code can
 *  store into that array, directly, through a helper that writes its parameter,
 *  through `java.util.Arrays.sort` or as the destination of `System.arraycopy`.
 */
class ArrayEscapeTest extends OptimizerSpec {

  private val sources = Map(
    "Test.scala" ->
      """|object Helpers {
         |  def fill(a: Array[Int]): Unit = a(0) = 1
         |  def sum(a: Array[Int]): Int = { var s = 0; var i = 0; while (i < a.length) { s += a(i); i += 1 }; s }
         |  def identity(a: Array[Int]): Array[Int] = a
         |}
         |object ReadOnly { private val table = Array(1, 2, 3); def get(i: Int): Int = table(i); def total: Int = Helpers.sum(Helpers.identity(table)) }
         |object WrittenDirect { val table = Array(1, 2, 3); def poke(): Unit = table(0) = 9 }
         |object WrittenViaHelper { private val table = Array(1, 2, 3); def poke(): Unit = Helpers.fill(table) }
         |object WrittenViaIdentity { private val table = Array(1, 2, 3); def poke(): Unit = Helpers.identity(table)(1) = 5 }
         |object WrittenViaSort { private val table = Array(3, 2, 1); def sortIt(): Unit = java.util.Arrays.sort(table) }
         |object WrittenViaArraycopy { private val table = Array(3, 2, 1); def copyIn(src: Array[Int]): Unit = System.arraycopy(src, 0, table, 0, 3) }
         |object ReadViaArraycopy { private val table = Array(3, 2, 1)
         |  def copyOut(): Array[Int] = { val d = new Array[Int](3); System.arraycopy(table, 0, d, 0, 3); d } }
         |object Family { val all: Vector[Int] = Vector(1, 2, 3); val byId: Map[Int, String] = Map(1 -> "a", 2 -> "b") }
         |object Test {
         |  def main(args: Array[String]): Unit = {
         |    println(ReadOnly.get(1) + ReadOnly.total + WrittenDirect.table(0) + ReadViaArraycopy.copyOut()(0))
         |    WrittenDirect.poke(); WrittenViaHelper.poke(); WrittenViaIdentity.poke(); WrittenViaSort.sortIt()
         |    WrittenViaArraycopy.copyIn(Array(7, 8, 9))
         |    println(Family.all.sum + Family.byId.size)
         |  }
         |}
         |""".stripMargin
  )

  private def withRomdata[T](log: Path)(body: => T): T = {
    val keys = Seq("scalanative.romdata", "scalanative.romdata.log")
    val old = keys.map(k => k -> sys.props.get(k))
    System.setProperty("scalanative.romdata", "true")
    System.setProperty("scalanative.romdata.log", log.toString)
    try body
    finally
      old.foreach {
        case (k, Some(v)) => System.setProperty(k, v)
        case (k, None)    => System.clearProperty(k)
      }
  }

  @Test def strictModeFailsTheLink(): Unit = {
    val log = Files.createTempFile("romdata", ".log")
    withRomdata(log) {
      System.setProperty("scalanative.romdata.strict", "Written.*|Family.*")
      try {
        optimize("Test", sources) {
          case _ => fail("strict mode should fail the link")
        }
      } catch {
        case e: build.BuildException =>
          assertTrue(
            e.getMessage,
            e.getMessage.contains("strict mode") && e.getMessage.contains(
              "WrittenViaSort$"
            )
          )
          assertFalse(e.getMessage, e.getMessage.contains("Family$"))
      } finally System.clearProperty("scalanative.romdata.strict")
    }
  }

  @Test def arrayOwnership(): Unit = {
    val log = Files.createTempFile("romdata", ".log")
    withRomdata(log) {
      optimize("Test", sources) {
        case (_, result) =>
          val rom = result.romData.modules.keySet.map(_.id)
          def isRom(m: String) = rom.contains(m + "$")
          val text = new String(Files.readAllBytes(log), "UTF-8")
          for (m <- Seq("ReadOnly", "ReadViaArraycopy", "Family", "Helpers"))
            assertTrue(s"$m should be ROM-resident\n$text", isRom(m))
          for (m <- Seq(
                "WrittenDirect",
                "WrittenViaHelper",
                "WrittenViaIdentity",
                "WrittenViaSort",
                "WrittenViaArraycopy"
              ))
            assertFalse(s"$m must stay at run time\n$text", isRom(m))
          // the log names the candidate and the offending store site
          assertTrue(
            text,
            text.contains("RT   WrittenViaHelper$") && text.contains(
              "Helpers$D4fill"
            )
          )
          assertTrue(
            text,
            text.contains("RT   WrittenViaSort$") && text.contains(
              "java.util.Arrays"
            )
          )
          assertTrue(
            text,
            text.contains("RT   WrittenViaArraycopy$") && text.contains(
              "memmove"
            )
          )
      }
    }
  }
}
