// SPDX-License-Identifier: Apache-2.0
// gba-testrom driver: Scala Native tools API, NIR -> 32-bit LLVM IR for armv4t-none-eabi as a static library
// (no main; ScalaNativeInit + @exported symbols). The C runtime compile/link of the tools is not used: build.sh
// compiles the generated .ll files itself, so the driver's own clang step may fail and that is ignored.
// args: outDir classpathFile   Env: ROMDATA=1|0, ROMDATA_OPTS=k=v,... (scalanative.romdata.<k>), DRIVER_PROPS=k=v,...,
//      CLANG_PATH / CLANGPP_PATH (default /usr/bin/clang, clang++)
import scala.scalanative.build.*
import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

object Driver:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get(args(0)).toAbsolutePath
    val cp = scala.io.Source.fromFile(args(1)).getLines().map(_.trim).filter(_.nonEmpty).map(Paths.get(_)).toSeq
    def props(env: String, prefix: String): Unit =
      sys.env.get(env).toSeq.flatMap(_.split(',')).map(_.trim).filter(_.nonEmpty).foreach { kv =>
        val Array(k, v) = kv.split("=", 2): @unchecked
        System.setProperty(prefix + k, v); println(s"property $prefix$k=$v")
      }
    if sys.env.get("ROMDATA").forall(v => v == "1" || v == "true") then
      System.setProperty("scalanative.romdata", "true"); println("property scalanative.romdata=true")
      props("ROMDATA_OPTS", "scalanative.romdata.")
    props("DRIVER_PROPS", "")
    Files.createDirectories(outDir)
    val nc = NativeConfig.empty
      .withClang(Paths.get(sys.env.getOrElse("CLANG_PATH", "/usr/bin/clang")))
      .withClangPP(Paths.get(sys.env.getOrElse("CLANGPP_PATH", "/usr/bin/clang++")))
      .withTargetTriple("armv4t-none-eabi")
      .withBuildTarget(BuildTarget.libraryStatic)
      .withGC(GC.none).withMode(Mode.releaseFull).withLTO(LTO.none)
      .withMultithreading(false).withCheck(false).withLinkStubs(true).withEmbedResources(false)
      .withBaseName("testrom")
    val cfg = Config.empty.withClassPath(cp).withMainClass(None).withBaseDir(outDir)
      .withCompilerConfig(nc).withLogger(Logger.default)
    println(s"is32BitPlatform=${nc.is32BitPlatform} triple=${nc.targetTriple} target=${nc.buildTarget}")
    given scala.scalanative.util.Scope = scala.scalanative.util.Scope.unsafe()
    try println(s"Output: ${Await.result(Build.build(cfg), 30.minutes)}")
    catch case e: Throwable => println(s"build step failed (expected once IR exists): $e")
