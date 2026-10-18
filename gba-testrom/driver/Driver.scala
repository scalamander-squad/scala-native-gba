// SPDX-License-Identifier: Apache-2.0
import java.nio.file.{Files, Paths}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

// gba-testrom driver: Scala Native tools API, NIR -> 32-bit LLVM IR for armv4t-none-eabi as a static library
// (no main; ScalaNativeInit + @exported symbols). The ROM supplies its own C runtime, so this driver compiles the
// generated LLVM objects but excludes the upstream OS-specific native C/assembly sources.
// args: outDir classpathFile   Env: ROMDATA=1|0, ROMDATA_OPTS=k=v,... (scalanative.romdata.<k>), DRIVER_PROPS=k=v,...,
//      AGBCC_ABI=1 for APCS stack/call ABI, CLANG_PATH / CLANGPP_PATH (default /usr/bin/clang, clang++)
import scala.scalanative.build.*

object Driver:
  def main(args: Array[String]): Unit =
    val outDir = Paths.get(args(0)).toAbsolutePath
    val cp = scala.io.Source
      .fromFile(args(1))
      .getLines()
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Paths.get(_))
      .toSeq
    def props(env: String, prefix: String): Unit =
      sys.env
        .get(env)
        .toSeq
        .flatMap(_.split(','))
        .map(_.trim)
        .filter(_.nonEmpty)
        .foreach { kv =>
          val Array(k, v) = kv.split("=", 2): @unchecked
          System.setProperty(prefix + k, v); println(s"property $prefix$k=$v")
        }
    if sys.env.get("ROMDATA").forall(v => v == "1" || v == "true") then
      System.setProperty("scalanative.romdata", "true");
      println("property scalanative.romdata=true")
      props("ROMDATA_OPTS", "scalanative.romdata.")
    props("DRIVER_PROPS", "")
    Files.createDirectories(outDir)
    val baseConfig = NativeConfig.empty
      .withClang(Paths.get(sys.env.getOrElse("CLANG_PATH", "/usr/bin/clang")))
      .withClangPP(
        Paths.get(sys.env.getOrElse("CLANGPP_PATH", "/usr/bin/clang++"))
      )
      .withTargetTriple("armv4t-none-eabi")
      .withAgbccAbi(
        sys.env.get("AGBCC_ABI").exists(v => v == "1" || v == "true")
      )
      .withBuildTarget(BuildTarget.libraryStatic)
      .withCompileNativeLibraries(false)
      .withGC(GC.none)
      .withMode(Mode.releaseFull)
      .withLTO(LTO.none)
      .withMultithreading(false)
      .withCheck(false)
      .withLinkStubs(true)
      .withEmbedResources(false)
      .withBaseName("testrom")
    val nc =
      if sys.env.get("AGBCC_CODEGEN_ALL").exists(v => v == "1" || v == "true")
      then
        baseConfig
          .withAgbccCodegen(true)
          .withAgbccCodegenExportSignatures(
            Map(
              "gbatest_agbcc_u16" -> scala.scalanative.nir.Type.Function(
                Seq(
                  scala.scalanative.nir.Type.Char,
                  scala.scalanative.nir.Type.Char
                ),
                scala.scalanative.nir.Type.Char
              )
            )
          )
      else if sys.env
            .get("AGBCC_CODEGEN_APP")
            .exists(v => v == "1" || v == "true") then
        baseConfig
          .withAgbccCodegenExports(
            Set("gbatest_main", "gbatest_agbcc_u16", "gbatest_agbcc_app_graph")
          )
          .withAgbccCodegenExportOrder(
            Seq("gbatest_main", "gbatest_agbcc_app_graph", "gbatest_agbcc_u16")
          )
          .withAgbccCodegenExportSignatures(
            Map(
              "gbatest_agbcc_u16" -> scala.scalanative.nir.Type.Function(
                Seq(
                  scala.scalanative.nir.Type.Char,
                  scala.scalanative.nir.Type.Char
                ),
                scala.scalanative.nir.Type.Char
              )
            )
          )
      else if sys.env
            .get("AGBCC_CODEGEN_GRAPH")
            .exists(v => v == "1" || v == "true") then
        baseConfig
          .withAgbccCodegenExports(
            Set("gbatest_agbcc_app_graph", "gbatest_agbcc_u16")
          )
          .withAgbccCodegenExportSignatures(
            Map(
              "gbatest_agbcc_u16" -> scala.scalanative.nir.Type.Function(
                Seq(
                  scala.scalanative.nir.Type.Char,
                  scala.scalanative.nir.Type.Char
                ),
                scala.scalanative.nir.Type.Char
              )
            )
          )
      else if sys.env.get("AGBCC_CODEGEN").exists(v => v == "1" || v == "true")
      then
        baseConfig
          .withAgbccCodegenExports(Set("gbatest_agbcc_u16"))
          .withAgbccCodegenExportSignatures(
            Map(
              "gbatest_agbcc_u16" -> scala.scalanative.nir.Type.Function(
                Seq(
                  scala.scalanative.nir.Type.Char,
                  scala.scalanative.nir.Type.Char
                ),
                scala.scalanative.nir.Type.Char
              )
            )
          )
      else baseConfig
    val cfg = Config.empty
      .withClassPath(cp)
      .withMainClass(None)
      .withBaseDir(outDir)
      .withCompilerConfig(nc)
      .withLogger(Logger.default)
    println(
      s"is32BitPlatform=${nc.is32BitPlatform} triple=${nc.targetTriple} target=${nc.buildTarget}"
    )
    given scala.scalanative.util.Scope = scala.scalanative.util.Scope.unsafe()
    println(s"Output: ${Await.result(Build.build(cfg), 30.minutes)}")
