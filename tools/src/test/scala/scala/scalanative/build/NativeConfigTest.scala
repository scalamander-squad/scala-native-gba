package scala.scalanative.build

import org.junit.Assert._
import org.junit.Test

class NativeConfigTest {
  @Test def nativeLibrariesCompileByDefault(): Unit =
    assertTrue(NativeConfig.empty.compileNativeLibraries)

  @Test def customRuntimeCanSkipClasspathNativeSources(): Unit = {
    val config = NativeConfig.empty.withCompileNativeLibraries(false)
    assertFalse(config.compileNativeLibraries)
    assertNotEquals(NativeConfig.empty, config)
  }
}
