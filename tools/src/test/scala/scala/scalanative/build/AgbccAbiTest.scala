package scala.scalanative.build

import org.junit.Assert._
import org.junit.Test

class AgbccAbiTest {
  @Test def defaultProfileLeavesCompilerFlagsAlone(): Unit = {
    assertFalse(NativeConfig.empty.agbccAbi)
    assertEquals(Nil, AgbccAbi.compileFlags(Some("armv4t-none-eabi"), false))
    assertEquals(Nil, AgbccAbi.compileFlags(None, false))
  }

  @Test def gbaProfileSelectsTheAgbccStackAbi(): Unit = {
    val config = NativeConfig.empty
      .withTargetTriple("armv4t-none-eabi")
      .withAgbccAbi(true)
    assertTrue(config.agbccAbi)
    assertNotEquals(
      NativeConfig.empty.withTargetTriple("armv4t-none-eabi").##,
      config.##
    )
    assertEquals(
      Seq("-mabi=apcs-gnu"),
      AgbccAbi.compileFlags(Some("armv4t-none-eabi"), true)
    )
    assertEquals(
      Seq("-mabi=apcs-gnu"),
      AgbccAbi.compileFlags(Some("thumbv4t-none-eabi"), true)
    )
  }

  @Test def gbaProfileRejectsOtherTargets(): Unit = {
    assertThrows(
      classOf[BuildException],
      () => AgbccAbi.compileFlags(Some("x86_64-unknown-linux-gnu"), true)
    )
    assertThrows(
      classOf[BuildException],
      () => AgbccAbi.compileFlags(None, true)
    )
  }
}
