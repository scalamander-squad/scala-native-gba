/* SPDX-License-Identifier: Apache-2.0
 * gba-testrom memory map: code, rodata, ROM-resident Scala constants (romdata) and class records in ROM
 * (0x08000000); .data/.bss and the bump heap in EWRAM (top 16 bytes = result block); stack at the top of IWRAM. */
MEMORY {
  rom   (rx)  : ORIGIN = 0x08000000, LENGTH = 32M
  ewram (rwx) : ORIGIN = 0x02000000, LENGTH = 256K - 16
  iwram (rwx) : ORIGIN = 0x03000000, LENGTH = 32K
}
ENTRY(_start)
SECTIONS {
  .text : { KEEP(*(.crt0)) *(.text .text.*) *(.rodata .rodata.*) *(.gcc_except_table*) . = ALIGN(4); } > rom
  /* class records (*G4type) are emitted as writable globals but never written: keep them in ROM */
  .rtti_rom : { *(.data.*G4type) . = ALIGN(4); } > rom
  .ARM.extab : { *(.ARM.extab* .gnu.linkonce.armextab.*) } > rom
  .ARM.exidx : { __exidx_start = .; *(.ARM.exidx* .gnu.linkonce.armexidx.*) __exidx_end = .; } > rom
  .data : { __data_start = .; *(.data .data.*) . = ALIGN(4); __data_end = .; } > ewram AT> rom
  __data_lma = LOADADDR(.data);
  .bss (NOLOAD) : { __bss_start = .; *(.bss .bss.* COMMON) . = ALIGN(8); __bss_end = .; } > ewram
  .heap (NOLOAD) : { __heap_start = .; } > ewram
  __heap_end = ORIGIN(ewram) + LENGTH(ewram);
  __stack_top = 0x03007F00;   /* BIOS uses 0x03007F00..0x03007FFF */
  /DISCARD/ : { *(.comment) *(.note*) }
}
