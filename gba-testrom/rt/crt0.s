@ SPDX-License-Identifier: Apache-2.0
@ gba-testrom crt0 (ARM state entry). The cartridge header carries no logo (mGBA's HLE BIOS does not check it):
@ only the branch, a title, the fixed 0x96 byte mGBA's ROM detection needs, and the complement checksum
@ (patched by build.sh). Sets the IRQ/System stacks, copies .data, zeroes .bss, calls main in Thumb state.
    .section .crt0, "ax"
    .arm
    .global _start
_start:
    b       rom_start
    .space  0x9C                    @ logo area: left zero
    .ascii  "SNTESTROM\0\0\0"       @ 0xA0 title
    .ascii  "ZSNT"                  @ 0xAC game code
    .ascii  "00"                    @ 0xB0 maker
    .byte   0x96, 0, 0              @ 0xB2 fixed value, unit, device
    .space  7                       @ 0xB5 reserved
    .byte   0, 0                    @ 0xBC version, 0xBD complement checksum (build.sh)
    .space  2
rom_start:
    mov     r0, #0x12               @ IRQ mode
    msr     cpsr_c, r0
    ldr     sp, =0x03007FA0
    mov     r0, #0x1F               @ System mode
    msr     cpsr_c, r0
    ldr     sp, =__stack_top
    ldr     r0, =__data_lma
    ldr     r1, =__data_start
    ldr     r2, =__data_end
1:  cmp     r1, r2
    ldrlo   r3, [r0], #4
    strlo   r3, [r1], #4
    blo     1b
    ldr     r1, =__bss_start
    ldr     r2, =__bss_end
    mov     r3, #0
2:  cmp     r1, r2
    strlo   r3, [r1], #4
    blo     2b
    ldr     r3, =main
    mov     lr, pc
    bx      r3
3:  b       3b
    .ltorg
