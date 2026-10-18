/* SPDX-License-Identifier: Apache-2.0
 * Minimal ROM harness for the standalone Scala Native -> agbcc export smoke test.
 */
typedef unsigned short u16;
typedef unsigned int u32;

extern u16 gbatest_agbcc_u16(u16 left, u16 right);

int main(void) {
    volatile u32 *result = (volatile u32 *)0x0203FFF0;
    result[0] = 0x54524F4D; /* TROM */
    result[1] = 0;
    result[2] = 0;
    result[3] = 0;

    if (gbatest_agbcc_u16(0xFFFF, 2) == 1 &&
        gbatest_agbcc_u16(0x1234, 0x4321) == 0x5555) {
        result[1] = 2;
        result[3] = 0x50415353; /* PASS */
    } else {
        result[2] = 1;
        result[3] = 0x4641494C; /* FAIL */
    }
    for (;;) {
        __asm__ volatile("b .");
    }
}
