// SPDX-License-Identifier: Apache-2.0
typedef unsigned int U32;
extern int abiRunAgbccToClang(void);
extern int abiRunClangToAgbcc(void);

int main(void) {
    volatile U32 *result = (volatile U32 *)0x0203fff0;
    int stockToClang = abiRunAgbccToClang();
    int clangToStock = abiRunClangToAgbcc();
    result[0] = 0x54524f4d; /* TROM */
    result[1] = (stockToClang != 0) + (clangToStock != 0);
    result[2] = (stockToClang == 0) + (clangToStock == 0);
    result[3] = result[2] == 0 ? 0x50415353 : 0x4641494c; /* PASS / FAIL */
    for (;;)
        ;
}
