// SPDX-License-Identifier: Apache-2.0
// Compile this file as both caller and callee with agbcc and Clang/APCS. The
// mixed calls exercise stack arguments, u64 alignment and varargs, a by-value
// struct, a struct return, and the callee-saved register convention.
typedef unsigned int U32;
typedef unsigned long long U64;

typedef struct {
    U32 first;
    U64 wide;
    U32 last;
} Payload;

typedef struct {
    U32 tag;
    U64 value;
    U32 tail;
} Result;

extern Result ABI_TARGET(
    U32 a0, U32 a1, U32 a2, U32 a3, U32 a4, U32 a5, U64 wide, Payload payload, ...);

#ifdef ABI_CALLEE
#include <stdarg.h>

Result ABI_TARGET(
    U32 a0, U32 a1, U32 a2, U32 a3, U32 a4, U32 a5, U64 wide, Payload payload, ...) {
    va_list args;
    U32 extra;
    U64 extraWide;
    Result result;
    va_start(args, payload);
    extra = va_arg(args, U32);
    extraWide = va_arg(args, U64);
    va_end(args);

    result.tag = a0 + a1 + a2 + a3 + a4 + a5 + payload.first + payload.last + extra;
    result.value = wide ^ payload.wide ^ extraWide ^ (((U64)a4 << 32) | a5);
    result.tail = result.tag ^ (U32)result.value ^ (U32)(result.value >> 32);
    return result;
}
#else
int ABI_CALLER(void) {
    Payload payload = {7, 0x99aabbccddeeff00ULL, 8};
    Result result = ABI_TARGET(
        1, 2, 3, 4, 5, 6, 0x1122334455667788ULL, payload, 9, 0xa1b2c3d4e5f60718ULL);
    U64 expected = 0x1122334455667788ULL ^ 0x99aabbccddeeff00ULL ^
        0xa1b2c3d4e5f60718ULL ^ (((U64)5 << 32) | 6);
    return result.tag == 45 && result.value == expected &&
        result.tail == (45U ^ (U32)expected ^ (U32)(expected >> 32));
}
#endif
