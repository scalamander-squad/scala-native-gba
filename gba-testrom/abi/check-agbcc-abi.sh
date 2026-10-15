#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Mixed-compiler GBA ROM: both callers and callees use agbcc's APCS ABI.
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
TESTROM=$(cd "$HERE/.." && pwd)
if [[ -z ${OUT:-} ]]; then OUT=$(mktemp -d); OWN_OUT=1
else mkdir -p "$OUT"; OWN_OUT=0
fi
cleanup() { [[ $OWN_OUT == 0 ]] || rm -rf "$OUT"; }
trap cleanup EXIT

AGBCC=${AGBCC:?set AGBCC to the game tools/agbcc/bin/agbcc}
CLANG=${CLANG:-clang}
ARM_GCC=${ARM_GCC:-arm-none-eabi-gcc}
ARM_CPP=${ARM_CPP:-arm-none-eabi-cpp}
ARM_AS=${ARM_AS:-arm-none-eabi-as}
OBJCOPY=${OBJCOPY:-arm-none-eabi-objcopy}
OBJDUMP=${OBJDUMP:-arm-none-eabi-objdump}
MGBA_RUN=${MGBA_RUN:-$TESTROM/out/mgba-run}
MGBA_SRC=${MGBA_SRC:-$HOME/.local/src/mgba}
MGBA_BUILD=${MGBA_BUILD:-$HOME/.local/src/mgba-build}
MGBA_LIBS=${MGBA_LIBS:--lpng -lz -lm -lpthread}
SRC=$HERE/Interop.c
APCS_CLANG=("$CLANG" -target armv4t-none-eabi -mthumb -march=armv4t \
  -mfloat-abi=soft -mabi=apcs-gnu -Oz -fomit-frame-pointer)
AGBCC_FLAGS=(-mthumb-interwork -O2 -fhex-asm)
AGBCC_DIR=$(cd "$(dirname "$AGBCC")/.." && pwd)
POKEROOT=$(cd "$AGBCC_DIR/../.." && pwd)

compile_agbcc() {
  local name=$1; shift
  "$ARM_CPP" -I"$POKEROOT/include" -I"$AGBCC_DIR/include" -I"$AGBCC_DIR" \
    -nostdinc -undef -std=gnu89 -DMODERN=0 "$@" "$SRC" > "$OUT/$name.i"
  "$AGBCC" "${AGBCC_FLAGS[@]}" -o - - < "$OUT/$name.i" > "$OUT/$name.s"
  "$ARM_AS" -mcpu=arm7tdmi -o "$OUT/$name.o" "$OUT/$name.s"
}

# Callees and callers are built with separate compilers in both directions.
compile_agbcc target-agbcc -DABI_CALLEE -DABI_TARGET=abiTargetAgbcc
"${APCS_CLANG[@]}" -DABI_CALLEE -DABI_TARGET=abiTargetClang \
  -c "$SRC" -o "$OUT/target-clang.o"
compile_agbcc caller-agbcc -DABI_TARGET=abiTargetClang -DABI_CALLER=abiRunAgbccToClang
"${APCS_CLANG[@]}" -DABI_TARGET=abiTargetAgbcc \
  -DABI_CALLER=abiRunClangToAgbcc -c "$SRC" -o "$OUT/caller-clang.o"

# Verify the narrow frame claim against actual agbcc object code. A default
# Clang object is the AAPCS control; the opt-in profile should match this
# function's four-byte frame at its call site.
SRC=$HERE/FrameProbe.c
compile_agbcc frame-agbcc
"${APCS_CLANG[@]}" -c "$SRC" -o "$OUT/frame-apcs.o"
"$CLANG" -target armv4t-none-eabi -mthumb -march=armv4t -mfloat-abi=soft \
  -Oz -fomit-frame-pointer -c "$SRC" -o "$OUT/frame-aapcs.o"
"$OBJDUMP" -d "$OUT/frame-agbcc.o" > "$OUT/frame-agbcc.dump"
"$OBJDUMP" -d "$OUT/frame-apcs.o" > "$OUT/frame-apcs.dump"
"$OBJDUMP" -d "$OUT/frame-aapcs.o" > "$OUT/frame-aapcs.dump"
python3 - "$OUT" <<'PY'
import pathlib, re, sys
out = pathlib.Path(sys.argv[1])

def frame(path):
    body = (out / path).read_text().split("<agbccFrameProbe>:", 1)[1].split("\n\n", 1)[0]
    depth = 0
    for line in body.splitlines():
        # GNU objdump places the opcode and operands in separate tab columns.
        columns = line.split("\t")
        if len(columns) < 3:
            continue
        op, args = columns[-2].strip(), columns[-1].strip()
        if op == "push":
            regs = re.search(r"\{([^}]+)\}", args)
            if regs:
                depth += 4 * len(regs.group(1).split(","))
        elif op == "sub":
            amount = re.search(r"\bsp\s*,\s*#(\d+)", args)
            if amount:
                depth += int(amount.group(1))
        elif op in ("bl", "blx"):
            return depth
    raise AssertionError(f"no call found in {path}\n{body}")

stock, apcs, aapcs = (frame(f"frame-{name}.dump") for name in ("agbcc", "apcs", "aapcs"))
assert stock == 4, f"agbcc frame changed from 4 bytes: {stock}"
assert apcs == stock, f"APCS profile frame={apcs}, agbcc frame={stock}"
assert aapcs == 8, f"default AAPCS frame changed from 8 bytes: {aapcs}"
print(f"PASS call-frame parity: agbcc={stock}, APCS profile={apcs}, AAPCS control={aapcs} bytes")
PY

SRC=$HERE/Interop.c
"$ARM_GCC" -mthumb -march=armv4t -mfloat-abi=soft -mabi=apcs-gnu \
  -c "$HERE/main.c" -o "$OUT/main.o"
"$ARM_GCC" -mthumb -march=armv4t -mfloat-abi=soft -c "$TESTROM/rt/crt0.s" \
  -o "$OUT/crt0.o"
"$ARM_GCC" -mthumb -march=armv4t -mfloat-abi=soft -mabi=apcs-gnu \
  -nostdlib -nostartfiles -Wl,--gc-sections -Wl,--no-warn-mismatch \
  -Wl,-T,"$TESTROM/rt/gba.ld" \
  "$OUT/crt0.o" "$OUT/main.o" "$OUT/target-agbcc.o" "$OUT/target-clang.o" \
  "$OUT/caller-agbcc.o" "$OUT/caller-clang.o" -lgcc -o "$OUT/abi.elf"
"$OBJCOPY" -O binary "$OUT/abi.elf" "$OUT/abi.gba"
python3 - "$OUT/abi.gba" <<'PY'
import sys
p = sys.argv[1]
b = bytearray(open(p, "rb").read())
b[0xBD] = (-(sum(b[0xA0:0xBD]) + 0x19)) & 0xFF
b += b"\xff" * (-len(b) % 4)
open(p, "wb").write(b)
PY
if [[ ! -x $MGBA_RUN ]]; then
  mkdir -p "$(dirname "$MGBA_RUN")"
  cc -O2 -I"$MGBA_SRC/include" -I"$MGBA_BUILD/include" \
    "$TESTROM/harness/mgba-run.c" "$MGBA_BUILD/libmgba.a" $MGBA_LIBS -o "$MGBA_RUN"
fi
"$MGBA_RUN" "$OUT/abi.gba" 120
