#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Builds gba-testrom/out/testrom.gba from src/TestRom.scala with a locally published fork (~/.ivy2/local).
# Needs: cs (coursier), a JDK, clang with the ARM target, arm-none-eabi-{gcc,ld,objcopy,nm} with newlib-nano.
# Env: NATIVE_VERSION  fork version (default <baseVersion>-<localForkTag> from ../project/ScalaNativeBuildInfo.scala)
#      ROMDATA=1|0     link-time module evaluation (default 1; 0 disables the fork's romdata pass -> romdata checks FAIL)
#      AGBCC_ABI=1     opt into APCS four-byte stack/call ABI (not instruction-selection matching)
#      AGBCC_CODEGEN=1 build out/agbc-probe.gba from one agbcc-generated export (needs AGBCC)
#      AGBCC_CODEGEN_APP=1 route test-ROM app exports and reachable calls through agbcc
#      AGBCC_CODEGEN_GRAPH=1 route a no-exception application call graph through agbcc
#      AGBCC_CODEGEN_ALL=1 route every lowered NIR method body through agbcc
#      AGBCC_EHABI_FRAMES=1 add EHABI frame recipes and catch-site LSDA to agbcc output
#      DRIVER_PROPS    extra tools system properties, e.g. scalanative.interflow.inlineUnderTry=false
#      ARM_AS          assembler (default ${ARM_PREFIX}as)   ARM_PREFIX (default arm-none-eabi-)
#      JOBS            parallel clang jobs (default nproc)   CLANG (default clang)   OUT (default out)
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd); cd "$HERE"
BI=../project/ScalaNativeBuildInfo.scala
V=${NATIVE_VERSION:-$(sed -n 's/.*baseVersion = "\(.*\)".*/\1/p' $BI)-$(sed -n 's/.*localForkTag = "\(.*\)".*/\1/p' $BI)}
S3=3.9.0; OUT=${OUT:-out}; P=${ARM_PREFIX:-arm-none-eabi-}; CLANG=${CLANG:-clang}
export ROMDATA=${ROMDATA:-1} ROMDATA_OPTS=${ROMDATA_OPTS-strict=gbatest\\.Rom.*}
ARMFL="-mthumb -march=armv4t -mfloat-abi=soft"
AGBCC_FLAGS=""
ABI_FLAGS=()
LINKER_FLAGS=()
AGBCC_CODEGEN_ENABLED=0
if [[ ${AGBCC_CODEGEN:-0} == 1 || ${AGBCC_CODEGEN:-0} == true || \
      ${AGBCC_CODEGEN_APP:-0} == 1 || ${AGBCC_CODEGEN_APP:-0} == true || \
      ${AGBCC_CODEGEN_GRAPH:-0} == 1 || ${AGBCC_CODEGEN_GRAPH:-0} == true || \
      ${AGBCC_CODEGEN_ALL:-0} == 1 || ${AGBCC_CODEGEN_ALL:-0} == true ]]; then
  AGBCC_CODEGEN_ENABLED=1
fi
if [[ ${AGBCC_ABI:-0} == 1 || ${AGBCC_ABI:-0} == true || $AGBCC_CODEGEN_ENABLED == 1 ]]; then
  AGBCC_FLAGS="-mabi=apcs-gnu"
  ABI_FLAGS=(-mabi=apcs-gnu)
fi
if [[ $AGBCC_CODEGEN_ENABLED == 1 ]]; then
  AGBCC=${AGBCC:?set AGBCC to the pokeemerald tools/agbcc/bin/agbcc}
  ARM_AS=${ARM_AS:-${P}as}
  LINKER_FLAGS=(-Wl,--no-warn-mismatch)
fi
mkdir -p "$OUT"; rm -rf "$OUT/classes" "$OUT/driver-classes" "$OUT/sn" "$OUT/obj"
cpof() { cs fetch -q --classpath "$@" | tr ':' '\n' | grep -v -- '-sources\.jar$' | paste -sd: -; }

echo "== resolve fork $V (Scala $S3)"
LIBCP=$(cpof org.scala-native:scala3lib_native0.5_3:$S3+$V org.scala-native:javalib_native0.5_3:$V org.scala-native:auxlib_native0.5_3:$V)
PLUGIN=$(cs fetch -q --intransitive org.scala-native:nscplugin_$S3:$V)
TOOLSCP=$(cpof org.scala-native:tools_3:$V)
DOTC=(cs launch -q "org.scala-lang:scala3-compiler_3:$S3" -M dotty.tools.dotc.Main --)

echo "== scalac -> NIR"
mkdir -p "$OUT/classes" "$OUT/driver-classes"
"${DOTC[@]}" -Xplugin:"$PLUGIN" -classpath "$LIBCP" -d "$OUT/classes" -deprecation src/*.scala
echo "== compile driver"
"${DOTC[@]}" -classpath "$TOOLSCP" -d "$OUT/driver-classes" driver/Driver.scala
echo "== link NIR -> LLVM IR (romdata=$ROMDATA)"
{ echo "$OUT/classes"; echo "$LIBCP" | tr ':' '\n'; } | sed "s|^$OUT|$HERE/$OUT|" > "$OUT/classpath.txt"
CLANG_PATH=$(command -v "$CLANG") CLANGPP_PATH=$(command -v "$CLANG++" || command -v "$CLANG") \
java -Xmx2g -Xss16m -cp "$HERE/$OUT/driver-classes:$TOOLSCP" Driver "$OUT/sn" "$OUT/classpath.txt" > "$OUT/driver.log" 2>&1 || {
  tail -40 "$OUT/driver.log"
  exit 1
}
ls "$OUT"/sn/native/generated/*.ll >/dev/null 2>&1 || { tail -30 "$OUT/driver.log"; echo "driver produced no IR"; exit 1; }
if [[ ${AGBCC_CODEGEN_ALL:-0} == 1 || ${AGBCC_CODEGEN_ALL:-0} == true ]]; then
  if grep -l '^define ' "$OUT"/sn/native/generated/*.ll; then
    echo "whole-program agbcc left method bodies in LLVM IR"
    exit 1
  fi
fi
if grep -q "not ROM-resident" "$OUT/driver.log"; then grep -A10 "not ROM-resident" "$OUT/driver.log" | head -20; echo "romdata strict check failed"; exit 1; fi

echo "== clang IR -> Thumb objects; runtime"
mkdir -p "$OUT/obj"
ls "$OUT"/sn/native/generated/*.ll | xargs -P "${JOBS:-$(nproc)}" -I{} sh -c "$CLANG"' -target armv4t-none-eabi '"$ARMFL"' '"$AGBCC_FLAGS"' -Oz -fomit-frame-pointer -fvisibility=hidden -funwind-tables -fdata-sections -ffunction-sections -Wno-override-module -c "{}" -o "'"$OUT"'/obj/$(basename "{}" .ll).o"'
if [[ $AGBCC_CODEGEN_ENABLED == 1 ]]; then
  mapfile -t AGBCC_SOURCES < <(find "$OUT/sn/native/generated" -maxdepth 1 -name '*.agbc.c' -print | sort)
  ((${#AGBCC_SOURCES[@]} == 1)) || {
    echo "expected one agbcc C source, found ${#AGBCC_SOURCES[@]}"
    tail -80 "$OUT/driver.log"
    exit 1
  }
  echo "== agbcc NIR export -> Thumb object"
  # Reuse the driver assembly, where entry-stack markers are resolved.
  AGBCC_DRIVER_ASM="${AGBCC_SOURCES[0]}.s"
  AGBCC_SOURCE_SHA=$(sha256sum "${AGBCC_SOURCES[0]}" | cut -d' ' -f1)
  if [[ ! -f "$AGBCC_DRIVER_ASM" ]] || ! grep -Fxq "@ sn_agbcc_source_sha256 $AGBCC_SOURCE_SHA" "$AGBCC_DRIVER_ASM"; then
    echo "missing or stale driver assembly; rerun IR generation with the updated fork"
    exit 1
  fi
  if grep -Eq '@sn_xlat_(frameaddr|stackptr)' "$AGBCC_DRIVER_ASM"; then
    echo "driver assembly contains unresolved entry-stack markers"
    exit 1
  fi
  cp "$AGBCC_DRIVER_ASM" "$OUT/obj/agbc-module.s" || exit 1
  if [[ ${AGBCC_CODEGEN_APP:-0} == 1 || ${AGBCC_CODEGEN_APP:-0} == true ]]; then
    for symbol in gbatest_main gbatest_agbcc_app_graph _SM22gbatest.AgbccAppGraph\$D4stepiiiEPT22gbatest.AgbccAppGraph\$; do
      grep -Fq "$symbol:" "$OUT/obj/agbc-module.s" || {
        echo "expected app graph symbol in agbcc output: $symbol"
        exit 1
      }
    done
  fi
  AGBCC_ASM="$OUT/obj/agbc-module.s"
  if [[ ${AGBCC_EHABI_FRAMES:-0} == 1 || ${AGBCC_EHABI_FRAMES:-0} == true ]]; then
    python3 tools/agbcc_ehabi.py "$AGBCC_ASM" "$OUT/obj/agbc-module.ehabi.s"
    AGBCC_ASM="$OUT/obj/agbc-module.ehabi.s"
  fi
  "$ARM_AS" -mcpu=arm7tdmi -o "$OUT/obj/agbc-module.o" "$AGBCC_ASM"
fi
${P}gcc $ARMFL -c rt/crt0.s -o "$OUT/obj/crt0.o"
${P}gcc $ARMFL "${ABI_FLAGS[@]}" -Os -funwind-tables -ffunction-sections -Wall -Wno-unused-parameter \
  -c rt/rt.c -o "$OUT/obj/rt.o"
${P}gcc $ARMFL "${ABI_FLAGS[@]}" -Os -funwind-tables -c rt/eh-ehabi.c -o "$OUT/obj/eh.o"

echo "== link"
${P}gcc $ARMFL "${ABI_FLAGS[@]}" "${LINKER_FLAGS[@]}" -nostartfiles -specs=nano.specs -Wl,--gc-sections -Wl,-T,rt/gba.ld -Wl,-Map,"$OUT/testrom.map" \
  "$OUT"/obj/*.o -o "$OUT/testrom.elf" -lc -lm -lgcc
# thread guards: a single-threaded freestanding build must not reference pthreads or the C ThreadInfo at all
if ${P}nm "$OUT/testrom.elf" | grep -iE ' (pthread_|scalanative_pthread|scalanative_currentThreadInfo)'; then echo "pthread state linked"; exit 1; fi
${P}objcopy -O binary "$OUT/testrom.elf" "$OUT/testrom.gba"
python3 - "$OUT/testrom.gba" <<'EOF'
import sys; p = sys.argv[1]; b = bytearray(open(p, 'rb').read())
b[0xBD] = (-(sum(b[0xA0:0xBD]) + 0x19)) & 0xFF
b += b'\xff' * (-len(b) % 4)
open(p, 'wb').write(b)
EOF
if [[ ${AGBCC_CODEGEN:-0} == 1 || ${AGBCC_CODEGEN:-0} == true ]]; then
  echo "== link standalone agbcc export probe ROM"
  ${P}gcc $ARMFL "${ABI_FLAGS[@]}" -Os -ffreestanding -fno-builtin -ffunction-sections -fdata-sections \
    -c harness/agbc-probe.c -o "$OUT/obj/agbc-probe.o"
  ${P}gcc $ARMFL "${ABI_FLAGS[@]}" -Dmain=scalanative_testrom_main -Os -funwind-tables -ffunction-sections \
    -Wall -Wno-unused-parameter -c rt/rt.c -o "$OUT/obj/rt-probe.o"
  INSTANCE_OWNER=$(grep -l '^@"_SM19gbatest.AgbccProbe\$G8instance" = hidden constant' "$OUT"/sn/native/generated/*.ll | head -1)
  [[ -n $INSTANCE_OWNER ]] || { echo "could not find AgbccProbe singleton"; exit 1; }
  INSTANCE_OBJ="$OUT/obj/$(basename "$INSTANCE_OWNER" .ll).o"
  mapfile -t SUPPORT_OBJS < <(find "$OUT/obj" -maxdepth 1 -name '*.o' \
    ! -name 'crt0.o' ! -name 'rt.o' ! -name 'rt-probe.o' ! -name 'eh.o' ! -name 'agbc-probe.o' ! -name 'agbc-module.o' \
    ! -name "$(basename "$INSTANCE_OBJ")" -print | sort)
  ${P}gcc $ARMFL "${ABI_FLAGS[@]}" "${LINKER_FLAGS[@]}" -nostartfiles -specs=nano.specs -Wl,--gc-sections \
    -Wl,-T,rt/gba.ld -Wl,-Map,"$OUT/agbc-probe.map" \
    "$OUT/obj/crt0.o" "$OUT/obj/agbc-probe.o" "$OUT/obj/agbc-module.o" \
    "$INSTANCE_OBJ" "${SUPPORT_OBJS[@]}" "$OUT/obj/rt-probe.o" "$OUT/obj/eh.o" \
    -lc -lm -lgcc -o "$OUT/agbc-probe.elf"
  ${P}objcopy -O binary "$OUT/agbc-probe.elf" "$OUT/agbc-probe.gba"
  python3 - "$OUT/agbc-probe.gba" <<'EOF'
import sys; p = sys.argv[1]; b = bytearray(open(p, 'rb').read())
b[0xBD] = (-(sum(b[0xA0:0xBD]) + 0x19)) & 0xFF
b += b'\xff' * (-len(b) % 4)
open(p, 'wb').write(b)
EOF
fi
echo "   $(stat -c %s "$OUT/testrom.gba") bytes: $OUT/testrom.gba"
if [[ -f $OUT/agbc-probe.gba ]]; then
  echo "   $(stat -c %s "$OUT/agbc-probe.gba") bytes: $OUT/agbc-probe.gba"
fi
