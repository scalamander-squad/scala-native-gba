#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Builds gba-testrom/out/testrom.gba from src/TestRom.scala with a locally published fork (~/.ivy2/local).
# Needs: cs (coursier), a JDK, clang with the ARM target, arm-none-eabi-{gcc,ld,objcopy,nm} with newlib-nano.
# Env: NATIVE_VERSION  fork version (default <baseVersion>-<localForkTag> from ../project/ScalaNativeBuildInfo.scala)
#      ROMDATA=1|0     link-time module evaluation (default 1; 0 disables the fork's romdata pass -> romdata checks FAIL)
#      DRIVER_PROPS    extra tools system properties, e.g. scalanative.interflow.inlineUnderTry=false
#      ARM_PREFIX      binutils/gcc prefix (default arm-none-eabi-)   CLANG (default clang)   OUT (default out)
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd); cd "$HERE"
BI=../project/ScalaNativeBuildInfo.scala
V=${NATIVE_VERSION:-$(sed -n 's/.*baseVersion = "\(.*\)".*/\1/p' $BI)-$(sed -n 's/.*localForkTag = "\(.*\)".*/\1/p' $BI)}
S3=3.9.0; OUT=${OUT:-out}; P=${ARM_PREFIX:-arm-none-eabi-}; CLANG=${CLANG:-clang}
export ROMDATA=${ROMDATA:-1} ROMDATA_OPTS=${ROMDATA_OPTS-strict=gbatest\\.Rom.*}
ARMFL="-mthumb -march=armv4t -mfloat-abi=soft"
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
java -Xmx2g -Xss16m -cp "$HERE/$OUT/driver-classes:$TOOLSCP" Driver "$OUT/sn" "$OUT/classpath.txt" > "$OUT/driver.log" 2>&1 || true
ls "$OUT"/sn/native/generated/*.ll >/dev/null 2>&1 || { tail -30 "$OUT/driver.log"; echo "driver produced no IR"; exit 1; }
if grep -q "not ROM-resident" "$OUT/driver.log"; then grep -A10 "not ROM-resident" "$OUT/driver.log" | head -20; echo "romdata strict check failed"; exit 1; fi

echo "== clang IR -> Thumb objects; runtime"
mkdir -p "$OUT/obj"
ls "$OUT"/sn/native/generated/*.ll | xargs -P "$(nproc)" -I{} sh -c "$CLANG"' -target armv4t-none-eabi '"$ARMFL"' -Oz -fomit-frame-pointer -fvisibility=hidden -funwind-tables -fdata-sections -ffunction-sections -Wno-override-module -c "{}" -o "'"$OUT"'/obj/$(basename "{}" .ll).o"'
${P}gcc $ARMFL -c rt/crt0.s -o "$OUT/obj/crt0.o"
${P}gcc $ARMFL -Os -funwind-tables -ffunction-sections -Wall -Wno-unused-parameter -c rt/rt.c -o "$OUT/obj/rt.o"
${P}gcc $ARMFL -Os -funwind-tables -c rt/eh-ehabi.c -o "$OUT/obj/eh.o"

echo "== link"
${P}gcc $ARMFL -nostartfiles -specs=nano.specs -Wl,--gc-sections -Wl,-T,rt/gba.ld -Wl,-Map,"$OUT/testrom.map" \
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
echo "   $(stat -c %s "$OUT/testrom.gba") bytes: $OUT/testrom.gba"
