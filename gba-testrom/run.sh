#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Runs out/testrom.gba headlessly in libmgba and exits non-zero unless the ROM's verdict is PASS.
# Env: MGBA_SRC (mGBA 0.10.x source tree, for headers)  MGBA_BUILD (its cmake build dir with libmgba.a)
#      MGBA_LIBS (libraries libmgba.a was built against; default -lpng -lz -lm -lpthread)   FRAMES (default 600)   ROM (default out/testrom.gba)
# The harness is built on first use into out/mgba-run (a minimal libmgba build: see the upstream-sync workflow).
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd); cd "$HERE"
MGBA_SRC=${MGBA_SRC:-$HOME/.local/src/mgba}; MGBA_BUILD=${MGBA_BUILD:-$HOME/.local/src/mgba-build}
ROM=${ROM:-out/testrom.gba}; FRAMES=${FRAMES:-600}; MGBA_LIBS=${MGBA_LIBS:--lpng -lz -lm -lpthread}
mkdir -p out
if [ ! -x out/mgba-run ] || [ harness/mgba-run.c -nt out/mgba-run ]; then
  cc -O2 -I"$MGBA_SRC/include" -I"$MGBA_BUILD/include" harness/mgba-run.c "$MGBA_BUILD/libmgba.a" \
     $MGBA_LIBS -o out/mgba-run
fi
out/mgba-run "$ROM" "$FRAMES"
