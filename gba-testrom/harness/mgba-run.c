/* SPDX-License-Identifier: Apache-2.0
 * gba-testrom headless runner on libmgba (0.10.x): runs a ROM for up to N frames, prints the ROM's mGBA debug-log
 * lines ("[rom] ..."), stops early once the result block has a verdict, and exits 0 only on a PASS verdict.
 * Result block (rt/rt.c): 0x0203FFF0 magic "TROM", +4 passed, +8 failed, +12 verdict ("PASS"/"FAIL").
 * Build: cc -O2 -I<mgba>/include -I<mgba-build>/include mgba-run.c <mgba-build>/libmgba.a -lz -lpng -lm -lpthread
 * Usage: mgba-run rom.gba [frames]      exit: 0 PASS, 1 FAIL/no verdict, 2 usage/load error */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mgba/core/core.h>
#include <mgba/core/log.h>
#include <mgba/core/config.h>

#define RESULT 0x0203FFF0u
static int otherLines;

static void logCb(struct mLogger* l, int category, enum mLogLevel level, const char* fmt, va_list args) {
	(void) l; (void) level;
	char buf[512];
	vsnprintf(buf, sizeof buf, fmt, args);
	if (strstr(mLogCategoryName(category), "Debug")) printf("[rom] %s\n", buf);
	else if (otherLines++ < 20) printf("[mgba %s] %s\n", mLogCategoryName(category), buf);
	fflush(stdout);
}

int main(int argc, char** argv) {
	if (argc < 2) { fprintf(stderr, "usage: %s rom.gba [frames]\n", argv[0]); return 2; }
	int frames = argc > 2 ? atoi(argv[2]) : 600;
	struct mLogger logger = { .log = logCb, .filter = NULL };
	mLogSetDefaultLogger(&logger);
	struct mCore* core = mCoreFind(argv[1]);
	if (!core) { fprintf(stderr, "not a ROM mGBA recognises: %s\n", argv[1]); return 2; }
	core->init(core);
	mCoreInitConfig(core, "headless");
	unsigned w, h;
	core->desiredVideoDimensions(core, &w, &h);
	color_t* video = calloc((size_t) w * h, BYTES_PER_PIXEL);
	core->setVideoBuffer(core, video, w);
	if (!mCoreLoadFile(core, argv[1])) { fprintf(stderr, "load failed: %s\n", argv[1]); return 2; }
	core->reset(core);
	int f = 0;
	uint32_t verdict = 0;
	for (; f < frames; ++f) {
		core->runFrame(core);
		verdict = core->busRead32(core, RESULT + 12);
		if (core->busRead32(core, RESULT) == 0x54524F4Du && verdict) break;
	}
	uint32_t magic = core->busRead32(core, RESULT), passed = core->busRead32(core, RESULT + 4), failed = core->busRead32(core, RESULT + 8);
	int pass = magic == 0x54524F4Du && verdict == 0x50415353u && failed == 0 && passed > 0;
	printf("result block: magic=%08x passed=%u failed=%u verdict=%08x after %d frames\n", magic, passed, failed, verdict, f + 1);
	printf("VERDICT: %s\n", pass ? "PASS" : magic != 0x54524F4Du ? "FAIL (runtime never started)" : verdict ? "FAIL" : "FAIL (no verdict: hung or crashed)");
	core->deinit(core);
	free(video);
	return pass ? 0 : 1;
}
