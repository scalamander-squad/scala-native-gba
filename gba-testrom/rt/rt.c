/* SPDX-License-Identifier: Apache-2.0
 * gba-testrom runtime: the smallest runtime the fork's freestanding mode (armv4t-none-eabi, GC.none,
 * single-threaded, libraryStatic) needs on a GBA:
 *  - main(): ScalaNativeInit, then the exported Scala entry gbatest_main, then the verdict;
 *  - check reporting over the mGBA debug log (0x04FFF600 string, 0x04FFF700 flags, 0x04FFF780 enable) and a
 *    result block at 0x0203FFF0 (top of EWRAM, read by harness/mgba-run);
 *  - a bump allocator for the scalanative_GC_* API (never collects; the test program allocates a few KB);
 *  - the scalanative_* hooks the IR references. Deliberately NOT provided: any pthread or C ThreadInfo symbol,
 *    so a regression of the fork's single-threaded/freestanding thread guards fails the link. */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define REG_DEBUG_ENABLE (*(volatile uint16_t *)0x04FFF780)
#define REG_DEBUG_FLAGS  (*(volatile uint16_t *)0x04FFF700)
#define REG_DEBUG_STRING ((volatile char *)0x04FFF600)
/* result block (the 16 bytes the linker script leaves at the top of EWRAM): magic, passed, failed,
 * verdict (0x50415353 "PASS" / 0x4641494C "FAIL") */
#define TR_RESULT ((volatile uint32_t *)0x0203FFF0)
#define TR_MAGIC 0x54524F4Du   /* "TROM" */

void gba_debug(const char *s) {
  int i = 0;
  REG_DEBUG_ENABLE = 0xC0DE;
  for (; s[i] && i < 255; i++) REG_DEBUG_STRING[i] = s[i];
  REG_DEBUG_STRING[i] = 0;
  REG_DEBUG_FLAGS = 0x100 | 2;   /* level: warn */
}
__attribute__((noreturn)) void gba_halt(const char *why) {
  gba_debug(why); gba_debug("TESTROM FAIL (halted)");
  TR_RESULT[3] = 0x4641494Cu;
  for (;;) {}
}

static char line[200]; static int len;
static void put(const char *s) { while (*s && len < 199) line[len++] = *s++; }
static void puti(int32_t v) {
  char t[12]; int n = 0; uint32_t u = v < 0 ? -(uint32_t)v : (uint32_t)v;
  if (v < 0) put("-");
  do { t[n++] = '0' + u % 10; u /= 10; } while (u);
  while (n && len < 199) line[len++] = t[--n];
}
static void puthex(uint32_t v) { put("0x"); for (int s = 28; s >= 0; s -= 4) { char c[2] = {"0123456789abcdef"[(v >> s) & 15], 0}; put(c); } }
static void flush(void) { line[len] = 0; gba_debug(line); len = 0; }

static uint32_t passed, failed;
void tr_check(const char *name, int ok) {
  if (ok) passed++; else failed++;
  put(ok ? "PASS " : "FAIL "); put(name); flush();
  TR_RESULT[1] = passed; TR_RESULT[2] = failed;
}
void tr_value(const char *name, int v) { put("info "); put(name); put(" = "); puthex((uint32_t)v); flush(); }
int tr_opaque(int v) { __asm__ volatile("" : "+r"(v)); return v; }

/* ---- allocator: bump pointer over EWRAM (linker symbols __heap_start .. __heap_end), 8-byte aligned ---- */
extern char __heap_start[], __heap_end[];
static char *hp = __heap_start;
static void *bump(size_t size) {
  size = (size + 7) & ~(size_t)7;
  if (size > (size_t)(__heap_end - hp)) gba_halt("halt: out of heap");
  void *p = hp; hp += size; memset(p, 0, size); return p;
}
/* 32-bit class record: { { cls, id, interfacesCount, interfaces, name }, size, ... } */
typedef struct { void *cls; int32_t id, interfacesCount; void *interfaces, *name; int32_t size; } Rtti;
void *scalanative_GC_alloc(void *info, size_t size) { void **o = bump(size); o[0] = info; return o; }
void *scalanative_GC_alloc_small(void *info, size_t size) { return scalanative_GC_alloc(info, size); }
void *scalanative_GC_alloc_large(void *info, size_t size) { return scalanative_GC_alloc(info, size); }
void *scalanative_GC_alloc_array(void *info, size_t length, size_t stride) {
  /* the fork's 32-bit layout: an array class's size is the element offset (12, or 16 for 8-byte elements) */
  uint32_t *a = scalanative_GC_alloc(info, (size_t)((Rtti *)info)->size + length * stride);
  a[1] = length; a[2] = stride; return a;
}
void scalanative_GC_init(void) {}
void scalanative_GC_collect(void) {}
void scalanative_GC_yield(void) {}
void scalanative_GC_set_mutator_thread_state(int s) {}
void scalanative_GC_add_roots(void *a, void *b) {}
void scalanative_GC_remove_roots(void *a, void *b) {}
void scalanative_GC_set_weak_references_collected_callback(void *cb) {}
size_t scalanative_GC_get_init_heapsize(void) { return __heap_end - __heap_start; }
size_t scalanative_GC_get_max_heapsize(void) { return __heap_end - __heap_start; }
size_t scalanative_GC_get_used_heapsize(void) { return hp - __heap_start; }
size_t scalanative_GC_stats_collection_total(void) { return 0; }
size_t scalanative_GC_stats_collection_duration_total(void) { return 0; }

/* ---- runtime hooks (single thread, no OS) ---- */
static void *curThread;
void *scalanative_currentThread(void) { return curThread; }
void scalanative_assignCurrentThread(void *t, void *ti) { curThread = t; }
static char threadInfo[64]; void *scalanative_currentNativeThread(void) { return threadInfo; }
int scalanative_StackOverflowGuards_check(void) { return 0; }
void scalanative_StackOverflowGuards_close(void) {} void scalanative_StackOverflowGuards_reset(void) {}
void scalanative_StackOverflowGuards_setup(int m) {}
long long scalanative_current_time_millis(void) { return 0; }
long long scalanative_current_time_nanos(void) { return 0; }
void scalanative_set_os_props(void (*f)(const char *, const char *)) {}
static int errno_v; int scalanative_errno(void) { return errno_v; } void scalanative_set_errno(int e) { errno_v = e; }
int scalanative_erange(void) { return 34; }
int scalanative_eacces(void) { return 13; } int scalanative_eexist(void) { return 17; } int scalanative_enoent(void) { return 2; }
int scalanative_enotdir(void) { return 20; } int scalanative_enotempty(void) { return 39; }
int scalanative_stdin_fileno(void) { return 0; } int scalanative_stdout_fileno(void) { return 1; } int scalanative_stderr_fileno(void) { return 2; }
/* referenced by StackTrace but unreachable on a freestanding target (empty stack traces) */
size_t scalanative_unwind_sizeof_context(void) { return 64; } size_t scalanative_unwind_sizeof_cursor(void) { return 64; }
int scalanative_unwind_get_proc_name_by_ip(void *ip, char *buf, size_t n, void *off) { return -1; }
int scalanative_atomic_memory_order_acquire(void) { return 2; } int scalanative_atomic_memory_order_release(void) { return 3; }
intptr_t scalanative_atomic_load_explicit_intptr(intptr_t *p, int o) { return *p; }
void scalanative_atomic_store_explicit_intptr(intptr_t *p, intptr_t v, int o) { *p = v; }
long long scalanative_atomic_load_llong(long long *p) { return *p; }
const char *snFatalErrorPrefix = "fatal: ";
static char *envp[1] = {0}; char **environ = envp;
void *scalanative_memmem(const void *h, size_t hn, const void *n, size_t nn) {
  if (!nn) return (void *)h;
  for (const char *p = h; nn <= hn && p + nn <= (const char *)h + hn; p++) if (!memcmp(p, n, nn)) return (void *)p;
  return 0;
}
/* newlib-nano syscalls: stdout/stderr go to the debug log, everything else is absent */
int _write(int fd, const void *b, size_t n) {
  const char *s = b;
  for (size_t i = 0; i < n; i++) { if (s[i] == '\n' || len == 199) { flush(); if (s[i] == '\n') continue; } line[len++] = s[i]; }
  return n;
}
int write(int fd, const void *b, size_t n) { return _write(fd, b, n); }
int _read(int fd, void *b, size_t n) { return 0; } int _close(int fd) { return -1; } long _lseek(int fd, long o, int w) { return 0; }
int _fstat(int fd, void *st) { return -1; } int _isatty(int fd) { return 1; } int _getpid(void) { return 1; } int _kill(int p, int s) { return -1; }
void *_sbrk(ptrdiff_t n) { gba_halt("halt: malloc/_sbrk called"); }
void _exit(int c) { gba_halt("halt: exit called"); }
void abort(void) { gba_halt("halt: abort called"); }

extern int ScalaNativeInit(void);
extern int gbatest_main(void);
int main(void) {
  TR_RESULT[0] = TR_MAGIC; TR_RESULT[1] = TR_RESULT[2] = TR_RESULT[3] = 0;
  gba_debug("TESTROM start");
  int init = ScalaNativeInit();
  tr_check("runtime.init", init == 0);
  gbatest_main();
  put("heap used "); puti(hp - __heap_start); put(" B"); flush();
  put("TESTROM "); put(failed == 0 && passed > 0 ? "PASS" : "FAIL"); put(" ("); puti(passed); put(" passed, "); puti(failed); put(" failed)"); flush();
  TR_RESULT[3] = failed == 0 && passed > 0 ? 0x50415353u : 0x4641494Cu;
  return 0;
}
