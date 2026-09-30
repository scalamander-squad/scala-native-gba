/* SPDX-License-Identifier: Apache-2.0
 * Scala Native exception runtime for ARM EHABI (libgcc unwinder), derived from nativelib's eh.c.
 * Differences from eh.c:
 *  - personality has the EHABI signature (state, ucb, context) and drives __gnu_unwind_frame;
 *  - r12 is set to the UCB so libgcc's _Unwind_GetLanguageSpecificData/_Unwind_GetRegionStart work;
 *  - no stdio: failures go to the mGBA debug channel and halt.
 * Scala Native only ever emits `landingpad catch ptr null` (catch-all, no cleanups, no resume),
 * so the personality only needs "first covering call-site with a landing pad wins". */
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <unwind.h>   /* arm-none-eabi-gcc's: _Unwind_Exception == _Unwind_Control_Block */

__attribute__((noreturn)) void gba_halt(const char *why);
void gba_debug(const char *s);

typedef void *Exception;
typedef void (*OnCatchHandler)(Exception);
typedef struct ExceptionWrapper { Exception obj; _Unwind_Exception unwindException; } ExceptionWrapper;
#define GetExceptionWrapper(ue) ((ExceptionWrapper *)((ue) + 1) - 1)

extern OnCatchHandler scalanative_Throwable_onCatchHandler(Exception e);
extern ExceptionWrapper *scalanative_Throwable_exceptionWrapper(Exception e);

size_t scalanative_Throwable_sizeOfExceptionWrapper(void) { return sizeof(ExceptionWrapper); }

static void Exception_cleanup(Exception self) { OnCatchHandler h = scalanative_Throwable_onCatchHandler(self); if (h) h(self); }
static void generic_exception_cleanup(_Unwind_Reason_Code code, _Unwind_Exception *ue) { Exception_cleanup(GetExceptionWrapper(ue)->obj); }

/* ---- LSDA parsing (same format clang emits on ARM EHABI: uleb128 call-site table) ---- */
typedef const uint8_t *LSDA_ptr;
static uint32_t read_uleb(LSDA_ptr *d) { uint32_t r = 0; int s = 0; uint8_t b; do { b = **d; (*d)++; r |= (uint32_t)(b & 0x7f) << s; s += 7; } while (b & 0x80); return r; }
static int32_t read_sleb(LSDA_ptr *d) { uint32_t r = 0; int s = 0; uint8_t b; do { b = **d; (*d)++; r |= (uint32_t)(b & 0x7f) << s; s += 7; } while (b & 0x80); if ((b & 0x40) && s < 32) r |= (~0u) << s; return (int32_t)r; }

typedef struct { uint32_t start, len, lp, action; } CallSite;

/* Returns landing pad address (absolute) and type index, or 0 if this frame has no handler for ip. */
static uintptr_t find_landing_pad(_Unwind_Context *ctx, uint8_t *type_index_out) {
  LSDA_ptr p = (LSDA_ptr)_Unwind_GetLanguageSpecificData(ctx);
  if (!p) return 0;
  uintptr_t func_start = _Unwind_GetRegionStart(ctx);
  uintptr_t ip = _Unwind_GetIP(ctx) - 1;
  uint8_t lpstart_enc = p[0], ttype_enc = p[1]; p += 2;
  if (lpstart_enc != 0xff) gba_halt("halt: LSDA LPStart encoding unsupported");
  if (ttype_enc != 0xff) (void)read_uleb(&p);        /* type table offset: unused */
  uint8_t cs_enc = *p++; (void)cs_enc;               /* clang: DW_EH_PE_uleb128 */
  uint32_t cs_len = read_uleb(&p);
  LSDA_ptr cs_end = p + cs_len, action_table = cs_end;
  while (p < cs_end) {
    CallSite cs; cs.start = read_uleb(&p); cs.len = read_uleb(&p); cs.lp = read_uleb(&p); cs.action = read_uleb(&p);
    if (!cs.lp) continue;
    uintptr_t s = func_start + cs.start;
    if (ip < s || ip > s + cs.len) continue;
    uint8_t ti = 0;
    if (cs.action) { LSDA_ptr a = action_table + cs.action - 1; ti = *a++; (void)read_sleb(&a); }
    *type_index_out = ti;
    return func_start + cs.lp;
  }
  return 0;
}

static _Unwind_Reason_Code continue_unwind(_Unwind_Control_Block *ucb, _Unwind_Context *ctx) {
  if (__gnu_unwind_frame(ucb, ctx) != _URC_OK) return _URC_FAILURE;
  return _URC_CONTINUE_UNWIND;
}

_Unwind_Reason_Code scalanative_personality(_Unwind_State state, _Unwind_Control_Block *ucb, _Unwind_Context *ctx) {
  bool search;
  switch (state & _US_ACTION_MASK) {
    case _US_VIRTUAL_UNWIND_FRAME:  search = true;  break;
    case _US_UNWIND_FRAME_STARTING: search = false; break;
    case _US_UNWIND_FRAME_RESUME:   return continue_unwind(ucb, ctx);
    default: gba_halt("halt: personality bad state");
  }
  /* libgcc caches the exception-table pointers in the UCB and finds it through r12 */
  _Unwind_SetGR(ctx, 12, (_Unwind_Word)ucb);
  uint8_t ti = 0;
  uintptr_t lp = find_landing_pad(ctx, &ti);
  if (!lp) return continue_unwind(ucb, ctx);
  if (search) { ucb->barrier_cache.sp = _Unwind_GetGR(ctx, 13); return _URC_HANDLER_FOUND; }
  _Unwind_SetGR(ctx, 0, (_Unwind_Word)ucb);
  _Unwind_SetGR(ctx, 1, (_Unwind_Word)ti);
  _Unwind_SetIP(ctx, lp);
  return _URC_INSTALL_CONTEXT;
}

Exception scalanative_catch(_Unwind_Exception *ue) {
  Exception e = GetExceptionWrapper(ue)->obj;
  _Unwind_Complete(ue);
  Exception_cleanup(e);
  return e;
}

__attribute__((noreturn)) void scalanative_throw(Exception obj) {
  ExceptionWrapper *w = scalanative_Throwable_exceptionWrapper(obj);
  w->unwindException.exception_cleanup = generic_exception_cleanup;
  w->obj = obj;
  _Unwind_Reason_Code code = _Unwind_RaiseException(&w->unwindException);
  if (code == _URC_END_OF_STACK) gba_halt("halt: uncaught exception (no handler found)");
  gba_halt("halt: _Unwind_RaiseException failed (missing unwind table on some frame?)");
}
