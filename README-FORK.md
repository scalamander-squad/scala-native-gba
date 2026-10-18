# scala-native-gba — Scala Native for Game Boy Advance development

This is an **unofficial** fork of [Scala Native](https://github.com/scala-native/scala-native), maintained by
scalamander-squad for compiling Scala 3 to ARMv4T/Thumb for the Game Boy Advance (the `immutableemerald` project).
It is not affiliated with or endorsed by the Scala Native project, EPFL or the Scala Center.

The project goal is to let GBA developers write ordinary, idiomatic Scala and compile it through Scala Native to
freestanding ARM7TDMI code, while progressively matching agbcc's instruction selection and producing byte-identical
game ROMs. Matching output is an active compiler objective, not a capability of the current LLVM-based default.

- **Base:** upstream Scala Native `main` after v0.5.12 (version `0.5.13-SNAPSHOT`, the sbt 2 build), merged in
  with its full history. The current working build version is **`0.5.13-gba10`** (local, not yet tagged; originally
  based on upstream `c25d3c520`, with later upstream main commits merged on the fork branch).
- **Branch `main`:** our commits on top of v0.5.12, with upstream merged in by merge commits. Each stage is tagged:
  `v0.5.12-gba1` … `v0.5.12-gba6` (on v0.5.12), then `v0.5.13-gba7` (the first upstream merge),
  `v0.5.13-gba8` (upstream catch-up; the scalafmt reformat in the "main branch and stage tags" commit is formatting
  only), and `v0.5.13-gba9` (the next upstream sync plus the GBA headless test ROM and its weekly merge workflow);
  `v0.5.12-romdata2`
  and `v0.5.12-gba4opt` mark the original development lines of the ROM-data pass and the optimiser patches, which
  were later applied onto `main`.
- **What the patches do** (each commit describes one change):
  - 32-bit fixes: `Long`/`Double` field and array alignment matching the LLVM layout on 32-bit ARM.
  - Exceptions: correct unwind edges for throws inside `try`; unwind handlers attached to polymorphic dispatch.
  - Optimiser: per-instruction try/catch handling and inlining under `try`; constant `inttoptr` folding; stable module
    fields (devirtualisation of module-level lambdas); inlining of allocation-returning callees; Scala 3 module purity;
    no specialisation on parameters a method never reads.
  - Link-time evaluation of module initialisers into ROM-resident constant object graphs (`-Dscalanative.romdata=…`),
    with a strict array-immutability analysis.
  - Freestanding runtime trims: no system property / environment / case tables, no box caches, empty stack traces
    on targets without an OS.
  - `NativeConfig.withCompileNativeLibraries(false)` lets a static-library build use a custom target runtime without
    compiling the classpath libraries' native C/assembly sources. The default remains `true`; the xlat and GBA test-ROM
    drivers opt out because they supply their own GBA runtime.
  - Single-threaded / freestanding guards (needed since upstream's threading rework): the exported thread start routine
    runs threads only when multithreading is enabled, single-threaded builds allocate no pthread state, and
    freestanding targets skip the C `ThreadInfo` writes at start-up and shutdown. Multithreaded builds are unchanged.
  - An opt-in `NativeConfig.withAgbccAbi(true)` compile profile for `armv4t-none-eabi`/`thumbv4t-none-eabi` adds
    `-mabi=apcs-gnu`. It is rejected for other targets. This selects APCS-GNU call, stack and aggregate lowering;
    for the tested non-leaf probe, it changes Clang's eight-byte frame to agbcc's four-byte frame. It does **not**
    match agbcc's instruction selection or register allocation, so it is a stepping stone for ABI-sensitive code and
    not a byte-identical code-generation mode. The profile is off by default; the GBA test ROM accepts
    `AGBCC_ABI=1` to exercise it.
  - An experimental, separate `NativeConfig.withAgbccCodegen(true)` path generates code from
    lowered Scala Native NIR through the game's actual agbcc compiler. It is opt-in, GBA-target-only, and requires
    `AGBCC` to point to agbcc; it also disables incremental compilation and LTO. The current emitter handles a
    deliberately narrow NIR subset (scalar and aggregate-by-value signatures, stack allocation, ordered volatile scalar
    accesses, nested aggregate constants with static storage, typed element-address
    calculations for nested struct/array paths, integer/bitwise and basic floating arithmetic operations,
    non-exceptional signed and unsigned division and remainder, integer comparisons (including unsigned 8/16-bit
    operands), pointer/reference equality,
    conversions and same-width float/integer bitcasts, direct and typed indirect calls, C varargs call sites, `sizeof`, `alignof`, aggregate extraction and
    insertion for literal struct/array paths,
    unordered scalar loads, and simple label/branch/switch control flow with unreachable terminals). Unsupported
    operations fail explicitly. An export filter keeps unrelated functions on LLVM; unfiltered mode selects every
    lowered method body. Use `.withAgbccCodegenExports(Set("symbol"))` to route only named exported C
    symbols through this backend together with their reachable NIR callees, including methods referenced through
    function pointers in method bodies and transitively referenced global initializers
    (including dispatch tables), while unrelated functions stay
    on LLVM; `.withAgbccCodegenExportOrder(Seq(...))` preserves the selected root definition order in the emitted
    object. With no export filter, `.withAgbccCodegen(true)` sends every lowered NIR method body through agbcc,
    including methods outside the export reference graph; unsupported NIR fails the build. Global data still uses
    LLVM's data emission, while instruction generation in this mode is entirely agbcc.
    The GBA test ROM's `AGBCC_CODEGEN_ALL=1 AGBCC_EHABI_FRAMES=1` mode enforces that no function definitions
    remain in LLVM IR. It also exercises inline stack save/restore, 32/64-bit trailing-zero counts, decimal logarithms,
    absolute values, and the freestanding runtime initialization path. Its C runtime/newlib support remains external
    native code; this mode does not claim byte identity with the original game's instructions. Supported operations include scalar 8/16/32/64-bit integer
    signatures, integer arithmetic/bitwise operations, basic floating arithmetic, finite literals and IEEE NaN/infinity,
    non-exceptional signed and unsigned integer division and remainder, integer and floating comparisons, pointer/reference equality,
    shifts, conversions and same-width float/integer bitcasts, stack allocation, compiler memory fences, ordered scalar loads/stores, typed element-address calculations for nested struct/array paths, direct
    calls, typed indirect calls, C varargs call sites, external scalar/aggregate global references, scalar and aggregate
    loads/stores, by-value aggregate
    signatures, aggregate extraction/insertion, `sizeof`/`alignof`, and label/branch/switch control flow with
    unreachable terminals lowered to a local Thumb self-loop. Scala 3
    frontend tests send exported arithmetic/control-flow functions, float bit reinterpretations/remainders, and
    Int-backed `Random()`/`SeedRng(Int)` implementations through NIR, linking, optimization/lowering, and agbcc with
    explicit `u16` C ABI signatures via `.withAgbccCodegenExportSignatures`. This preserves the narrow external
    signature while Scala keeps using Int to represent unsigned C integers. Their emitted `.text` bytes and relocations
    match an independently authored C oracle. A separate local probe against the current xlat classpath also routed its
    actual translated `Random`/`SeedRng` definitions through this backend: their generated instruction bytes match
    stock `random.o` exactly (26 bytes for `Random`, 16 for `SeedRng`). The xlat driver and build script now expose
    this path with `AGBCC_CODEGEN_EXPORTS=Random,SeedRng,SeedRng2,Random2` and `AGBCC_CODEGEN_RNG_ABI=1`; the list
    defines the C source definition order. A focused pipeline generated its agbcc object, linked the translated objects,
    and emitted the expected veneers. All four function symbols now have the same offsets and sizes as stock `random.o`,
    and the entire 116-byte `.text` section (including literal pools and padding) matches after masking relocation words.
    A subsequent complete-game relink using these four generated RNG functions and the original C data declarations
    matched every one of the stock ROM's 16,777,216 bytes, including final state addresses. Every other game translation
    unit remained stock C/assembly; this does not establish whole-game Scala parity. The xlat
    driver now passes the ARM newlib sysroot to clang and skips bundled native C/assembly compilation while using its
    project runtime; driver errors are fatal. The current pipeline reaches partial linking and emits veneers. A full
    manifest build reaches the game linker but fails on undefined functions from translations that are still in progress
    (EWRAM 98.56%, IWRAM 94.27% before link failure), so whole-ROM identity is not established. Additional emitted `.text` bytes and relocations
    match independent C oracles for integer arithmetic, all four 32-bit signed/unsigned division and remainder operations,
    pointer equality/inequality, zero-extended unsigned 8/16-bit comparisons, sign-aware narrow signed division,
    remainder, comparisons, and shifts, struct-field address calculation,
    64-bit addition, external aggregate global element addresses, single/double floating operations and a float literal,
    aggregate loads/stores and zero initialization, indirect and variadic calls, float/double remainder call emission,
    bitwise logic, all shift variants,
    32-bit float/integer and 64-bit double/long bitcasts, compiler fences, and ordered scalar accesses,
    signed/unsigned comparisons, and branch fixtures. Floating remainder emits a local fdlibm-derived shift/subtract
    helper because the classic agbcc ROM link (`-lgcc -lc`) does not provide `fmodf`/`fmod`; tests compare it with the
    host C library across 4,096 deterministic input pairs per precision plus explicit subnormal, signed-zero,
    infinity, and zero-divisor cases.
    Variadic function definitions are still rejected. Ordered loads/stores use volatile scalar accesses, including
    64-bit values, and acquire/release ordering uses compiler memory clobbers that emit no ARM7TDMI instructions.
    This path is for the fork's single-threaded GBA target and does not provide concurrent atomicity. An 8-bit unsigned division check also verifies
    that operands are zero-extended before division. The unfiltered backend now also compiles the full GBA runtime
    test program's lowered methods, with LLVM retained for global data. Broader NIR coverage and byte identity for
    the translated game as a whole remain active work. A new real-NIR probe routed translated `Sin` and `Cos` through agbcc with
    `AGBCC_CODEGEN_EXPORTS=Sin,Cos` and signed 16-bit C ABI overrides. The emitted 60-byte `.text` prefix matches
    stock `trig.o` byte-for-byte (SHA-256 `ef03c58f38d6150438b40fe3ab22a33738ca754f50d4bd4be7f5a765dd231520`),
    and both `R_ARM_ABS32 gSineTable` relocations match at offsets `0x18` and `0x38`. This exercised the
    `__xlat_addr_<symbol>` address-cell lowering, non-volatile unordered table reads, and scheduling of independent
    multiply operands ahead of the table access. It establishes exact code generation for these two functions only;
    it does not establish whole-object or ROM identity. Focused address-load tests now pass **5/5** on Scala 2.13.
    A four-export `Sin`/`Cos`/`Sin2`/`Cos2` probe confirms volatile table reads lower to inline `ldrh` instructions,
    and `Sin2` materializes its table address after the division helpers. Its bytes still differ from stock agbcc:
    `Sin2` still differs in register allocation, narrowing, branches and literal placement. The current game translation
    keeps an explicit `Cos2` call to `Sin2`, but that does not resolve the whole-object layout difference. `Sin`/`Cos`
    remain the exact trig functions. The option is not yet a supported route to
    byte-identical ROMs. Build
    environments must set `AGBCC` and provide `ARM_AS` or `arm-none-eabi-as`. The parity test is
    `toolsJVM3/Test/testOnly scala.scalanative.codegen.llvm.AgbccCodeGenTest` with `AGBCC`, `ARM_AS`,
    `ARM_OBJCOPY`, and `ARM_READELF` configured.
- **Merging upstream:** upstream is merged, never rebased: `git fetch origin main && git merge origin/main` (`origin` is the upstream repository; `squad` is this fork)
  on a branch, conflicts resolved so that upstream's change and our semantics both survive; cherry-picks of upstream
  commits we took early are dropped in favour of upstream's own version. The local version is
  `<upstream base version>-gbaN` (`localForkTag` in `project/ScalaNativeBuildInfo.scala`). Before the result goes to
  `main`: `toolsJVM3/test` on Scala 3.9.0 with `JAVA_TOOL_OPTIONS=-Xss16m` (includes our tests `ArrayEscapeTest`,
  `StableModuleFieldsTest`, `InlineUnderTryTest`, `DuplicateUnusedParamsTest`, `MemoryLayoutTest`), a local publish
  (`sbt -batch "++3.9.0; …/publishLocal; …"` — sbt 2 joins its arguments into one command line, so commands are
  `;`-separated), and the device checks in the GBA project (play ROM, lockstep equivalence against stock
  pokeemerald on the fast-forward scenarios, boot to Birch), which need a local emulator and toolchain.
- **Automatic upstream sync:** `.github/workflows/upstream-sync.yml` runs weekly (Mondays 06:00 UTC, or manually with
  `base`/`dry_run` inputs). It merges `scala-native/scala-native` `main` into `upstream-sync/<date>` (never rebase), runs
  `toolsJVM3/testFull` (Scala 3.9.0) and our test classes on `toolsJVM2_13`, and opens a PR to `main`; on conflict it
  opens or updates an issue labelled `upstream-sync`. After the tests it publishes the merge locally and runs the GBA
  test ROM (below) in headless libmgba; a failing ROM opens an issue instead of a PR. Upstream merges often touch `.github/workflows/**`, which
  `GITHUB_TOKEN` cannot push, so add the repository secret `UPSTREAM_SYNC_TOKEN` (PAT with `repo` and `workflow` scopes);
  without it a rejected push becomes an issue. The game's device checks and the `-gbaN` version bump are done locally after merging the PR.
- **GBA test ROM (`gba-testrom/`):** a small original Scala 3 program (Apache-2.0, no game code or assets) with the
  smallest runtime the freestanding mode needs (crt0, linker script, bump allocator, EHABI personality, mGBA debug-log
  output) that boots on the GBA and self-checks what the patches are for: `Long`/`Double` field and array alignment,
  exceptions caught inside `try` (also through polymorphic dispatch, division by zero, bounds, casts), inlining under
  `try`, a module evaluated at link time whose instance and object graph lie in ROM (`0x08000000`+; the link also fails
  if it is not ROM-resident), stable-module-field lambdas, strings/`StringBuilder`/boxing/collections from the trimmed
  javalib, and the thread guards (`Thread.currentThread()` works; the link fails if any pthread or C `ThreadInfo` symbol
  is referenced). Each check logs `PASS`/`FAIL <name>` to the mGBA debug log, then `TESTROM PASS`/`FAIL`; a result
  block at `0x0203FFF0` carries the verdict. Locally, after a `publishLocal`: `gba-testrom/build.sh && gba-testrom/run.sh`
  (needs `cs`, a JDK, `clang`, Arm GNU `arm-none-eabi-*` with newlib-nano, and a libmgba 0.10 build: `MGBA_SRC`,
  `MGBA_BUILD`, `MGBA_LIBS`); `run.sh` exits non-zero unless the verdict is PASS. `ROMDATA=0 gba-testrom/build.sh`
  disables the ROM-data pass and must make the romdata checks fail. The upstream-sync workflow runs it on every clean
  merge and puts the verdict in the PR. The current default LLVM ROM boots with **38 passed, 0 failed** checks.
  ROMs are never published: `gba-testrom/out/`, `*.gba` and `*.elf` are ignored,
  and CI uploads no artifacts. Locally, after a `publishLocal`, `AGBCC_CODEGEN=1 AGBCC=/path/to/agbcc JOBS=1
  gba-testrom/build.sh && ROM=out/agbc-probe.gba gba-testrom/run.sh` also builds a standalone probe ROM. It routes
  the Int-backed `gbatest_agbcc_u16` export through the agbcc backend with a `u16` C ABI override, assembles that
  generated C as a Thumb object, and checks two calls through the boundary at boot. This is a real-ROM integration
  smoke test for one exported function, not whole-ROM byte identity.
  `AGBCC_CODEGEN_GRAPH=1` routes an exported loop and its private Scala helper through agbcc; the GBA ROM executes the
  call graph and reports `PASS agbcc.app-call-graph`. The current graph-mode ROM passes all **38** runtime checks,
  including the APCS layout and EHABI exception cases.
  `AGBCC_CODEGEN_APP=1` is an opt-in diagnostic that routes the test ROM's main export and reachable NIR callees
  through agbcc; unrelated definitions remain LLVM-generated (the current build emits 26 LLVM IR modules alongside
  the agbcc translation unit). Reproduce the full-app run with `AGBCC_CODEGEN_APP=1 AGBCC_EHABI_FRAMES=1
  AGBCC=/path/to/agbcc JOBS=1 gba-testrom/build.sh && ROM=out/testrom.gba gba-testrom/run.sh`.
  The driver fixes the selected root order, and runtime class/trait IDs are assigned in canonical symbol order.
  This fixes observed run-to-run output drift from nondeterministic linker-map/set iteration. Two clean builds with
  identical inputs produced byte-identical agbcc `.text` and byte-identical 1,130,384-byte ROM images (ROM SHA-256
  `6db3e71b28237ca03335594255186852f766d040ba51212128eeef69303cece8`); mGBA passed all **47/47** checks on the
  second image. This proves reproducibility for this mixed-backend test ROM, not equality with a stock agbcc build.
  Exceptional NIR edges emit EHABI call-site and landing-pad markers for the metadata pass; without
  `AGBCC_EHABI_FRAMES=1`, the app ROM halts at the first exception because agbcc provides no unwind tables. With
  the frame/LSDA pass enabled, the rebuilt full-app ROM passes **47/47** checks. The APCS profile uses four-byte Long/Double aggregate
  alignment to match the classic toolchain (the default EABI profile retains eight-byte alignment). Coverage includes
  Long/Double fields and arrays, IEEE Double constants and arithmetic, Double array reads and boxing/string
  conversion, ROM-resident module data, and all exception cases. The focused current compiler tests pass **43/43**;
  the latest full JVM tools suite passed **145 tests, 1 ignored**. This confirms runtime behavior for the exercised app graph; matching the emitted
  instruction stream and producing a byte-identical ROM remain separate goals.
  The bundled agbcc 2.9 also emits no `.ARM.exidx`/`.ARM.extab` for a C caller with either `-fexceptions` or
  `-fsjlj-exceptions`, and its bundled `libgcc.a`/`libc.a` expose no `setjmp`/`longjmp`; exception support therefore
  needs explicit frame metadata or a runtime protocol implemented for this target. The test-ROM build has an opt-in
  `AGBCC_EHABI_FRAMES=1` pass that adds frame-only `.ARM.exidx` recipes for agbcc prologues with saved registers and
  fixed stack allocation; object checks confirm it leaves `.text` unchanged. Exceptional NIR operation edges emit marker
  labels which this pass converts to catch-all `.ARM.extab` LSDA call-site tables for `scalanative_personality`.
  The frame parser still rejects prologues it cannot describe, including frame-pointer and dynamic-stack functions.
  Run its parser tests with
  `PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s gba-testrom/tools -p 'test_*.py'`.
  `gba-testrom/abi/check-agbcc-abi.sh` is a separate mixed-compiler ROM check; it needs the game agbcc binary in
  `AGBCC` and verifies calls in both directions across six fixed arguments, a 64-bit value, a by-value struct,
  varargs and a struct return. Its linker suppresses EABI attribute mismatch diagnostics because agbcc and Clang
  label their object files EABI0 and EABI5 respectively; the test exercises runtime calling behavior rather than
  claiming those metadata labels agree.
- **Licence:** Apache-2.0, as upstream (see `LICENSE.md`); our changes are offered under the same licence.
- Fixes that are generally useful are intended to be proposed upstream following the project's contribution rules.


### ARM arithmetic and ABI validation (2026-10-18)

The agbcc emitter recognizes explicit `__xlat_sdiv32` / `__xlat_sdiv64` calls as signed C division with the game's
bundled libgcc behavior: zero divisors return zero and minimum signed value divided by -1 wraps. These markers
avoid introducing JVM division checks into code whose original C relies on that ARM behavior. A ten-check mGBA
probe includes zero, overflow, truncation and wide results; six independent fault injections fail as expected.
The application LLVM post-pass lowers these markers to the original libgcc helpers instead of LLVM `sdiv`, whose
zero/overflow cases would be poison. Applications using these markers must supply that matching runtime.

The current 132-unit game links with every lowered method through agbcc; its ordinary runtime/veneer layout is
still different from stock. Byte-identical whole-ROM generation is unfinished. The companion application's ABI
extractor derives integer signatures from original agbcc DWARF and excludes unsupported pointer/aggregate shapes.
