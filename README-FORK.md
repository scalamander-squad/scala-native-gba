# scala-native-gba — Scala Native for Game Boy Advance development

This is an **unofficial** fork of [Scala Native](https://github.com/scala-native/scala-native), maintained by
scalamander-squad for compiling Scala 3 to ARMv4T/Thumb for the Game Boy Advance (the `immutableemerald` project).
It is not affiliated with or endorsed by the Scala Native project, EPFL or the Scala Center.

The project goal is to let GBA developers write ordinary, idiomatic Scala and compile it through Scala Native to
freestanding ARM7TDMI code, while progressively matching agbcc's instruction selection and producing byte-identical
game ROMs. Matching output is an active compiler objective, not a capability of the current LLVM-based default.

- **Base:** upstream Scala Native `main` after v0.5.12 (version `0.5.13-SNAPSHOT`, the sbt 2 build), merged in
  with its full history. The current toolchain is **`0.5.13-gba9`** (tag `v0.5.13-gba9`, originally based on
  upstream `c25d3c520`; later upstream main commits are merged on the fork branch).
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
    deliberately narrow NIR subset (scalar and aggregate-by-value signatures, stack allocation, typed element-address
    calculations for nested struct/array paths, integer/bitwise and basic floating arithmetic operations,
    non-exceptional signed and unsigned division and remainder, integer comparisons (including unsigned 8/16-bit
    operands), pointer/reference equality,
    conversions, direct and typed indirect calls, `sizeof`, `alignof`, aggregate extraction and insertion for literal struct/array paths,
    and simple label/branch/switch control flow). Unsupported
    operations fail explicitly. Other functions still use LLVM, so this is a mixed backend experiment rather than a
    whole-program agbcc compiler. Use `.withAgbccCodegenExports(Set("symbol"))` to route only named exported C
    symbols through this backend while other functions stay on LLVM; use `.withAgbccCodegen(true)` to attempt all
    exports (unsupported NIR fails the build). Supported operations include scalar 8/16/32/64-bit integer
    signatures, integer arithmetic/bitwise operations, basic floating arithmetic and finite literals, non-exceptional
    signed and unsigned integer division and remainder, integer and floating comparisons, pointer/reference equality,
    shifts and conversions, stack allocation, typed element-address calculations for nested struct/array paths, direct
    and typed indirect calls, external scalar/aggregate global references and scalar and aggregate loads/stores, by-value aggregate
    signatures, aggregate extraction/insertion, `sizeof`/`alignof`, and label/branch/switch control flow. A Scala 3
    frontend test sends exported
    `Random()` and `SeedRng(Int)` functions with external RNG state/counter globals through NIR, linking,
    optimization/lowering, and agbcc. Their emitted `.text` bytes and relocations match an independently authored C
    oracle, as do additional integer arithmetic, all four 32-bit signed/unsigned division and remainder operations,
    pointer equality/inequality, zero-extended unsigned 8/16-bit comparisons, struct-field address calculation,
    64-bit addition, external aggregate global element addresses, single/double floating operations and a float literal,
    aggregate loads/stores and zero initialization, indirect calls, and branch fixtures. An 8-bit unsigned division check also verifies
    that operands are zero-extended before division. These checks establish a narrow real-NIR-to-agbcc path; general
    aggregate and runtime lowering, broader NIR coverage, and ROM-level
    byte identity remain active work. The option is not yet a supported route to byte-identical ROMs. Build
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
  merge and puts the verdict in the PR. ROMs are never published: `gba-testrom/out/`, `*.gba` and `*.elf` are ignored,
  and CI uploads no artifacts.
  `gba-testrom/abi/check-agbcc-abi.sh` is a separate mixed-compiler ROM check; it needs the game agbcc binary in
  `AGBCC` and verifies calls in both directions across six fixed arguments, a 64-bit value, a by-value struct,
  varargs and a struct return. Its linker suppresses EABI attribute mismatch diagnostics because agbcc and Clang
  label their object files EABI0 and EABI5 respectively; the test exercises runtime calling behavior rather than
  claiming those metadata labels agree.
- **Licence:** Apache-2.0, as upstream (see `LICENSE.md`); our changes are offered under the same licence.
- Fixes that are generally useful are intended to be proposed upstream following the project's contribution rules.
