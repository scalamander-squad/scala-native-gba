# scala-native-gba — an unofficial Scala Native fork for Game Boy Advance targets

This is an **unofficial** fork of [Scala Native](https://github.com/scala-native/scala-native), maintained by
scalamander-squad for compiling Scala 3 to ARMv4T/Thumb for the Game Boy Advance (the `immutableemerald` project).
It is not affiliated with or endorsed by the Scala Native project, EPFL or the Scala Center.

- **Base:** upstream Scala Native `main` after v0.5.12 (version `0.5.13-SNAPSHOT`, the sbt 2 build), merged in
  with its full history. The current toolchain is **`0.5.13-gba8`** (tag `v0.5.13-gba8`).
- **Branch `main`:** our commits on top of v0.5.12, with upstream merged in by merge commits. Each stage is tagged:
  `v0.5.12-gba1` … `v0.5.12-gba6` (on v0.5.12), then `v0.5.13-gba7` (the first upstream merge), `v0.5.13-gba8` (upstream catch-up; the scalafmt reformat in the "main branch and stage tags" commit is formatting only); `v0.5.12-romdata2`
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
  opens or updates an issue labelled `upstream-sync`. Upstream merges often touch `.github/workflows/**`, which
  `GITHUB_TOKEN` cannot push, so add the repository secret `UPSTREAM_SYNC_TOKEN` (PAT with `repo` and `workflow` scopes);
  without it a rejected push becomes an issue. Device checks and the `-gbaN` version bump are done locally after merging the PR.
- **Licence:** Apache-2.0, as upstream (see `LICENSE.md`); our changes are offered under the same licence.
- Fixes that are generally useful are intended to be proposed upstream following the project's contribution rules.
