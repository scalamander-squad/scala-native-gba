# scala-native-gba — an unofficial Scala Native fork for Game Boy Advance targets

This is an **unofficial** fork of [Scala Native](https://github.com/scala-native/scala-native), maintained by
scalamander-squad for compiling Scala 3 to ARMv4T/Thumb for the Game Boy Advance (the `immutableemerald` project).
It is not affiliated with or endorsed by the Scala Native project, EPFL or the Scala Center.

- **Base:** Scala Native v0.5.12, plus upstream's unreleased Scala 3.9/3.10 compiler-plugin support.
- **Branch `main`:** the current toolchain (`0.5.12-gba6`), Scala Native v0.5.12 plus our commits with full history.
  Each stage is tagged: `v0.5.12-gba1` … `v0.5.12-gba6` on `main`; `v0.5.12-romdata2` and `v0.5.12-gba4opt` mark the
  original development lines of the ROM-data pass and the optimiser patches, which were later applied onto `main`.
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
- **Licence:** Apache-2.0, as upstream (see `LICENSE.md`); our changes are offered under the same licence.
- Fixes that are generally useful are intended to be proposed upstream following the project's contribution rules.
