<!-- Copyright (C) 2025-2026, AdaCore -->

# Runtime Generation Tests

Tests runtime generation by running the per-arch prebuilder entry points
(`python -m bb_runtimes_targets_gen.targets.<arch>`) against all targets,
comparing outputs with baseline if it's setup.

## Architecture

The framework uses an abstract `TestedRepository` base class with concrete implementations:

- `BbRuntimesRepository` - current bb-runtimes with Python package
- `CertifiedRtsRepository` - current certified-rts
- Legacy repository variants (`LegacyTwoStepRepository` subclasses), used only
  to generate baselines with the historical two-step pipeline
  (`gen_rts_sources.py` assembly descriptor + `build_rts.py`)

Each implementation provides:
- `get_targets_list()` - discover testable targets
- `run_prebuilder()` - execute the per-arch prebuilder entry point in one step,
  with `--gcc-dir`/`--gnat-dir` pointing at the toolchain source trees

## Usage

```bash
python -m tests.runtimes_gen.test_runner --help
```

## How it works

For each target:
1. Run the per-arch prebuilder entry point to produce the runtime sources
2. Diff the output against baseline

Output goes to `/tmp/runtimes_gen_test_XXXXXXXX/` with baseline/ and candidate/
subdirs. The candidate side only has a `runtimes/` dir; the baseline
side also has an `assembly/` dir with the legacy pipeline's intermediary
extracted sources.
