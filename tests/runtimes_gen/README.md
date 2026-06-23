<!-- Copyright (C) 2025-2026, AdaCore -->

# Runtime Generation Tests

Tests runtime generation by running `gen_rts_sources.py` and the per-arch targetizer entry points (`python -m bb_runtimes_targets_gen.targets.<arch>`) against all targets, comparing outputs with baseline if it's setup.

## Architecture

The framework uses an abstract `TestedRepository` base class with concrete implementations:

- `BbRuntimesRepository` - current bb-runtimes with Python package
- `CertifiedRtsRepository` - current certified-rts 
- Other legacy repository variants (will be removed later)

Each implementation provides:
- `get_targets_list()` - discover testable targets
- `run_assembly()` - execute gen_rts_sources.py
- `run_targetizer()` - execute the per-arch targetizer entry point (`python -m bb_runtimes_targets_gen.targets.<arch>`); legacy variants still use `build_rts.py`

## Usage

```bash
python -m tests.runtimes_gen.test_runner --help
```

## How it works

For each target:
1. Run gen_rts_sources.py to generate source trees
2. Run the per-arch targetizer entry point to produce the runtime sources
3. Diff the output against baseline

Output goes to `/tmp/runtimes_gen_test_XXXXXXXX/` with baseline/ and candidate/ subdirs.
And each has a `assembly/` dir, with the intermediary extracted sources and a `targetized_runtimes\`
dir.

