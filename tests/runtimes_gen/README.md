<!-- Copyright (C) 2025-2026, AdaCore -->

# Generating runtimes

For quick runtime generation for testing purposes, run this command from the
repository root with Python 3.12 or later:

```sh
make gen-runtimes
```

Make calls the package CLI with `--all` and imports both support packages
from this checkout through `PYTHONPATH`. It requires no package installation
or tox environment. With `--all`, the CLI generates every concrete target
and up to five variants per generator. It selects variants at equal intervals
across the list, including the first and last variants. Explicit target names
bypass sampling. Each runtime stays under `generated/<runtime-name>/`.
Existing runtimes with the same name are overwritten. Generation stops at
the first error. Source files use symlinks by default. To copy files instead:

```sh
make gen-runtimes CLI_ARGS='cortex-m0' USE_SYMLINKS=false
```

To generate selected targets, replace the default arguments:

```sh
make gen-runtimes CLI_ARGS='cortex-m0 rpi3mc'
```

The package also installs the `prebuild_rts` command. To call the same CLI
from this checkout without installation:

```sh
PYTHONPATH="$PWD/support/rts_prebuilder_project:$PWD/support/bb_runtimes_targets_gen" \
    python3 -m bb_runtimes_targets_gen.targets --all \
    --output-dir generated --overwrite --source-search-path src
```

`tox -e runtimes` runs the same CLI in an isolated environment. Runtime
generation remains outside the default tox environments because it needs
runtime source checkouts.

## Source directories

Make supplies this checkout's `src` directory. The CLI also uses sibling
`../gnat` and `../gcc` directories when they exist. To supply additional
source directories through Make, set `SOURCE_SEARCH_PATHS` to a space-separated list:

```sh
make gen-runtimes SOURCE_SEARCH_PATHS='/path/to/gnat /path/to/gcc'
```

Each entry becomes a `--source-search-path` argument. Direct CLI calls
can use `--gnat-dir`, `--gcc-dir`, or repeated `--source-search-path` arguments.
