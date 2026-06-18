# bb-runtimes

This repository is used to generate the runtime source trees for AdaCore's
bare metal targets.

## dependencies

This repository depends on both gcc and gnat repositories to generate a full
runtime. However it can also use the sources from an installed compiler and
rely on those to just generate the BSP part of the runtime.

You need GNAT Pro 21.0w 20200327 or GNAT Community 2020 minimum to use
this repository to generate runtimes.

## generation of runtimes

Runtimes are generated per architecture. Each target family is its own
runnable module; pass one or more board names (their `cli_name`):

```
# List the boards an architecture supports
python -m bb_runtimes_targets_gen.targets.<arch> --list-targets

# Generate the runtimes for some boards into ./temp
python -m bb_runtimes_targets_gen.targets.<arch> <board1> <board2> --output-dir=temp
```

To install into the compiler's default location, point `--output-dir` at it,
e.g. `--output-dir /opt/gnat/arm-eabi/lib/gnat`.

Each generated runtime ships a `build.py` that compiles it; run it after
generation (assuming the proper compiler is in the PATH). See
`support/rts_prebuilder_project/docs` for the full flow and
`python -m bb_runtimes_targets_gen.targets.<arch> --help` for all options.

## rebuild of a runtime

To build a runtime with non default options, use the project file present in
the runtime folder: runtime_build.gpr for the Light runtimes and
ravenscar_build.gpr for Tasking and Embedded runtimes.

To build with debug options, use -XBUILD=Debug ; to build with assertions use
-XBUILD=Assert.

So for example to rebuild the ravenscar-sfp-stm32f4 runtime with debug
information, assuming GNAT is installed in ~/install/gnat, run the following:

```
gprbuild -P ~/install/gnat/arm-eabi/lib/gnat/ravenscar-sfp-stm32f4/ravenscar-build.gpr -XBUILD=Debug
```
