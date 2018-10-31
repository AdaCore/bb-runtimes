# bb-runtimes

This repository is used to generate the runtime source trees for AdaCore's
bare metal targets.

## dependencies

This repository depends on both gcc and gnat repositories to generate a full
runtime. However it can also use the sources from an installed compiler and
rely on those to just generate the BSP part of the runtime.

## generation of BSPs

```
./build-rts.py --output=temp --bsps-only <board1> <board2> ...
```

The list of supported boards is listed in build-rts.py within build_configs.

## building and installing a runtime

Once a BSP is generated, make sure you have setup a GNAT compiler for the
board's target, and call gprbuild/gprinstall

```
gprbuild -P temp/BSPs/<rts_project>.gpr -j0 -f
gprinstall -P temp/BSPs/<rts_project>.gpr -p -f
```

where -P specified the project file, -j0 specifies to build using all CPUs
available on the host, -f forces a full project build.

## rts with debug information

To build a runtime with debug information, you can set the scenario variable
BUILD to Debug:

```
gprbuild -P temp/BSPs/<rts_project>.gpr -j0 -XBUILD=Debug -f
```
