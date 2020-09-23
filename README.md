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

```
./build_rts.py --output=temp --build <board1> <board2> ...
```

The list of supported boards is listed in build_rts.py within build_configs.

The above call with generate the runtimes for <board1> <board2> in 'temp'
and will build them, assuming the proper compiler is in the PATH.

To install the runtime in the compiler's default location, you will thus
need to specify

```
./build_rts.py --output <gnat_prefix>/<target>/lib/gnat` ...
```

So for example --output /opt/gnat/arm-eabi/lib/gnat

## rebuild of a runtime

To build a runtime with non default options, use the project file present in
the runtime folder: runtime_build.gpr for zfp runtimes and ravenscar_build.gpr
for ravenscar runtimes.

To build with debug options, use -XBUILD=Debug ; to build with assertions use
-XBUILD=Assert.

So for example to rebuild the ravenscar-sfp-stm32f4 runtime with debug
information, assuming GNAT is installed in ~/install/gnat, run the following:

```
gprbuild -P ~/install/gnat/arm-eabi/lib/gnat/ravenscar-sfp-stm32f4/ravenscar-build.gpr -XBUILD=Debug
```