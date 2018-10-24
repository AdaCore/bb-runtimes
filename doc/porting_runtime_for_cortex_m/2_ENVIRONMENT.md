# Preparing your environment

## Requirements

To follow this tutorial, you will need

 1. at least one of the boards.
 2. the [stlink tools](https://github.com/texane/stlink)
 3. a GNAT compiler for ARM that can be downloaded from the GNAT
    [Ada Community site](https://www.adacore.com/community)
 4. the GNAT run-times' BSPs from the AdaCore
    [github repository](https://github.com/AdaCore/bb-runtimes)
 5. [svd2ada](https://github.com/AdaCore/svd2ada)
 6. a SVD description file for your MCU

*Install the GNAT compiler:*

On Linux:

```
   $ tar zxvf <gnat_package>.tgz
   $ cd <gnat_package>
   $ ./doinstall
   ...
   $ export PATH=<install path>/bin:$PATH
```

On Windows, run the installer, and make sure to check that the compiler is
properly added to the PATH.

*Build/install the stlink tools*

Follow the instructions from the stlink repository, or install a pre-built
version of the tools.

Note that on Windows, a version of stlink comes pre-installed with the ARM
ELF compiler, so this step can be skipped in that case.

*Clone the bb-runtimes repository*

Make sure you have git installed, then

```
   git clone https://github.com/AdaCore/bb-runtimes
```

*Build svd2ada*

SVD2Ada is a tool that uses
[CMSIS-SVD](https://arm-software.github.io/CMSIS_5/SVD/html/index.html)
files to generate Ada bindings to Cortex-M MCU peripherals.

You will need to clone the [svd2ada
repository](https://github.com/AdaCore/svd2ada), and install a native
GNAT compiler to be able to use this tool.

Additionally, you will need the SVD file that corresponds to your MCU. The
SVD2Ada contains some examples of such file, but you may need to see with your
silicon vendor to obtain the one specific to your MCU.

Finally, note that from our experience, most of those SVD files contain errors,
so always check the generated bindings when implementing a peripheral driver.

## bb-runtimes repository organisation

The bb-runtimes repository is used to generate the projects used to build and
install the run-time libraries.

The files of interest in this repository are:
```
.
├── arm
|   ├── cortexm.py
│   └── stm32
│       ├── stm32f40x
│       │   └── svd
│       ├── stm32f429x
│       │   └── svd
│       ├── stm32f469x
│       │   └── svd
│       ├── stm32f7x
│       │   └── svd
│       └── stm32f7x9
│           └── svd
├── src/
├── support/*.py
└── build_rts.py
```

build_rts.py is the main script that will generate the run-time BSPs project directory.

The actual BSP sources are split into two main locations: the src directory and the target-specific directories.

The sources in src come in general with many variants, that are denoted via a double underscore in their name. Once installed via build_rts.py the variant part is automatically removed from the file name.

For example: `s-bbbosu__armv7m.adb` is installed as `s-bbbosu.adb` in the run-time.

The sources in the target-specific directories (such as `arm/stm32/stm32f40x`) are low-level startup code and linker scripts. The startup code includes the clock configuration.

### Check your installation

To check your installation, we'll re-generate the stm32f4 run-time:

```
$ cd bb-runtimes
$ mkdir build
$ ./build_rts.py --bsps-only --output=build --prefix=arm-eabi/lib/gnat --link stm32f4
```
This generates the proper project tree that will be used to actually build and install the run-times for the stm32f4.

At this stage, you should have
```
build
└── BSPs
    ├── ravenscar_full_stm32f4.gpr
    ├── ravenscar_sfp_stm32f4.gpr
    ├── zfp_stm32f4.gpr
    ├── README-stm32f4.txt
    ├── cortex-m
    │   ├── src
    │   │   ├── arch/
    │   │   └── gnarl/
    │   └── stm32
    │       ├── link/
    │       ├── src
    │       │   └── crt0/
    │       └── stm32f4
    │           ├── link/
    │           ├── ravenscar-full
    │           │   ├── adalib/
    │           │   ├── arch/
    │           │   ├── obj/
    │           │   └── user_srcs/
    │           ├── ravenscar-sfp
    │           │   ├── adalib/
    │           │   ├── arch/
    │           │   ├── obj/
    │           │   └── user_srcs/
    │           ├── src
    │           │   ├── crt0/
    │           │   └── gnarl/
    │           └── zfp
    │               ├── adalib/
    │               ├── arch/
    │               ├── obj/
    │               └── user_srcs/
    └── support/
```

**Important note:** in the call to build_rts.py:
* *--bsps-only* will generate only the necessary tree structure for building the run-times using a mix of sources from bb-runtimes and from the compiler itself. Without this switch, you would need both the original gnat repository (not publicly available), and the gcc sources.
* *--link* is creating symbolic links in the generated tree, as this is useful when developing a new run-time. **This won't work on Windows** as symbolic links are not supported on this platform.
* *--prefix=arm-eabi/lib/gnat* this prefix is relative to the final installation directory. When the run-time is installed in the GNAT installation directory, then gprbuild will be able to find the new run-time by its simple name.


Now let's build and install a run-time:

```
$ gprbuild -P build/BSPs/ravenscar_sfp_stm32f4.gpr
$ gprinstall -f -p -P build/BSPs/ravenscar_sfp_stm32f4.gpr --prefix=./runtimes
```

The runtimes directory should have been created at this stage, and contain the run-time library:

```
runtimes/
├── arm-eabi
│   └── lib
│       └── gnat
│           └── ravenscar-sfp-stm32f4
│               ├── adalib
│               ├── gnarl
│               ├── gnat
│               ├── ld
│               └── share
│                   └── gpr
│                       └── manifests
└── share
    └── gpr
        └── manifests
```

Note that the prefix given to gprinstall can be set to the GNAT installation directory, to ease developing applications that use the newly generated run-time.

So for example if gnat is installed in `/opt/gnat`, then doing `gprinstall -f -p -P<rtsproject> --prefix=/opt/gnat` will allow you to later on use your run-time by just specifying `gprbuild --RTS=ravenscar-sfp-mystm32`.

To use the run-times installed in other directories, the you'd have to specify the full path to the run-time explicitely: `gprbuild --RTS=/full/path/to/ravenscar-sfp-mystm32`

## creating the sources for the new run-time

Now is the time to create a new run-time, based on the 'stm32f4' run-times that come pre-installed with GNAT for ARM.

We'll name it 'mystm32' run-time for the purpose of this tutorial.

So first, let's copy the arm/stm32/stm32f40x directory as arm/stm32/mystm32.

`cp -r arm/stm32/stm32f40x arm/stm32/mystm32`

Now we'll need to edit the arm/cortexm.py file to add support for generating a run-time for 'mystm32':

```
diff --git a/arm/cortexm.py b/arm/cortexm.py
index 307ec13..412b795 100644
--- a/arm/cortexm.py
+++ b/arm/cortexm.py
@@ -314,6 +314,8 @@ class Stm32(CortexMTarget):
     def cortex(self):
         if self.mcu.startswith('stm32f4'):
             return 'cortex-m4'
+        elif self.mcu == 'mystm32':
+            return 'cortex-m4'
         elif self.mcu.startswith('stm32f7'):
             return 'cortex-m7'
         else:
@@ -350,6 +352,8 @@ class Stm32(CortexMTarget):
             self.mcu = 'stm32f7x'
         elif self.board == 'stm32f769disco':
             self.mcu = 'stm32f7x9'
+        elif self.board == 'mystm32':
+            self.mcu = 'mystm32'
         else:
             assert False, "Unknown stm32 board: %s" % self.board
```

This modification is specific to the STM32. To port the run-time for other targets, please read the class responsible for generating the BSP and adapt as necessary.

You will finally need to modify the build_rts.py script:

```
     elif target.startswith('stm32'):
         t = Stm32(target)
+    elif target == 'mystm32':
+        t = Stm32(target)
     elif target == 'openmv2':
         t = Stm32(target)
```

### generating your custom run-time

You should now be ready to generate your own run-time:

```
$ ./build_rts.py --bsps-only --output=build --prefix=arm-eabi/lib/gnat --link mystm32
```

and check that the projects and sources have been properly created:
* build/BSPs/zfp_mystm32.gpr
* build/BSPs/ravenscar_sfp_mystm32.gpr
* build/BSPs/ravenscar_full_mystm32.gpr
* BSPs/cortex-m/stm32/mystm32/...

[Next](3_STM32F469.md) - [Home](README.md)
