#!/usr/bin/env python
#
# Copyright (C) 2016-2020, AdaCore
#
# Python script to build and install the embedded runtimes for bare metal
# targets.

import getopt
import os
import subprocess
import sys


def usage():
    """Script usage"""
    print("usage: install.py [--arch=arm-eabi|aarch64-elf] [--prefix=<path>]")
    print("  --arch: only build for the specified architecture")
    print("  --prefix: installation prefix for the runtimes")
    print("")
    print("By default:")
    print("  Builds and installs all targets for which a compiler is")
    print("  available. The runtimes are installed in the toolchain itself.")


ALL_BSP = {'arm-eabi': ['stm32f4', 'stm32f429disco', 'stm32f469disco',
                        'stm32f746disco', 'stm32756geval', 'stm32f769disco',
                        'samg55', 'sam4s', 'samv71', 'openmv2', 'rpi2',
                        'feather_stm32f405', 'stm32f051r8-hsi', 'nrf52832',
                        'nrf52840', 'cortex-m0', 'cortex-m0p', 'cortex-m1',
                        'cortex-m3', 'cortex-m4', 'cortex-m4f', 'cortex-m7f',
                        'cortex-m7df'
                        ],
           'aarch64-elf': ['rpi3']}


def main():
    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "", ["arch=", "prefix=", "help"])
    except getopt.GetoptError as e:
        print("error: " + str(e))
        usage()
        sys.exit(2)

    prefix = None
    arch = None

    for opt, arg in opts:
        if opt == '--help':
            usage()
            sys.exit()
        elif opt == '--arch':
            arch = arg
        elif opt == '--prefix':
            prefix = os.path.abspath(arg)

    assert arch is None or arch in ALL_BSP, "unsupported arch %s" % arch

    cmd = [sys.executable, './build_rts.py', '--build', '--force']
    if prefix is not None:
        cmd.append('--output=%s' % prefix)
    if arch is None:
        for k, v in ALL_BSP.items():
            cmd += v
    else:
        cmd += ALL_BSP[arch]

    subprocess.check_call(cmd)


if __name__ == '__main__':
    main()
