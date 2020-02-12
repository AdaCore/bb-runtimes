#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support.files_holder import FilesHolder
from support.bsp_sources.installer import Installer
from support.docgen import docgen

# PikeOS
from pikeos import ArmPikeOS, PpcPikeOS, X86PikeOS

# Cortex-M runtimes
from arm.cortexm import Stm32, Sam, SmartFusion2, LM3S

# Cortex-A/R runtimes
from arm.cortexar import TMS570, Rpi2, Rpi2Mc, Zynq7000

# Aarch64
from aarch64 import Rpi3, Rpi3Mc, ZynqMP

# leon
from sparc import Leon2, Leon3, Leon4

# powerpc
from powerpc import MPC8641, MPC8349e, P2020, P5566, P5634

# riscv
from riscv import Spike

# visium
from visium import Visium

# native
from native import X86Native, X8664Native

import argparse
import os
import subprocess
import sys


def build_configs(target):
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'ppc-pikeos':
        t = PpcPikeOS()
    elif target == 'x86-pikeos':
        t = X86PikeOS()
    elif target == 'zynq7000':
        t = Zynq7000()
    elif target == 'rpi2':
        t = Rpi2()
    elif target == 'rpi2mc':
        t = Rpi2Mc()
    elif target == 'rpi3':
        t = Rpi3()
    elif target == 'rpi3mc':
        t = Rpi3Mc()
    elif target == 'zynqmp':
        t = ZynqMP()
    elif target.startswith('sam'):
        t = Sam(target)
    elif target.startswith('smartfusion2'):
        t = SmartFusion2()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target == 'openmv2':
        t = Stm32(target)
    elif target == 'tms570':
        # by default, the TMS570LS3137 HDK board
        t = TMS570('tms570ls31')
    elif target == 'tms570_sci':
        # by default, the TMS570LS3137 HDK board
        t = TMS570('tms570ls31', uart_io=True)
    elif target == 'a6mc':
        # alias for the LaunchPad TMS570LC43x board
        t = TMS570('tms570lc43')
    elif target == 'a6mc_sci':
        t = TMS570('tms570lc43', uart_io=True)
    elif target == 'lm3s':
        t = LM3S()
    elif target == 'leon2' or target == 'leon':
        t = Leon2()
    elif target == 'leon3':
        t = Leon3(smp=False)
    elif target == 'leon3-smp':
        t = Leon3(smp=True)
    elif target == 'leon4':
        t = Leon4(smp=False)
    elif target == 'leon4-smp':
        t = Leon4(smp=True)
    elif target == 'mpc8641':
        t = MPC8641()
    elif target == '8349e':
        t = MPC8349e()
    elif target == 'p2020':
        t = P2020()
    elif target == 'p5566':
        t = P5566()
    elif target == 'mpc5634':
        t = P5634()
    elif target == 'mcm':
        t = Visium()
    elif target == 'spike':
        t = Spike()
    elif target == 'x86-linux':
        t = X86Native()
    elif target == 'x86-windows':
        t = X86Native()
    elif target == 'x86_64-linux':
        t = X8664Native()
    elif target == 'x86_64-windows':
        t = X8664Native()
    else:
        print('Error: undefined target %s' % target)
        sys.exit(2)

    return t


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        '-v', '--verbose', action="store_true",
        help='Verbose output')
    parser.add_argument(
        '-f', '--force', action="store_true",
        help='Forces the installation: overwrite any pre-existing runtime.')
    parser.add_argument(
        '--rts-src-descriptor',
        help='The runtime source descriptor file (rts-sources.json)')
    parser.add_argument(
        '--gen-doc', action="store_true",
        help='Generate the documentation')
    parser.add_argument(
        '-o', '--output', default='install',
        help='Prefix for the installation')
    parser.add_argument(
        '-l', '--link', action="store_true",
        help="Use symlinks instead of copies when installing")
    parser.add_argument(
        '-b', '--build', action="store_true",
        help="Build the runtimes")
    parser.add_argument(
        '--build-flags', help="Flags passed to gprbuild")
    parser.add_argument(
        'target', nargs='+',
        help='The target board(s) for which to generate runtimes')
    args = parser.parse_args()

    if args.verbose:
        FilesHolder.verbose = True
    if args.link:
        FilesHolder.link = True
    if args.force:
        Installer.overwrite = True

    boards = []

    for arg in args.target:
        board = build_configs(arg)
        boards.append(board)

    dest = os.path.abspath(args.output)
    if not os.path.exists(dest):
        os.makedirs(dest)

    # README file generation
    if args.gen_doc:
        # figure out the target
        target = boards[0].target
        for board in boards:
            assert target == board.target, \
                "cannot generate rts doc for mixed cross compilers"

        doc_dir = os.path.join(dest, 'doc')
        docgen(boards, target, doc_dir)
        # and do nothing else
        return

    if not os.path.exists(dest):
        os.makedirs(dest)

    # Install the runtimes
    projects = []
    for board in boards:
        print("install runtime sources for %s" % board.name)
        installer = Installer(board)
        projects += installer.install(
            dest, rts_descriptor=args.rts_src_descriptor)

    # and build them
    if args.build:
        for prj in projects:
            cmd = ['gprbuild', '-j0', '-p', '-P', prj]
            if args.build_flags is not None:
                cmd += args.build_flags.split()
            subprocess.check_call(cmd)

    print("runtimes successfully installed in %s" %
          os.path.relpath(dest, os.getcwd()))


if __name__ == '__main__':
    main()
