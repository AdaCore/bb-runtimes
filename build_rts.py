#! /usr/bin/env python3
#
# Copyright (C) 2016-2020, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support.files_holder import FilesHolder
from support.bsp_sources.installer import Installer
from support.docgen import docgen

# PikeOS
from pikeos import ArmPikeOS, ArmPikeOS42, ArmPikeOS5

# Cortex-M runtimes
from arm.cortexm import Stm32, Sam, SmartFusion2, LM3S, Microbit, \
     NRF52840, NRF52832, MicrosemiM1, \
     CortexM0, CortexM0P, CortexM1, CortexM3, CortexM4, CortexM4F, \
     CortexM7F, CortexM7DF

# Cortex-A/R runtimes
from arm.cortexar import TMS570, Rpi2, Rpi2Mc, Zynq7000

# Aarch64
from aarch64 import Rpi3, Rpi3Mc, ZynqMP

# leon
from sparc import Leon2, Leon3, Leon4

# m68k
from m68k import M68020, M68020_SoftFloat

# powerpc
from powerpc import MPC8641, MPC8349e, P2020, P5566, P5634

# riscv
from riscv import Spike, Unleashed, HiFive1, PicoRV32, RV32IMC

# visium
from visium import Visium

# native
from native import X86Native, X8664Native

import argparse
import os
import subprocess
import sys


def build_configs(target):
    # PikeOS
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'arm-pikeos4.2':
        t = ArmPikeOS42()
    elif target == 'arm-pikeos5':
        t = ArmPikeOS5()
    # AArch64 elf
    elif target == 'rpi3':
        t = Rpi3()
    elif target == 'rpi3mc':
        t = Rpi3Mc()
    elif target == 'zynqmp':
        t = ZynqMP()
    # ARM elf
    elif target == 'zynq7000':
        t = Zynq7000()
    elif target == 'rpi2':
        t = Rpi2()
    elif target == 'rpi2mc':
        t = Rpi2Mc()
    elif target.startswith('sam'):
        t = Sam(target)
    elif target.startswith('smartfusion2'):
        t = SmartFusion2()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target == 'feather_stm32f405':
        t = Stm32(target)
    elif target == 'openmv2':
        t = Stm32(target)
    elif target == 'tms570':
        # by default, the TMS570LS3137 HDK board
        t = TMS570('tms570ls31')
    elif target == 'tms570_sci':
        # by default, the TMS570LS3137 HDK board
        t = TMS570('tms570ls31', uart_io=True)
    elif target == 'tms570lc':
        # alias for the TMS570LC43x HDK board
        t = TMS570('tms570lc43', uart_io=True)
    elif target == 'tms570lc_dcc':
        t = TMS570('tms570lc43', uart_io=False)
    elif target == 'lm3s':
        t = LM3S()
    elif target == 'microbit':
        t = Microbit()
    elif target == 'nrf52840':
        t = NRF52840()
    elif target == 'nrf52832':
        t = NRF52832()
    elif target == "microsemi-m1":
        t = MicrosemiM1()
    elif target == 'cortex-m0':
        t = CortexM0()
    elif target == 'cortex-m0p':
        t = CortexM0P()
    elif target == 'cortex-m1':
        t = CortexM1()
    elif target == 'cortex-m3':
        t = CortexM3()
    elif target == 'cortex-m4':
        t = CortexM4()
    elif target == 'cortex-m4f':
        t = CortexM4F()
    elif target == 'cortex-m7f':
        t = CortexM7F()
    elif target == 'cortex-m7df':
        t = CortexM7DF()
    # SPARC/Leon elf
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
    # m68k elf
    elif target == 'm68020':
        t = M68020()
    elif target == 'm68020-softfloat':
        t = M68020_SoftFloat()
    # PPC elf
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
    # Visium elf
    elif target == 'mcm':
        t = Visium()
    # Risc-V
    elif target == 'spike':
        t = Spike()
    elif target == 'hifive1':
        t = HiFive1()
    elif target == 'unleashed':
        t = Unleashed()
    elif target == 'picorv32':
        t = PicoRV32()
    elif target == 'rv32imc':
        t = RV32IMC()
    # native platforms
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
        help=('Forces the installation by overwriting '
              'any pre-existing runtime.'))
    parser.add_argument(
        '--rts-src-descriptor',
        help='The runtime source descriptor file (rts-sources.json)')
    parser.add_argument(
        '--gen-doc', action="store_true",
        help='Generate the documentation')
    parser.add_argument(
        '-o', '--output', default='install',
        help='Where built runtimes will be installed')
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
        help='List of target boards to generate runtimes for')
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
                "cannot generate rts doc for different compiler targets"

        doc_dir = os.path.join(dest, 'doc')
        docgen(boards, target, doc_dir)
        # and do nothing else
        return

    if not os.path.exists(dest):
        os.makedirs(dest)

    # Install the runtimes sources
    projects = []
    for board in boards:
        print("install runtime sources for %s" % board.name)
        sys.stdout.flush()
        installer = Installer(board)
        projects += installer.install(
            dest, rts_descriptor=args.rts_src_descriptor)

    # and build them
    if args.build:
        for prj in projects:
            print("building project %s" % prj)
            sys.stdout.flush()
            cmd = ['gprbuild', '-j0', '-p', '-P', prj]
            if args.build_flags is not None:
                cmd += args.build_flags.split()
            subprocess.check_call(cmd)
            # Post-process: remove build artifacts from obj directory
            cleanup_ext = ('.o', '.ali', '.stdout', '.stderr', '.d', '.lexch')
            obj_dir = os.path.join(os.path.dirname(prj), 'obj')
            for fname in os.listdir(obj_dir):
                _, ext = os.path.splitext(fname)
                if ext in cleanup_ext:
                    os.unlink(os.path.join(obj_dir, fname))

    print("runtimes successfully installed in %s" % dest)


if __name__ == '__main__':
    main()
