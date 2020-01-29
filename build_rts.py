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
from support.rts_sources import SourceTree
from support.rts_sources.sources import rts_scenarios, rts_sources

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

import getopt
import os
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
        print 'Error: undefined target %s' % target
        sys.exit(2)

    return t


def usage():
    print "usage: build-rts.py OPTIONS board1 board2 ..."
    print "Options are:"
    print " -v --verbose      be verbose"
    print " --bsps-only       generate only the BSPs"
    print " --gen-doc         generate the runtime documentation"
    print " --output=DIR      where to generate the source tree"
    print " --prefix=DIR      where built rts will be installed."
    print " --gcc-dir=DIR     gcc source directory"
    print " --gnat-dir=DIR    gnat source directory"
    print " --link            create symbolic links"
    print ""
    print "By default, the build infrastructure is performed in:"
    print "  $PWD/install:                 default output"
    print "  <output>/BSPs:                The bsps"
    print "  <output>/lib/gnat:            projects for the shared runtime"
    print "  <output>/include/rts-sources: sources for the shared runtime"
    print ""
    print "The prefix controls where the built runtimes are installed."
    print "By default, the generated project files will install rts in:"
    print "  <gnat>/<target>/lib/gnat"
    print ""
    print "when generating the runtime source tree together with the BSPs, the"
    print "boards specified on command line must use the same toolchain"
    print "target."


class Main(object):
    def __init__(self):
        # global link, gccdir, gnatdir, verbose, create_common

        self.dest = "install"
        self.dest_bsps = None
        self.dest_prjs = None
        self.dest_srcs = None
        self.prefix = None
        self.gen_rts_srcs = True

        try:
            opts, args = getopt.getopt(
                sys.argv[1:], "hvl",
                ["help", "verbose", "bsps-only",
                 "output=", "output-bsps=", "output-prjs=", "output-srcs=",
                 "prefix=", "gcc-dir=", "gnat-dir=", "link"])
        except getopt.GetoptError, e:
            print "error: " + str(e)
            print ""
            usage()
            sys.exit(2)

        for opt, arg in opts:
            if opt in ("-v", "--verbose"):
                FilesHolder.verbose = True
            elif opt in ("-h", "--help"):
                usage()
                sys.exit()
            elif opt in ("-l", "--link"):
                FilesHolder.link = True
            elif opt == "--output":
                self.dest = arg
            elif opt == "--output-bsps":
                self.dest_bsps = arg
            elif opt == "--output-prjs":
                self.dest_prjs = arg
            elif opt == "--output-srcs":
                self.dest_srcs = arg
            elif opt == "--gcc-dir":
                FilesHolder.gccdir = arg
            elif opt == "--gnat-dir":
                FilesHolder.gnatdir = arg
            elif opt == "--prefix":
                self.prefix = arg
            elif opt == "--bsps-only":
                self.gen_rts_srcs = False
            else:
                print "unexpected switch: %s" % opt
                sys.exit(2)

        if len(args) < 1:
            print "error: missing configuration"
            print ""
            usage()
            sys.exit(2)

        self.boards = []

        for arg in args:
            board = build_configs(arg)
            self.boards.append(board)

        # figure out the target
        self.target = self.boards[0].target
        if self.target is None:
            self.target = "native"
        for board in self.boards:
            if board.target is None and self.target == "native":
                continue
            if board.target != self.target:
                self.target = None
        assert self.target is not None, \
            "cannot generate rts sources for mixed cross compilers"

        # make sure dest_bsps, dest_prjs and dest_srcs have values
        if self.dest_bsps is None:
            self.dest_bsps = os.path.join(self.dest, 'BSPs')
        if self.gen_rts_srcs:
            if self.dest_prjs is None:
                self.dest_prjs = os.path.join(self.dest, 'lib', 'gnat')
            if self.dest_srcs is None:
                self.dest_srcs = os.path.join(
                    self.dest, 'include', 'rts-sources')

    def install(self):
        self.dest = os.path.abspath(self.dest)
        if not os.path.exists(self.dest):
            os.makedirs(self.dest)

        # create the destination directory
        if not os.path.exists(self.dest_bsps):
            os.makedirs(self.dest_bsps)

        # Install the BSPs
        for board in self.boards:
            install = Installer(board)
            install.install(self.dest_bsps, self.prefix)

        # post-processing, install ada_object_path and ada_source_path to be
        # installed in all runtimes by gprinstall
        bsp_support = os.path.join(self.dest_bsps, 'support')
        if not os.path.exists(bsp_support):
            os.mkdir(bsp_support)
            with open(os.path.join(bsp_support, 'ada_source_path'), 'w') as fp:
                fp.write('gnat\ngnarl\n')
            with open(os.path.join(bsp_support, 'ada_object_path'), 'w') as fp:
                fp.write('adalib\n')

        if self.gen_rts_srcs:
            is_pikeos = 'pikeos' in self.target

            # determining what runtime sources we need:
            # - 'pikeos': all profiles, and a specific rts sources organisation
            # - 'ravenscar-full': all profiles support
            # - 'ravenscar-sfp': sfp + zfp profiles support
            # - 'zfp': just zfp support

            if is_pikeos:
                rts_profile = 'ravenscar-full'
            else:
                rts_profile = 'zfp'

                for board in self.boards:
                    if 'ravenscar-full' in board.system_ads:
                        # install everything
                        rts_profile = 'ravenscar-full'
                        break
                    else:
                        for rts in board.system_ads:
                            if 'ravenscar-' in rts:
                                rts_profile = 'ravenscar-sfp'

            # Compute rts sources subdirectories
            if not os.path.exists(self.dest_prjs):
                os.makedirs(self.dest_prjs)
            if not os.path.exists(self.dest_srcs):
                os.makedirs(self.dest_srcs)

            # Install the shared runtime sources
            SourceTree.dest_sources = self.dest_srcs
            SourceTree.dest_prjs = self.dest_prjs

            # create the rts sources object. This uses a slightly different set
            # on pikeos.
            rts_srcs = SourceTree(
                is_bb=not is_pikeos, profile=rts_profile,
                rts_sources=rts_sources, rts_scenarios=rts_scenarios)
            rts_srcs.install()


def main():
    m = Main()
    m.install()


if __name__ == '__main__':
    main()
