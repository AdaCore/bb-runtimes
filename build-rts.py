#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from build_rts_support.files_holder import FilesHolder
from build_rts_support.rts_sources import SourceDirs

# PikeOS
from pikeos import ArmPikeOS, PpcPikeOS, X86PikeOS

# Cortex-M runtimes
from arm.cortexm import Stm32, Sam, SmartFusion2, LM3S

# Cortex-A/R runtimes
from arm.cortexar import TMS570, Rpi2, Zynq7000

# Aarch64
from aarch64 import Rpi3, AARCH64QEMU

# leon
from sparc import Leon2, Leon3, Leon4

# powerpc
from powerpc import MPC8641, MPC8349e, P2020, P5566, P5634

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
    elif target == 'rpi3':
        t = Rpi3()
    elif target == 'aarch64-qemu':
        t = AARCH64QEMU()
    elif target.startswith('sam'):
        t = Sam(target)
    elif target.startswith('smartfusion2'):
        t = SmartFusion2()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target == 'openmv2':
        t = Stm32(target)
    elif target == 'tms570':
        t = TMS570()
    elif target == 'lm3s':
        t = LM3S()
    elif target == 'leon2' or target == 'leon':
        t = Leon2()
    elif target == 'leon3':
        t = Leon3()
    elif target == 'leon4':
        t = Leon4()
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
    print " --bsps-only       generate only BSPs"
    print " --output=DIR      output directory"
    print " --prefix=DIR      where built rts will be installed"
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


def main():
    # global link, gccdir, gnatdir, verbose, create_common

    dest = "install"
    dest_bsps = None
    dest_prjs = None
    dest_srcs = None
    prefix = None
    gen_rts_srcs = True

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "hvl",
            ["help", "verbose", "bsps-only",
             "output=", "output-bsps=", "output-prjs=", "output-srcs=",
             "prefix=",
             "gcc-dir=", "gnat-dir=",
             "link"])
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
            dest = arg
        elif opt == "--output-bsps":
            dest_bsps = arg
        elif opt == "--output-prjs":
            dest_prjs = arg
        elif opt == "--output-srcs":
            dest_srcs = arg
        elif opt == "--gcc-dir":
            FilesHolder.gccdir = arg
        elif opt == "--gnat-dir":
            FilesHolder.gnatdir = arg
        elif opt == "--prefix":
            prefix = arg
        elif opt == "--bsps-only":
            gen_rts_srcs = False
        else:
            print "unexpected switch: %s" % opt
            sys.exit(2)

    if len(args) < 1:
        print "error: missing configuration"
        print ""
        usage()
        sys.exit(2)

    boards = []
    rts_profile = 'zfp'

    is_pikeos = 'pikeos' in args[0]

    for arg in args:
        board = build_configs(arg)
        boards.append(board)

    dest = os.path.abspath(dest)
    # default paths in case not specified from the command-line:
    if dest_bsps is None:
        dest_bsps = os.path.join(dest, 'BSPs')

    if not os.path.exists(dest):
        os.makedirs(dest)
    if not os.path.exists(dest_bsps):
        os.makedirs(dest_bsps)

    # Install the BSPs
    for board in boards:
        board.install(dest_bsps, prefix)

    # post-processing, install ada_object_path and ada_source_path to be
    # installed in all runtimes by gprinstall
    bsp_support = os.path.join(dest_bsps, 'support')
    if not os.path.exists(bsp_support):
        os.mkdir(bsp_support)
        with open(os.path.join(bsp_support, 'ada_source_path'), 'w') as fp:
            fp.write('gnat\ngnarl\n')
        with open(os.path.join(bsp_support, 'ada_object_path'), 'w') as fp:
            fp.write('adalib\n')

    if gen_rts_srcs:
        target = boards[0].target
        is_pikeos = target is not None and 'pikeos' in target

        # determining what runtime sources we need:
        # - 'pikeos': all profiles, and a specific rts sources organisation
        # - 'ravenscar-full': all profiles support
        # - 'ravenscar-sfp': sfp + zfp profiles support
        # - 'zfp': just zfp support

        if is_pikeos:
            rts_profile = 'pikeos'
        else:
            rts_profile = 'zfp'

        for board in boards:
            assert board.target == target, \
                "cannot generate rts sources for mixed targets: %s <> %s" % (
                    target, board.target)
            if not is_pikeos and board.full_system_ads is not None:
                rts_profile = 'ravenscar-full'
            elif rts_profile == 'zfp' and board.sfp_system_ads is not None:
                rts_profile = 'ravenscar-sfp'

        # Compute rts sources subdirectories

        if dest_prjs is None:
            dest_prjs = os.path.join(dest, 'lib', 'gnat')
        if dest_srcs is None:
            dest_srcs = os.path.join(dest, 'include', 'rts-sources')
        if not os.path.exists(dest_prjs):
            os.makedirs(dest_prjs)
        if not os.path.exists(dest_srcs):
            os.makedirs(dest_srcs)

        # Install the shared runtime sources
        SourceDirs.dest_sources = dest_srcs
        SourceDirs.dest_prjs = dest_prjs

        # create the rts sources object. This uses a slightly different set
        # on pikeos.
        rts_srcs = SourceDirs(is_bb=rts_profile != 'pikeos')

        # setup the rts profiles to be supported by the rts sources directory
        if rts_profile in ('pikeos', 'ravenscar-full'):
            rts_srcs.init_zfp()
            rts_srcs.init_sfp()
            rts_srcs.init_full()
        elif rts_profile == 'ravenscar-sfp':
            rts_srcs.init_zfp()
            rts_srcs.init_sfp()
        elif rts_profile == 'zfp':
            rts_srcs.init_zfp()
        rts_srcs.install()


if __name__ == '__main__':
    main()
