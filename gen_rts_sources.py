#! /usr/bin/env python3
#
# Copyright (C) 2016-2020, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

import argparse
import os

from support.files_holder import FilesHolder
from support.rts_sources import SourceTree
from support.rts_sources.sources import all_scenarios, sources


def main():
    # global link, gccdir, gnatdir, verbose, create_common

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-v', '--verbose', action="store_true",
        help="verbose output")
    parser.add_argument(
        '-l', '--link', action="store_true",
        help="use symlinks when installing files")
    parser.add_argument(
        '--gcc-dir', help='gcc sources dir')
    parser.add_argument(
        '--gnat-dir', help='gnat sources dir')
    parser.add_argument(
        '--output', help=(
            'installation location. By default the runtime descriptor is '
            'installed in <output>/lib/gnat while the sources are installed '
            'in <output>/include/rts-sources'))
    parser.add_argument(
        '--output-descriptor',
        help='installation location for the runtime sources descriptor')
    parser.add_argument(
        '--output-sources',
        help='installation location for the runtime sources tree')
    parser.add_argument(
        '--rts-profile', choices=['zfp', 'ravenscar-sfp', 'ravenscar-full'],
        required=True,  help='supported profiles')
    parser.add_argument(
        '--pikeos', action="store_true",
        help="use PikeOS sources")

    args = parser.parse_args()

    if args.verbose:
        FilesHolder.verbose = True
    if args.link:
        FilesHolder.link = True
    if args.gcc_dir is not None:
        FilesHolder.gccdir = os.path.abspath(args.gcc_dir)
    if args.gnat_dir is not None:
        FilesHolder.gnatdir = os.path.abspath(args.gnat_dir)

    if args.output is not None:
        dest = os.path.abspath(args.output)
    else:
        dest = os.path.abspath('install')

    if args.output_descriptor is not None:
        dest_json = os.path.abspath(args.output_descriptor)
    else:
        dest_json = os.path.join(dest, 'lib', 'gnat', 'rts-sources.json')

    if args.output_sources is not None:
        dest_srcs = os.path.abspath(args.output_sources)
    else:
        dest_srcs = os.path.join(dest, 'include', 'rts-sources')

    if not os.path.exists(os.path.dirname(dest_json)):
        os.makedirs(os.path.dirname(dest_json))
    if not os.path.exists(dest_srcs):
        os.makedirs(dest_srcs)

    # Install the shared runtime sources
    SourceTree.dest_sources = dest_srcs

    # create the rts sources object. This uses a slightly different set
    # on pikeos.
    rts_srcs = SourceTree(
        is_bb=not args.pikeos, profile=args.rts_profile,
        rts_sources=sources, rts_scenarios=all_scenarios)
    rts_srcs.install_tree(dest_json=dest_json, dest_sources=dest_srcs)


if __name__ == '__main__':
    main()
