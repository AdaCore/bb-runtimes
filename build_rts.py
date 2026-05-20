#!/usr/bin/env python3
#
# Copyright (C) 2025-2026, AdaCore
#

"""Targetize RTS sources for one or more targets and optionally build them."""

import argparse
import importlib
import shutil
import sys
from pathlib import Path
from typing import Any

from rts_prebuilder.end_user_data.logger import get_logger
from rts_prebuilder.engine import RuntimeTargetizerCLI

from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)
from bb_runtimes_targets_gen.targets.all_targets import ALL_TARGETS

_repo_root = Path(__file__).resolve().parent
engine_interface.common.path_resolver_instance.add_search_paths(
    _repo_root / "src",
)

engine_interface.self_register()

log = get_logger(__name__)


def apply_target_specific_tweaks(args: argparse.Namespace) -> None:
    """
    Apply target specific tweaks to the arguments before running the targetizer.
    We only accept to do this because this a backward compatibility only script.
    """
    # Hack for vx7r2cert target to also build the -rtp variant
    # Users shall manually add the -rtp target to the command line in the future
    # TODO If this is necessary, we can consider allowing multiple targets to
    # share same cli_name, and all be built when that cli_name is requested.
    if args.target and len(args.target) == 1 and args.target[0].endswith("vx7r2cert"):
        log.warning(
            "Applying vx7r2cert target hack: Got %s target, adding also %s",
            args.target[0],
            args.target[0] + "-rtp",
        )
        args.target.append(args.target[0] + "-rtp")


def main() -> None:
    cli = RuntimeTargetizerCLI.register_targets(*ALL_TARGETS)

    # We use the lower level API of the CLI to add more arguments
    # for the build step

    parser = cli.create_parser(
        prog="build_rts.py",
        description="Targetize RTS (Runtime System) sources for a specific target, and build them",
    )

    # Parse actual build flags (the parser above is just the prebuilder flags)
    parser.add_argument("-b", "--build", action="store_true", help="Build the runtimes")
    parser.add_argument("--build-flags", help="Flags passed to gprbuild")
    parser.add_argument(
        "--shared",
        action="store_true",
        help="Additionally build shared runtime "
        "(only available on platforms that support shared libraries)",
    )

    args = parser.parse_args()

    apply_target_specific_tweaks(args)

    log.info("Starting rts_prebuilder targetization for targets: %s", args.target)

    # Call the CLI runner of the prebuilder's targetizer
    installed_targetized_runtimes_tuples = cli.run_targetizer(args)

    # Now call the build if requested
    log.info("Starting build step")

    if not args.build:
        log.info("Skipping actual build as --build was not passed")
        sys.exit(0)

    if len(installed_targetized_runtimes_tuples) == 0:
        log.info("No target runtimes were installed, nothing to build")
        sys.exit(0)

    for target_obj, runtime_path in installed_targetized_runtimes_tuples:
        # Objects needed before building the runtime
        obj_dir = runtime_path / "obj"
        obj_dir.mkdir(parents=True, exist_ok=True)

        target_obj.pre_build_step(obj_dir)

        # Import and call runtime-specific build script
        # TODO This import is dirty, we're just keeping for backward compatibility
        # until we can remove this script.

        # Add the runtime path to the Python path. To ensure the correct module is
        # loaded the runtime location is inserted as the first element of the path.
        sys.path.insert(0, str(runtime_path))

        # `build` module is the per-runtime build.py deployed from
        # src/datafiles/build.py.in; static type unknown, so use Any.
        rts_build: Any = importlib.import_module("build")

        log.info("Running %s", rts_build.__file__)

        # Reload the imported module. This is important as by default Python caches modules
        # by name. If the module is not reloaded explicitly Python will reuse the same module
        # that has been imported first over and over again, even if it has been removed with
        # del.
        importlib.reload(rts_build)

        # Call the build script

        rts_build.main(args.shared, args.build_flags)

        # Delete the module
        del rts_build

        # Remove the runtime from the Python path
        sys.path.remove(str(runtime_path))

        # Remove python artifacts from runtime directory
        shutil.rmtree(runtime_path / "__pycache__", ignore_errors=True)


if __name__ == "__main__":
    main()
