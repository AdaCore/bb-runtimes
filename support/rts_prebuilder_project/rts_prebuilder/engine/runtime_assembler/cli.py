#
# Copyright (C) 2025-2026, AdaCore
#

"""Common CLI functionality for runtime assembler."""

import argparse
import logging
from pathlib import Path

from rts_prebuilder.base_types import VALID_BASE_PROFILES_SET, VALID_PLATFORM_IDS_SET
from rts_prebuilder.end_user_data.gnat_gcc_dir_selector import (
    DEFAULT_GCC_DIR,
    DEFAULT_GNAT_DIR,
    set_gccdir,
    set_gnatdir,
)
from rts_prebuilder.end_user_data.logger import get_logger, set_log_level

from .assembler import RuntimeAssembler

log = get_logger(__name__)


# Import will be done lazily to avoid issues at import time


class RuntimeAssemblerCLI:
    """CLI class to encapsulate parser creation and execution."""

    @staticmethod
    def create_parser(
        prog: str | None = None, description: str | None = None
    ) -> argparse.ArgumentParser:
        """Create and configure the argument parser."""
        parser = argparse.ArgumentParser(prog=prog, description=description)

        parser.add_argument(
            "--output-dir",
            "--output",  # Legacy argname TODO:4 remove in future versions
            help=(
                "installation location. By default the runtime JSON descriptor is "
                "installed in <output>/lib/gnat while the sources are installed "
                "in <output>/include/rts-sources"
                "ignored if --output-sources is provided."
            ),
            type=Path,
        )

        parser.add_argument(
            "--output-sources-dir",
            "--output-sources",  # Legacy argname TODO:4 remove in future versions
            type=Path,
            help="Output directory for generated runtime sources",
        )

        parser.add_argument(
            "--output-descriptor-path",
            "--output-descriptor",  # Legacy argname TODO:4 remove in future versions
            type=Path,
            help="Output JSON descriptor file path (default: <output-dir>/lib/gnat/rts-sources.json)",
        )

        parser.add_argument(
            "--platform",
            "--source-profile",  # Legacy argname TODO:4 remove in future versions
            choices=VALID_PLATFORM_IDS_SET,
            default="bb",
            help="Platform specific source selections",
        )

        parser.add_argument(
            "--top-base-profile",
            "--rts-profile",  # Legacy argname TODO:4 remove in future versions
            choices=VALID_BASE_PROFILES_SET,
            required=True,
            help="Highest base profile for supported by the assembled runtime sources",
        )

        parser.add_argument(
            "--verbose",
            "-v",
            dest="verbose",
            action="store_true",
            help="Enable verbose output",
        )

        parser.add_argument(
            "--gcc-dir",
            type=Path,
            help="GCC sources directory (Must exist on disk), "
            f" will fallback to {DEFAULT_GCC_DIR} if not provided",
        )
        parser.add_argument(
            "--gnat-dir",
            type=Path,
            help=f"GNAT sources directory (Must exist on disk)"
            f" will fallback to {DEFAULT_GNAT_DIR} if not provided.",
        )

        parser.add_argument(
            "--link",
            "-l",
            action="store_true",
            help="Use symlinks when installing files instead of copies"
            " (Default: False).",
            default=False,
        )

        return parser

    @staticmethod
    def run_runtime_assembler(args: argparse.Namespace) -> None:
        """Run the RuntimeAssembler with the given arguments."""
        # Set user data from arguments
        if args.verbose:
            set_log_level(logging.DEBUG)
        else:
            set_log_level(logging.INFO)

        set_gccdir(args.gcc_dir)
        set_gnatdir(args.gnat_dir)

        # Convert platform to platform_srcs format
        platform = args.platform + "_srcs"

        assembler = RuntimeAssembler(
            platform=platform, top_base_profile=args.top_base_profile
        )

        # Set up output paths
        # And create directories if they don't exist

        if args.output_sources_dir:
            output_sources_dir = args.output_sources_dir.resolve()
        elif args.output_dir:
            output_sources_dir = args.output_dir / "include" / "rts-sources"
        else:
            output_sources_dir = Path.cwd() / "install" / "include" / "rts-sources"

        log.debug("output sources dir: %s", output_sources_dir)

        if args.output_descriptor_path:
            dest_json = args.output_descriptor_path.resolve()
        elif args.output_dir:
            dest_json = args.output_dir / "lib" / "gnat" / "rts-sources.json"
        else:
            dest_json = Path.cwd() / "install" / "lib" / "gnat" / "rts-sources.json"

        log.debug("dest_json path: %s", dest_json)

        output_sources_dir.mkdir(parents=True, exist_ok=True)
        dest_json.parent.mkdir(parents=True, exist_ok=True)

        # Install the runtime sources
        assembler.install_tree(
            dest_json=dest_json, output_sources_dir=output_sources_dir, link=args.link
        )

        log.info(
            "Successfully generated runtime sources in %s and JSON descriptor in %s",
            output_sources_dir,
            dest_json,
        )
