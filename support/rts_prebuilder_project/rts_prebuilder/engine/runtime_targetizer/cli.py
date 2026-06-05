#
# Copyright (C) 2025-2026, AdaCore
#

import argparse
import logging
from pathlib import Path
from typing import ClassVar, Tuple

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)
from rts_prebuilder.end_user_data.compiler_selector import (
    DEFAULT_COMPILER,
    Compiler,
    set_compiler,
)
from rts_prebuilder.end_user_data.logger import get_logger, set_log_level
from .cli_help_gen import list_targets_for_help

from ..common.infrastructure_interface import get_resolver
from .targetizer import RuntimeTargetizer

log = get_logger(__name__)


class RuntimeTargetizerCLI:
    """CLI class to encapsulate parser creation and execution.
    To use this class, register targets using the register_targets class method,
    then call the run class method to execute the CLI. It will auto adapt
    to the registered targets.
    """

    _cli_singleton: ClassVar["RuntimeTargetizerCLI | None"] = None
    """
    Singleton instance of the CLI.
    This allows for easy access to the CLI instance across the application.
    """

    _targets: dict[str, AbstractTarget] = {}
    """
    Dictionary of available targets, keyed by their AbstractTarget.cli_name attribute.
    """
    _generators: list[AbstractTargetGenerator] = []
    """List of registered target family generators (lazy, summarized in help)."""

    @classmethod
    def register_targets(
        cls,
        *targets: AbstractTarget | AbstractTargetGenerator,
    ) -> "RuntimeTargetizerCLI":
        """Register targets with the CLI singleton.
        If the singleton does not exist, it is created.
        Returns the CLI singleton instance for convenience.

        :param targets: The targets to register with the CLI.
        :return: The CLI singleton instance.
        """

        if not cls._cli_singleton:
            cls._cli_singleton = RuntimeTargetizerCLI()

        # Split concrete targets and generators
        concrete: dict[str, AbstractTarget] = {}
        for t in targets:
            if isinstance(t, AbstractTarget):
                concrete[t.cli_name] = t
            elif isinstance(t, AbstractTargetGenerator):
                cls._cli_singleton._generators.append(t)
            else:
                raise TypeError(
                    f"Unsupported target registration type: {type(t).__name__}"
                )

        cls._cli_singleton._targets.update(concrete)

        return cls._cli_singleton

    @classmethod
    def run(cls) -> None:
        """
        Convenience method to run the CLI quickly. Without any customization.
        """

        cli = cls._cli_singleton

        if cli is None:
            raise RuntimeError("No targets registered. Use register_targets first.")

        parser = cli.create_parser(
            prog="targetize_rts_sources",
            description="Targetize RTS (Runtime System) sources for a specific target",
        )

        args = parser.parse_args()

        cli.run_targetizer(args)

    def create_parser(self, prog: str, description: str) -> argparse.ArgumentParser:
        """Create and configure the argument parser.

        :param prog: The name of the program (for help messages).
        :param description: A brief description of the program.
        :return: Configured ArgumentParser instance.
        """
        parser = argparse.ArgumentParser(
            prog=prog,
            description=description,
            epilog=(
                "Available targets (short):\n"
                + list_targets_for_help(
                    self._targets, self._generators, summary_only=True
                )
            ),
            formatter_class=argparse.RawDescriptionHelpFormatter,
        )

        parser.add_argument(
            "target",
            nargs="*",
            help=(
                "The targets names (as in AbstractTarget.cli_name) to use for targetization,"
                " amongst the available ones. See below for summarized families."
            ),
            type=str,
        )
        parser.add_argument(
            "--list-targets",
            action="store_true",
            help=(
                "Print all available targets (including generated variants) and exit."
            ),
        )

        parser.add_argument(
            "--output-dir",
            "--output",  # Legacy argname TODO:4 remove in future versions
            help=("The installation directory for the target specific runtime."),
            type=Path,
        )

        parser.add_argument(
            "--rts-src-descriptor",
            help="The path to the runtime source descriptor file (rts-sources.json)"
            "If not given, auto-search using locate_json_descriptor will be attempted.",
            type=Path,
        )

        parser.add_argument(
            "--compiler",
            help="The compiler to generate flags for"
            f" (default: {DEFAULT_COMPILER.name})",
            dest="compiler",
            choices=[compiler.name for compiler in Compiler],
            type=str,
            default=DEFAULT_COMPILER.name,
        )

        parser.add_argument(
            "--overwrite",
            "-o",
            "--force",  # Legacy argname TODO:4 remove in future versions
            "-f",
            action="store_true",
            help=(
                "Forces the installation by overwriting any pre-existing install."
                " (default: False)."
            ),
        )

        parser.add_argument(
            "--verbose",
            "-v",
            dest="verbose",
            action="store_true",
            help="Enable verbose output",
        )

        parser.add_argument(
            "-l",
            "--link",
            action="store_true",
            help="Use symlinks instead of copies when installing (default: False).",
        )

        parser.add_argument(
            "--profiles",
            type=lambda s: [item.strip() for item in s.split(",")],
            help="Optional filter to only include runtimes matching the given profiles "
            "(comma-separated list, e.g., 'light' or 'light,light-tasking').",
        )

        parser.add_argument(
            "--source-search-path",
            action="append",
            default=[],
            type=Path,
            help="Extra directory to search for source files (repeatable). ",
        )

        return parser

    def run_targetizer(
        self, args: argparse.Namespace
    ) -> list[Tuple[AbstractTarget, Path]]:
        """Execute the targetization process based on parsed arguments.

        :param args: Parsed command-line arguments.
        :return: A list of tuples containing the target and the path to each installed runtime.
        """
        # Set user data from arguments
        if args.verbose:
            set_log_level(logging.DEBUG)
        else:
            set_log_level(logging.INFO)

        set_compiler(Compiler[args.compiler])
        log.info("Selected compiler: %s", args.compiler)

        # Make caller-provided source roots (e.g. the bb-runtimes repo 'src'
        # dir) visible to the resolver so board files and datafiles templates
        # outside the installed package can be found.
        if getattr(args, "source_search_path", None):
            get_resolver().add_search_paths(*args.source_search_path)

        # Optional listing mode
        if getattr(args, "list_targets", False):
            print(
                list_targets_for_help(
                    self._targets, self._generators, summary_only=False
                )
            )
            return []

        # Require at least one target when not listing
        if not getattr(args, "target", []):
            raise SystemExit(
                "No target specified. Provide at least one target or use --list-targets to see options."
            )

        # Resolve targets: concrete first, then generator-backed
        targets: list[AbstractTarget] = []
        for name in args.target:
            if name in self._targets:
                targets.append(self._targets[name])
                continue

            resolved = None
            for gen in self._generators:
                if gen.match_variant(name):
                    resolved = gen.instantiate(name)
                    break

            if resolved is None:
                listing = list_targets_for_help(
                    self._targets, self._generators, summary_only=False
                )
                raise SystemExit(f"Unknown target: {name}\n\n{listing}")
            targets.append(resolved)

        installed_targets_and_runtimes = []

        for target in targets:
            log.info(
                "Targetizing runtime for target: %s (Class: %s / name: %s)",
                target.cli_name,
                target.__class__.__name__,
                target.name,
            )

            # Handle output directory arg
            if not args.output_dir:
                output_dir = Path("install")
            else:
                output_dir = args.output_dir

            log.info("Output directory: %s", output_dir)

            runtime_targetizer = RuntimeTargetizer(
                target=target,
                json_descriptor=args.rts_src_descriptor,
            )

            # Step 1: Create the targetized runtimes
            created_runtimes = runtime_targetizer.create_targetized_runtimes(
                filter_base_profile_names=args.profiles,
            )
            if len(created_runtimes) == 0:
                log.warning("No runtimes created for target %s", target.name)
                continue

            # Step 2: Install the created runtimes
            runtimes = runtime_targetizer.install_targetized_runtimes(
                destination=output_dir,
                overwrite=args.overwrite,
                link=args.link,
            )

            installed_targets_and_runtimes.extend([(target, rt) for rt in runtimes])

        return installed_targets_and_runtimes
