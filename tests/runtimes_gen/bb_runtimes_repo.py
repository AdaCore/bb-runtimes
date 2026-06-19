#
# Copyright (C) 2025-2026, AdaCore
#

"""bb_runtimes repository implementation."""

from __future__ import annotations

import logging
import sys
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTargetGenerator,
    AbstractTarget,
)
from bb_runtimes_targets_gen.targets import TARGETS as ALL_TARGETS

from .tested_repository import AbstractTestedRepository, TargetInfo


log = logging.getLogger(__name__)


class BbRuntimesRepository(AbstractTestedRepository):
    """bb_runtimes repository."""

    def __init__(self, root_path: Path):
        super().__init__(root_path)

    @override
    def get_targets_list(self) -> list[TargetInfo]:
        """Get all targets from ALL_TARGETS."""

        def create_target_info(
            target_instance: AbstractTarget, cli_name: str
        ) -> TargetInfo:
            """Helper to create TargetInfo from an AbstractTarget instance."""
            base_profile = self.deduce_top_base_profile(
                target_instance.system_ads.keys()
            )  # Deduce top base profile
            return TargetInfo(
                cli_name=cli_name,
                platform=target_instance.platform,
                top_base_profile=base_profile,
            )

        all_targets = ALL_TARGETS
        targets = []
        for t in all_targets:
            if isinstance(t, AbstractTargetGenerator):
                # Handle target generators - instantiate variants
                for i, variant_name in enumerate(t.generate_variants()):
                    # Limit to first 5 variants of the same generator for brevity
                    if i >= 5:
                        break
                    # Instantiate the variant to get an AbstractTarget instance
                    target_instance = t.instantiate(variant_name)
                    targets.append(create_target_info(target_instance, variant_name))
            else:
                # Already an AbstractTarget instance
                targets.append(create_target_info(t, t.cli_name))

        return targets

    @override
    def get_targets_list_subset(self) -> list[TargetInfo]:
        """Get vx7r2cert targets subset."""
        all_targets = self.get_targets_list()
        subset = [t for t in all_targets if t.platform == "vx7r2cert"]
        if not subset:
            subset = all_targets[:3]
        return subset

    @override
    def run_assembly(
        self,
        platform: str,
        base_profile: str,
        output_dir: Path,
        gcc_path: Path,
        gnat_path: Path,
        link_mode: bool = False,
        verbose: bool = False,
    ) -> None:
        """Run gen_rts_sources.py."""
        gen_script = self.root_path / "gen_rts_sources.py"
        if not gen_script.exists():
            raise FileNotFoundError(f"Generator script not found: {gen_script}")

        cmd = [
            sys.executable,
            str(gen_script),
            "--top-base-profile",
            base_profile,
            "--source-profile",
            platform,
            "--output",
            str(output_dir),
        ]

        # Only add --gnat and --gcc if paths are provided
        if gnat_path:
            cmd.extend(["--gnat", str(gnat_path)])
        if gcc_path:
            cmd.extend(["--gcc", str(gcc_path)])

        if link_mode:
            cmd.append("--link")

        output_dir.mkdir(parents=True, exist_ok=True)

        self._run_command(
            cmd,
            self.root_path,
            f"Assembly for {base_profile}/{platform}",
            verbose,
        )

    @override
    def run_targetizer(
        self,
        descriptor_file: Path,
        target_cli_name: str,
        output_dir: Path,
        base_profile: str,
        verbose: bool = False,
    ) -> None:
        """Run the per-arch targetizer entry point for this target."""
        arch_module = self._arch_module_for(target_cli_name)

        lib_gnat_dir = output_dir / "lib" / "gnat"
        lib_gnat_dir.mkdir(parents=True, exist_ok=True)

        cmd = [
            sys.executable,
            "-m",
            arch_module,
            target_cli_name,
            "--rts-src-descriptor",
            str(descriptor_file),
            "--output",
            str(lib_gnat_dir),
            # Board files and datafiles templates live in the repo 'src' dir,
            # outside the installed package.
            "--source-search-path",
            str(self.root_path / "src"),
            "--overwrite",
        ]

        self._run_command(
            cmd, self.root_path, f"Targetizer for {target_cli_name}", verbose
        )

    @staticmethod
    def _arch_module_for(target_cli_name: str) -> str:
        """Return the ``python -m`` arch entry point that owns target_cli_name.

        ALL_TARGETS mixes plain targets with generators (which expand to several
        variants), so match the same way get_targets_list() enumerates them.
        """
        for t in ALL_TARGETS:
            if isinstance(t, AbstractTargetGenerator):
                names = list(t.generate_variants())
            else:
                names = [t.cli_name]
            if target_cli_name in names:
                parts = t.__class__.__module__.split(".")
                arch = parts[parts.index("targets") + 1]
                return f"bb_runtimes_targets_gen.targets.{arch}"
        raise ValueError(f"No targets entry point for cli_name {target_cli_name!r}")
