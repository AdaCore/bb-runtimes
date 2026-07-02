#
# Copyright (C) 2025-2026, AdaCore
#

"""Certified-rts repository implementation."""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any, override

from .configs.certified_rts_targets import CERTIFIED_RTS_TARGETS
from .tested_repository import AbstractTestedRepository, TargetInfo


class CertifiedRtsRepository(AbstractTestedRepository):
    """
    Implementation for certified-rts repository.

    Uses gen-rts-sources.py (current version without --bb-dir).
    Handles all targets within the certified-rts repository.
    """

    def __init__(
        self,
        root_path: Path,
        bb_runtimes_root: Path,
    ):
        super().__init__(root_path)
        # The bb-runtimes board sources are passed to the targetizer explicitly
        # via --source-search-path; the cert engine's own fixed relative guess at
        # the bb-runtimes location does not hold in every checkout layout.
        self._bb_runtimes_root = bb_runtimes_root
        # Internal mappings: (cli_name, platform, base_profile) -> cert_subdir
        self._target_to_info: dict[tuple[str, str, str], str] = {}

    def _load_config(self) -> Any:
        """Load certified-rts configuration."""
        return CERTIFIED_RTS_TARGETS

    @override
    def get_targets_list(self) -> list[TargetInfo]:
        """
        Return all targets from all certified-rts configurations.

        For certified-rts, targets are defined in configuration dictionaries
        that specify cert_subdir and other metadata.
        """
        config = self._load_config()
        targets = []
        # Clear and rebuild the mapping
        self._target_to_info.clear()

        for target_config in config:
            target_name: str = target_config["name"]
            base_profile: str = target_config["top_base_profile"]
            platforms: list[str] = target_config["platforms"]
            cert_subdir: str = target_config.get("cert_subdir", target_name)

            for platform in platforms:
                cli_name = target_name

                # Store the mapping for later use in run_targetizer
                # Maps (cli_name, platform, base_profile) -> cert_subdir
                self._target_to_info[(cli_name, platform, base_profile)] = cert_subdir

                targets.append(
                    TargetInfo(
                        cli_name=cli_name,
                        platform=platform,
                        top_base_profile=base_profile,
                    )
                )

        return targets

    @override
    def get_targets_list_subset(self) -> list[TargetInfo]:
        """Return first target for quick testing."""
        all_targets = self.get_targets_list()
        return all_targets[:1] if all_targets else []

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
        """Run gen-rts-sources.py."""
        # The generator is at certified-rts root
        gen_script = self.root_path / "gen-rts-sources.py"
        if not gen_script.exists():
            raise FileNotFoundError(f"Generator script not found: {gen_script}")

        # Default to certified-rts/../../toolchain/gcc and certified-rts/../../toolchain/gnat if not provided
        if gnat_path is None:
            gnat_path = self.root_path.parent.parent / "toolchain" / "gnat"
        if gcc_path is None:
            gcc_path = self.root_path.parent.parent / "toolchain" / "gcc"

        cmd = [
            sys.executable,
            str(gen_script),
            "--rts-profile",
            base_profile,
            "--source-profile",
            platform,
            "--output",
            str(output_dir),
        ]

        cmd.extend(["--gnat", str(gnat_path)])
        cmd.extend(["--gcc", str(gcc_path)])

        if link_mode:
            cmd.append("--link")

        output_dir.mkdir(parents=True, exist_ok=True)

        self._run_command(
            cmd,
            self.root_path,
            f"Certified-rts assembly for {base_profile}/{platform}",
            verbose,
        )

    def _get_target_info(
        self, target_cli_name: str, platform: str, base_profile: str
    ) -> str:
        """Get the cert_subdir for a given target combination."""
        key = (target_cli_name, platform, base_profile)
        target_info = self._target_to_info.get(key)
        if target_info is None:
            raise ValueError(
                f"No target info mapping found for target: {target_cli_name}, "
                f"platform: {platform}, base_profile: {base_profile}"
            )
        return target_info

    @override
    def run_targetizer(
        self,
        descriptor_file: Path,
        target_cli_name: str,
        output_dir: Path,
        base_profile: str,
        verbose: bool = False,
    ) -> None:
        """Run the certified-rts targetizer entry point (python -m <cert>.rts)."""
        # Look up the platform from the descriptor file path
        # The descriptor is at: assembly/{platform}/{base_profile}/lib/gnat/rts-sources.json
        # descriptor_file.parent = gnat, .parent = lib, .parent = base_profile, .parent = platform
        platform = descriptor_file.parent.parent.parent.parent.name

        # Get the cert_subdir for this target combination
        cert_subdir = self._get_target_info(target_cli_name, platform, base_profile)

        target_dir = self.root_path / cert_subdir
        rts_dir = target_dir / "rts"
        if not (rts_dir / "__main__.py").exists():
            raise FileNotFoundError(f"RTS __main__.py not found: {rts_dir}")

        lib_gnat_dir = output_dir / "lib" / "gnat"
        lib_gnat_dir.mkdir(parents=True, exist_ok=True)

        cmd = [
            sys.executable,
            "-m",
            f"{cert_subdir}.rts",
            "--rts-src-descriptor",
            str(descriptor_file),
            "--output",
            str(lib_gnat_dir),
            # bb-runtimes board sources, passed explicitly (the cert engine's
            # fixed relative guess at the bb-runtimes location is unreliable).
            "--source-search-path",
            str(self._bb_runtimes_root / "src"),
            target_cli_name,
            "--force",
        ]

        # Run from certified-rts root
        cwd = self.root_path

        self._run_command(
            cmd,
            cwd,
            f"Certified-rts targetizer for {target_cli_name}",
            verbose,
        )
