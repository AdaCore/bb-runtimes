#
# Copyright (C) 2025-2026, AdaCore
#

"""
Legacy repository implementations (to be removed after migration).

These implementations support testing against older versions of the repositories.
"""

from __future__ import annotations

import logging
import sys
from pathlib import Path
from typing import override

from .tested_repository import AbstractTestedRepository, TargetInfo
from .certified_rts_repo import CertifiedRtsRepository


class BbRuntimesLegacyRepository(AbstractTestedRepository):
    """
    Implementation for legacy bb_runtimes (no Python package).

    This reads targets from a predefined configuration file instead of
    discovering them dynamically.
    """

    def __init__(self, root_path: Path, config_file: Path | None = None):
        super().__init__(root_path)
        if config_file is None:
            # Use default config file
            config_file = (
                Path(__file__).parent / "configs" / "bb_runtimes_legacy_targets.csv"
            )
        self._config_file = config_file
        self._targets: list[TargetInfo] | None = None

    def _load_targets(self) -> list[TargetInfo]:
        """Load targets from CSV configuration file."""
        if self._targets is not None:
            return self._targets

        targets = []
        with open(self._config_file, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                # Skip comments and empty lines
                if not line or line.startswith("#"):
                    continue

                parts = line.split(",")
                if len(parts) != 3:
                    continue

                cli_name, platform, base_profile = [p.strip() for p in parts]
                targets.append(
                    TargetInfo(
                        cli_name=cli_name,
                        platform=platform,
                        top_base_profile=base_profile,
                    )
                )

        self._targets = targets
        return targets

    @override
    def get_targets_list(self) -> list[TargetInfo]:
        """Load targets from configuration file."""
        return self._load_targets()

    @override
    def get_targets_list_subset(self) -> list[TargetInfo]:
        """Return vx7r2cert targets or first target from full list."""
        all_targets = self.get_targets_list()
        # Filter for vx7r2cert platform
        subset = [t for t in all_targets if "vx7r2cert" in t.platform.lower()]
        # If no vx7r2cert targets, return first target
        if not subset and all_targets:
            subset = [all_targets[0]]
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
        """Run legacy gen_rts_sources.py."""
        gen_script = self.root_path / "gen_rts_sources.py"
        if not gen_script.exists():
            raise FileNotFoundError(f"Generator script not found: {gen_script}")

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

        if gnat_path is not None:
            cmd.extend(["--gnat", str(gnat_path)])

        if gcc_path is not None:
            cmd.extend(["--gcc", str(gcc_path)])

        if link_mode:
            cmd.append("--link")

        output_dir.mkdir(parents=True, exist_ok=True)

        self._run_command(
            cmd,
            self.root_path,
            f"Legacy assembly for {base_profile}/{platform}",
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
        """Run legacy build_rts.py."""
        build_script = self.root_path / "build_rts.py"
        if not build_script.exists():
            raise FileNotFoundError(f"Build script not found: {build_script}")

        # Determine profiles to build
        if base_profile == "embedded":
            profiles = "light,light-tasking,embedded"
        elif base_profile == "light-tasking":
            profiles = "light,light-tasking"
        elif base_profile == "cert":
            profiles = "cert"
        else:
            profiles = "light"

        lib_gnat_dir = output_dir / "lib" / "gnat"
        lib_gnat_dir.mkdir(parents=True, exist_ok=True)

        cmd = [
            sys.executable,
            str(build_script),
            "--rts-src-descriptor",
            str(descriptor_file),
            "--profiles",
            profiles,
            "--output",
            str(lib_gnat_dir),
            target_cli_name,
            "--force",
        ]

        self._run_command(
            cmd,
            self.root_path,
            f"Legacy targetizer for {target_cli_name}",
            verbose,
        )


class BbRuntimesLegacyBaselineRepository(BbRuntimesLegacyRepository):
    """
    Implementation for bb-master worktree (bb-runtimes at master branch).

    This repository represents the legacy version of bb-runtimes that serves
    as a baseline for comparison. It is expected to be a worktree checkout of
    bb-runtimes at the master branch, located at ../bb-master relative to the
    main repository.

    This implementation extends BbRuntimesLegacyRepository and may require
    additional legacy adjustments over time.
    """

    @override
    def __str__(self) -> str:
        """Return a friendly name for this repository."""
        return "bb-runtimes legacy"


class CertifiedRtsLegacyRepository(CertifiedRtsRepository):
    """Legacy certified-rts implementation using build-rts.py with --bb-dir."""

    def __init__(
        self,
        root_path: Path,
        associated_bb_runtimes: Path,
    ):
        # Initialize parent
        super().__init__(root_path, associated_bb_runtimes)
        # Store bb_runtimes path for legacy use
        self._associated_bb_runtimes = associated_bb_runtimes

    @property
    @override
    def associated_bb_runtimes(self) -> Path:
        """Return the associated bb_runtimes path for legacy --bb-dir."""
        if self._associated_bb_runtimes is None:
            raise ValueError("associated_bb_runtimes not set")
        return self._associated_bb_runtimes

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
        """Run legacy gen-rts-sources.py with --bb-dir argument."""
        # The generator is at certified-master root
        gen_script = self.root_path / "gen-rts-sources.py"
        if not gen_script.exists():
            raise FileNotFoundError(f"Generator script not found: {gen_script}")

        # Default to certified-master/../../toolchain/gcc and
        # certified-master/../../toolchain/gnat if not provided
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
            "--bb-dir",
            str(self.associated_bb_runtimes),
        ]

        cmd.extend(["--gnat", str(gnat_path)])
        cmd.extend(["--gcc", str(gcc_path)])

        if link_mode:
            cmd.append("--link")

        output_dir.mkdir(parents=True, exist_ok=True)

        self._run_command(
            cmd,
            self.root_path,
            f"Legacy certified-rts assembly for {base_profile}/{platform}",
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
        """Run legacy build-rts.py with --bb-dir argument."""
        # Look up the platform from the descriptor file path
        # The descriptor is at: assembly/{platform}/{base_profile}/lib/gnat/rts-sources.json
        platform = descriptor_file.parent.parent.parent.parent.name

        # Get the cert_subdir for this target combination
        cert_subdir = self._get_target_info(target_cli_name, platform, base_profile)

        # Legacy certified-rts uses build-rts.py script in the target subdirectory
        target_dir = self.root_path / cert_subdir
        build_script = target_dir / "build-rts.py"
        if not build_script.exists():
            raise FileNotFoundError(f"Build script not found: {build_script}")

        lib_gnat_dir = output_dir / "lib" / "gnat"
        lib_gnat_dir.mkdir(parents=True, exist_ok=True)

        cmd = [
            sys.executable,
            str(build_script),
            "--rts-src-descriptor",
            str(descriptor_file),
            "--output",
            str(lib_gnat_dir),
            "--bb-dir",
            str(self.associated_bb_runtimes),
            "--force",
            target_cli_name,
        ]

        self._run_command(
            cmd,
            target_dir,
            f"Legacy certified-rts targetizer for {target_cli_name}",
            verbose,
        )


def setup_legacy_baseline(
    repo: AbstractTestedRepository, logger: logging.Logger, bb_runtimes_path: Path
) -> AbstractTestedRepository | None:
    """
    Check for legacy baselines and configure them if they exist.

    For bb-runtimes: checks for bb-master worktree
    For certified-rts: checks for ../../Cert/certified-master

    This function encapsulates all legacy baseline detection logic.
    When legacy repos are removed, this entire function can be deleted.

    Args:
        repo: The repository to configure with a baseline
        logger: Logger for status messages
        bb_runtimes_path: Path to bb-runtimes (used to find certified-master)

    Returns:
        The baseline repository if configured, None otherwise
    """
    # Check if this is a CertifiedRtsRepository
    if isinstance(repo, CertifiedRtsRepository):
        # Fixed path: ../../Cert/certified-master relative to bb-runtimes
        cert_master_path = bb_runtimes_path.parent.parent / "Cert" / "certified-master"

        if cert_master_path.exists() and cert_master_path.is_dir():
            # Legacy certified-rts needs legacy bb-runtimes (bb-master) for --bb-dir
            bb_master_path = bb_runtimes_path.parent / "bb-master"
            if not bb_master_path.exists():
                logger.warning(
                    "certified-master found but bb-master not found at %s - "
                    "certified-master needs bb-master for baseline generation",
                    bb_master_path,
                )
                return None

            logger.info("Found certified-master at %s", cert_master_path)
            logger.info(
                "Using bb-master at %s for certified-master baseline", bb_master_path
            )
            logger.info("Setting certified-master as baseline for comparison")

            # Create a legacy baseline repository for the entire certified-master
            # Use bb-master (not current bb-runtimes) for --bb-dir
            baseline_repo: AbstractTestedRepository = CertifiedRtsLegacyRepository(
                root_path=cert_master_path,
                associated_bb_runtimes=bb_master_path,
            )
            repo.baseline = baseline_repo
            return baseline_repo
        else:
            logger.info(
                "No certified-master found at %s (baseline comparison disabled)",
                cert_master_path,
            )
            return None
    else:
        # For bb-runtimes, check for bb-master worktree
        bb_master_path = repo.root_path.parent / "bb-master"

        if bb_master_path.exists() and bb_master_path.is_dir():
            logger.info("Found bb-master worktree at %s", bb_master_path)
            logger.info("Setting bb-master as baseline for comparison")
            baseline_repo = BbRuntimesLegacyBaselineRepository(bb_master_path)
            repo.baseline = baseline_repo
            return baseline_repo
        else:
            logger.info(
                "No bb-master worktree found at %s (baseline comparison disabled)",
                bb_master_path,
            )
            return None
