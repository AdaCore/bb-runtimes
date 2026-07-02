#
# Copyright (C) 2025-2026, AdaCore
#

"""
Abstract base class for tested repositories with common interfaces.
"""

import logging
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
from collections.abc import Iterable

log = logging.getLogger("runtimes_gen_test")


@dataclass
class TargetInfo:
    """Target test information."""

    cli_name: str
    platform: str
    top_base_profile: str


class AbstractTestedRepository(ABC):
    """Base class for testable repositories."""

    def __init__(self, root_path: Path):
        self._root_path = root_path
        self._baseline: "AbstractTestedRepository | None" = None
        self._associated_bb_runtimes: Path | None = None

    @property
    def root_path(self) -> Path:
        return self._root_path

    @property
    def associated_bb_runtimes(self) -> Path | None:
        return self._associated_bb_runtimes

    @property
    def baseline(self) -> "AbstractTestedRepository | None":
        return self._baseline

    @baseline.setter
    def baseline(self, value: "AbstractTestedRepository | None") -> None:
        self._baseline = value

    def deduce_top_base_profile(self, available_base_profiles: Iterable[str]) -> str:
        """Return top base profile from available profiles.

        Priority: light < cert ~= light-tasking < embedded.
        """
        profile_priority = {"light": 1, "cert": 2, "light-tasking": 2, "embedded": 3}
        max_prio = 0
        top_profile = None
        for p in available_base_profiles:
            try:
                prio = profile_priority[p]
            except KeyError as e:
                raise KeyError(f"unknown profile encountered: {e}")
            if prio > max_prio:
                max_prio = prio
                top_profile = p

        if top_profile is None:
            raise ValueError(
                "Could not deduce top base profile from the target's available profiles"
            )

        return top_profile

    def log_test_start(
        self,
        target: TargetInfo,
        platform: str,
        base_profile: str,
        link_mode: bool,
        verbose: bool,
        logger: "logging.Logger",
    ) -> None:
        """Log test combination start."""
        target_info = f"{target.cli_name} (platform: {target.platform}, top base profile: {base_profile})"

        logger.info(
            "Testing %s: Generating runtime for target %s",
            self.root_path.name,
            target_info,
        )

    @abstractmethod
    def get_targets_list(self) -> list[TargetInfo]:
        """Get full targets list."""
        pass

    @abstractmethod
    def get_targets_list_subset(self) -> list[TargetInfo]:
        """Get subset for quick testing."""
        pass

    @abstractmethod
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
        """Run assembly step."""
        pass

    @abstractmethod
    def run_targetizer(
        self,
        descriptor_file: Path,
        target_cli_name: str,
        output_dir: Path,
        base_profile: str,
        verbose: bool = False,
    ) -> None:
        """Run targetizer step."""
        pass

    def _run_command(
        self, cmd: list[str], cwd: Path, description: str, verbose: bool = False
    ) -> subprocess.CompletedProcess[str]:
        """Run command and handle errors."""
        if verbose:
            log.info("Running command: %s", " ".join(cmd))
            log.info("Working directory: %s", cwd)

        completed = subprocess.run(
            cmd,
            cwd=str(cwd),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )

        if completed.returncode != 0:
            raise RuntimeError(
                f"{description} failed (exit code {completed.returncode})\n"
                f"Command: {' '.join(cmd)}\n"
                f"CWD: {cwd}\n"
                f"STDOUT: {completed.stdout}\n"
                f"STDERR: {completed.stderr}"
            )

        return completed
