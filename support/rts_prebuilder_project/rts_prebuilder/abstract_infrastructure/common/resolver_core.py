#
# Copyright (C) 2025-2026, AdaCore
#

import csv
import subprocess
from dataclasses import dataclass
from pathlib import Path
from abc import ABC, abstractmethod

from rts_prebuilder.base_types import UnresolvedPath
from rts_prebuilder.end_user_data.logger import get_logger
from rts_prebuilder.end_user_data.metadata import get_metadata_dir

from .resolver_steps import AbstractResolutionStep, OutcomeDescription

log = get_logger(__name__)

DEFAULT_RECORD_GIT_METADATA = False
"""
You may switch this to False for performance-sensitive testing.
But it must never be committed as default.
"""


@dataclass(frozen=True)
class GitMetadata:
    """Git metadata about a resolved path."""

    git_revision: str | None
    """Revision of the dir at the time of path resolution."""
    git_dirty: bool | None
    """Whether the directory had uncommitted changes at the time of path resolution."""


@dataclass(frozen=True)
class ResolutionRecord:
    """Record of a single path resolution event."""

    unresolved_src_path: UnresolvedPath
    resolved_path: Path
    git_metatdata: GitMetadata
    outcome: OutcomeDescription


class SourcePathResolver(ABC):
    """
    Resolver engine for resolving source paths and recording resolution metadata.

    Must be provided with resolver steps to use.
    """

    _resolvers_steps: list[AbstractResolutionStep]
    """
    List of resolver steps to try in order.
    Each function takes an unresolved source path and returns a resolved Path or None if it cannot resolve.
    """

    _resolution_records: list[ResolutionRecord]
    """
    List of resolution records for tracking resolved paths.
    Each record contains the unresolved source path, the resolved path, and the resolution outcome used.
    And git_metadata if enabled.
    """

    _record_git_metadata: bool
    """
    Whether to record git metadata about resolved paths.
    Warning: Incurs overhead due to git commands.
    """

    def __init__(self, steps: list[AbstractResolutionStep]) -> None:
        # Set up the default resolver steps and other attributes
        self._resolvers_steps = steps
        self._resolution_records = []
        self._record_git_metadata = DEFAULT_RECORD_GIT_METADATA
        if self._record_git_metadata:
            log.warning(
                "Recording metadata is enabled. This may incur additional overhead due to git commands."
            )

    def override_resolvers_steps(self, resolvers: list[AbstractResolutionStep]) -> None:
        """
        Override the list of resolver steps with a new list.

        :param resolvers: The new list of resolver steps to use.
        """
        self._resolvers_steps = resolvers

    def resolve(self, unresolved_src_path: UnresolvedPath) -> Path:
        """
        Resolve the given unresolved source path to a full path.

        :param unresolved_src_path: The unresolved source path to resolve.
        :return: The resolved full path.
        """

        resolved_path: Path | None = None
        outcome_str = "No resolution attempted"
        # One (step name, outcome) entry per step tried, to remind the user what
        # was attempted should resolution fail entirely.
        tried: list[tuple[str, OutcomeDescription]] = []

        if unresolved_src_path.is_absolute():
            # Input path is already resolved
            resolved_path = unresolved_src_path
            outcome_str = "Path was already absolute, no resolution needed"

        else:
            # Try each resolver in order
            for resolver_step in self._resolvers_steps:
                result, outcome_str = resolver_step(unresolved_src_path)
                tried.append((resolver_step.__class__.__name__, outcome_str))
                log.debug(
                    "Resolving %s: step %s returned outcome: %s",
                    f"../{unresolved_src_path.name}",
                    resolver_step.__class__.__name__,
                    outcome_str,
                )

                if result is not None:
                    # Successfully resolved
                    resolved_path = result
                    break

        if resolved_path is None:
            outcomes = "\n".join(f"  - {name}: {outcome}" for name, outcome in tried)
            raise FileNotFoundError(
                f"No resolver step could resolve path: {unresolved_src_path}\n"
                f"Resolution steps tried (in order):\n{outcomes}"
            )

        self.record_resolution_with_metadata(
            unresolved_src_path, resolved_path, outcome_str
        )

        return resolved_path

    def resolve_and_readtext(self, unresolved_src_path: UnresolvedPath) -> str:
        """
        Resolve the given unresolved source path and read its content.

        :param unresolved_src_path: The unresolved source path to resolve.
        :return: Text content.
        """
        return self.resolve(unresolved_src_path).read_text()

    def record_resolution_with_metadata(
        self,
        unresolved_src_path: UnresolvedPath,
        resolved_path: Path,
        outcome: OutcomeDescription,
    ) -> None:
        """
        Record a resolution event, by grabbing more metadata about the resolved
        file.
        """

        # Normalize the resolved path for the records
        resolved_path = resolved_path.resolve().absolute()

        log.debug(
            "Resolved %s to %s via %s",
            unresolved_src_path,
            resolved_path,
            outcome,
        )

        # Collect metadata
        if not self._record_git_metadata:
            git_revision = None
            git_dirty = None
        else:
            try:
                git_revision = subprocess.check_output(
                    ["git", "rev-parse", "HEAD"],
                    cwd=resolved_path.parent,
                    text=True,
                ).strip()
            except subprocess.CalledProcessError:
                git_revision = None

            try:
                git_dirty = bool(
                    subprocess.check_output(
                        ["git", "status", "--porcelain"],
                        cwd=resolved_path.parent,
                        text=True,
                    ).strip()
                )
            except subprocess.CalledProcessError:
                git_dirty = None

        metadata = GitMetadata(
            git_revision=git_revision,
            git_dirty=git_dirty,
        )

        self._resolution_records.append(
            ResolutionRecord(
                unresolved_src_path=unresolved_src_path,
                resolved_path=resolved_path,
                git_metatdata=metadata,
                outcome=outcome,
            )
        )

    def write_resolution_records_to_metadata(
        self,
        filename: str,
        subdir: Path | None = None,
    ) -> Path:
        """Write resolution records to CSV file in metadata directory."""
        # Determine output path
        if subdir:
            metadata_dir = get_metadata_dir(subdir)
            metadata_dir.mkdir(parents=True, exist_ok=True)
            output_path = metadata_dir / filename
        else:
            output_path = get_metadata_dir() / filename
            output_path.parent.mkdir(parents=True, exist_ok=True)

        # Write CSV
        with output_path.open("w", newline="") as csvfile:
            fieldnames = (
                [
                    "unresolved_src_path",
                    "resolved_path",
                    "git_revision",
                    "git_dirty",
                    "outcome",
                ]
                if self._record_git_metadata
                else ["unresolved_src_path", "resolved_path", "outcome"]
            )
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()

            for record in self._resolution_records:
                writer.writerow(
                    {
                        "unresolved_src_path": record.unresolved_src_path,
                        "resolved_path": str(record.resolved_path),
                        "git_revision": record.git_metatdata.git_revision,
                        "git_dirty": record.git_metatdata.git_dirty,
                        "outcome": record.outcome,
                    }
                    if self._record_git_metadata
                    else {
                        "unresolved_src_path": record.unresolved_src_path,
                        "resolved_path": str(record.resolved_path),
                        "outcome": record.outcome,
                    }
                )

        return output_path

    @abstractmethod
    def add_search_paths(self, *paths: Path) -> None:
        """
        Add search paths for resolving relative source paths.
        Concrete implementations must define how to handle added search paths
        depending on their resolver steps.

        :param paths: Set of directories to add to the search paths.
        """
        ...


def _find_in_dirs(path: Path, dirs: set[Path]) -> Path | None:
    """Helper"""
    for d in dirs:
        candidate = d / path
        if candidate.exists():
            return candidate

    return None
