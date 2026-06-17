#
# Copyright (C) 2025-2026, AdaCore
#

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Tuple, override

from rts_prebuilder.base_types import UnresolvedPath
from rts_prebuilder.end_user_data.gnat_gcc_dir_selector import get_gccdir, get_gnatdir

from .helpers.read_manifest import read_manifest

type OutcomeDescription = str
"""
String describing the resolution method used.
"""


class AbstractResolutionStep(ABC):
    """
    Blueprint for a resolution step.

    NOTE: All resolution step subclasses MUST NOT take any arguments in their __init__ method.
    Because the resolution should use global state (e.g., gnatdir/gccdir from end_user_data...)
    at resolution time, not the one at instantiation time.
    """

    @abstractmethod
    def resolver_func(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        """
        Resolver function type blueprint.
        Each function takes an unresolved source path and returns a tuple of:
        - The resolved Path or None if it cannot resolve.
        - A string describing the resolution method used.
        """
        ...

    def __call__(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        """Make the class instance callable, delegating to resolver_func."""
        return self.resolver_func(unresolved_src_path)


class SearchPathsResolutionStep(AbstractResolutionStep):
    """
    This step uses a list of search paths to look for source files.
    """

    _search_paths: list[Path] = []

    def add_search_paths(self, *paths: Path) -> None:
        """
        Add a search path to the resolver.

        TODO: Consider adding a parameter to specify position in the list
              for more control.

        :param paths: The path to add to the search paths. (Order is preserved)
        """
        for path in paths:
            self._search_paths.append(path)

    @override
    def resolver_func(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        """Resolve the given unresolved source path by searching in the configured search paths."""
        for d in self._search_paths:
            candidate = d / unresolved_src_path
            if candidate.exists():
                return candidate, "File was found in search paths"

        return None, f"File not found in search paths ({self._search_paths})"


class GnatDirManifestResolutionStep(AbstractResolutionStep):
    """
    Resolution step that checks if a path exists in GNAT directory manifest.
    This step handles files that are listed in the GNAT manifest file.
    """

    @override
    def resolver_func(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        gnatdir = get_gnatdir()
        manifest = read_manifest(gnatdir)

        # Only handle single filenames (no path components)
        if len(unresolved_src_path.parts) != 1 or not gnatdir:
            return None, "Not a manifest file (has path components or no GNAT dir)"

        # Construct and verify the path
        resolved_path = gnatdir / unresolved_src_path

        if resolved_path.exists():
            # Check if file is in manifest
            if unresolved_src_path.name not in manifest:
                raise Exception(
                    f"Error: source file {unresolved_src_path} found in GNAT dir"
                    " but not listed in MANIFEST"
                )

            return (
                resolved_path,
                f"File was found in GNAT {gnatdir} dir root (from manifest)",
            )
        else:
            return None, "File not in GNAT dir"


class GnatDirLibsResolutionStep(AbstractResolutionStep):
    """
    Resolution step that checks if a file exists in GNAT runtime library directories.
    This step handles files in hie, libgnarl, and libgnat subdirectories.
    """

    @override
    def resolver_func(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        gnatdir = get_gnatdir()
        lib_dirs = ("hie", "libgnarl", "libgnat")

        # Check if path starts with one of the library directories
        if not gnatdir or not unresolved_src_path.parts:
            return None, "No GNAT dir configured or empty path"

        if unresolved_src_path.parts[0] not in lib_dirs:
            return None, "Path does not start with GNAT lib dir (hie/libgnarl/libgnat)"

        # Construct and verify the path
        resolved_path = gnatdir / unresolved_src_path
        if resolved_path.exists():
            return (
                resolved_path,
                "File was found in GNAT runtime libs (hie/libgnarl/libgnat)",
            )
        else:
            return None, "File expected in GNAT libs but not found on disk"


class GccDirResolutionStep(AbstractResolutionStep):
    """
    Resolution step that checks if a file exists in GCC directory.
    """

    @override
    def resolver_func(
        self, unresolved_src_path: UnresolvedPath
    ) -> Tuple[Path | None, OutcomeDescription]:
        gccdir = get_gccdir()

        if not gccdir:
            return None, "No GCC dir configured"

        # Construct and verify the path
        resolved_path = gccdir / unresolved_src_path
        if resolved_path.exists():
            return resolved_path, "File was found in GCC root dir"
        else:
            return None, "File not found in GCC dir"
