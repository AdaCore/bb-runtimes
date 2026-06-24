#
# Copyright (C) 2025-2026, AdaCore
#

"""
This modules gives Targets everything related to registering source files
"""

from pathlib import Path
from typing import TypeVar, final

from rts_prebuilder.base_types import UnresolvedPath

from ..common.source_file import LinkerScriptSourceFile, LinkerSwitch, SourceFile


class SourcesAndFlagsComponentMixin:
    """
    This class provides handlers to register information about sources / flags.

    Mainly it enables to register SourceFile/LinkerScriptSourceFile/LinkerSwitch
    objects for a given class.
    But it has a composer behavior, meaning that if the class has a parent
    property, it will also include the sources registered to the parent. (and so on
    recursively).

    TODO:3 It's a mixin only because we rely on children having the `parent`
    property. it can be changed by taking parent as an argument of the constructor.
    and use composition instead of inheritance.
    """

    _sources: list[SourceFile]
    """List of SourceFile objects registered to this composer"""

    _linker_switches: list[LinkerSwitch]
    """List of LinkerSwitch objects registered to this composer"""

    _sources_search_paths: list[Path]
    """List of paths to search for source files to be used in path resolution"""

    def __init__(self) -> None:
        """
        Initialize the sources list
        Rely on child classes to provide a parent property used
        as an extra source.
        """
        self._sources = []
        self._linker_switches = []
        self._sources_search_paths = []

    @property
    def parent(self) -> "SourcesAndFlagsComponentMixin | None":
        """
        Returns the parent "component". The sources/flags from the parent
        will be merged with self's ones.
        """
        return None

    "Setters"

    @final
    def get_sources_search_paths(self) -> list[Path]:
        """
        Returns the list of paths to search for source files.

        Merged with the parent search paths if any.
        """
        all_paths = self._sources_search_paths.copy()
        if self.parent:
            all_paths.extend(self.parent.get_sources_search_paths())
        return all_paths

    @final
    def prepend_sources_search_paths(self, *paths: Path) -> None:
        """
        Prepends paths to the sources search paths list.

        :param paths: The path(s) to prepend
        """
        self._sources_search_paths = list(paths) + self._sources_search_paths

    @final
    def append_sources(self, *Sources: SourceFile) -> None:
        self._sources.extend(Sources)

    @final
    def append_sources_for_dir(self, subdir: Path, *sources: Path) -> None:
        """
        Appends sources to the sources list, setting their dest_subdir to the
        given subdir.

        :param subdir: See SourceFile.dest_subdir
        :param sources: See SourceFile.unresolved_src_path
                        This function takes care of marking
                        as UnresolvedPath to reduce verbosity on caller side.
        """
        # Mark all sources as unresolved
        unresolved_paths_list = [UnresolvedPath(s) for s in sources]
        self.append_sources(
            *[
                SourceFile(dest_subdir=subdir, unresolved_src_path=src)
                for src in unresolved_paths_list
            ]
        )

    @final
    def add_gnarl_sources(self, *sources: str) -> None:
        self.append_sources_for_dir(Path("gnarl"), *[Path(s) for s in sources])

    @final
    def add_gnat_sources(self, *sources: str) -> None:
        self.append_sources_for_dir(Path("gnat"), *[Path(s) for s in sources])

    @final
    def _add_source_to_dir(
        self, subdir: Path, source: str, dest_name: str | None
    ) -> None:
        self.append_sources(
            SourceFile(
                dest_subdir=subdir,
                unresolved_src_path=UnresolvedPath(Path(source)),
                dest_path=Path(dest_name) if dest_name is not None else None,
            )
        )

    @final
    def add_gnat_source(self, source: str, dest_name: str | None = None) -> None:
        self._add_source_to_dir(Path("gnat"), source, dest_name)

    @final
    def add_gnarl_source(self, source: str, dest_name: str | None = None) -> None:
        self._add_source_to_dir(Path("gnarl"), source, dest_name)

    @final
    def add_build_scripts(self, build_script: Path, prebuild_script: Path) -> None:
        """Register the scripts an end user runs to rebuild the runtime after
        changing something in it, installed at the runtime root: build.py (runs
        gprbuild) and the pre_build.py it calls first (a target-specific step,
        a no-op by default). The two always go together.
        """
        self.append_sources(
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(build_script),
                dest_path=Path("build.py"),
            ),
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(prebuild_script),
                dest_path=Path("pre_build.py"),
            ),
        )

    @final
    def add_linker_script(
        self,
        source_str: str,
        loaders: tuple[str, ...] | None = None,
        dest_path_str: str | None = None,
    ) -> None:

        if loaders and not isinstance(loaders, tuple):
            raise Exception(
                "Linker script loaders must be a tuple of strings"
                f", you provided: {loaders!r} ({type(loaders)})."
                "Reminder: ('foo') is a string, not a tuple."
                "Use ('foo',) for a single-element tuple."
            )

        self.append_sources(
            LinkerScriptSourceFile(
                unresolved_src_path=UnresolvedPath(Path(source_str)),
                loaders=loaders,
                dest_subdir=Path("ld"),
                dest_path=Path(dest_path_str) if dest_path_str else None,
            )
        )

    "Getters"

    @final
    def get_sources(
        self, dest_subdir: Path | None = None, exclude_subdirs: list[Path] | None = None
    ) -> list[SourceFile]:
        """
        Returns the list of sources registered to this composer.
        All sources from parent composers are also included (with same
        args).

        :param dest_subdir: If set, only sources with this dest_subdir are returned
        :param exclude_subdirs: If set, sources with these dest_subdirs are excluded
        """

        sources: list[SourceFile] = []

        for s in self._sources:
            if exclude_subdirs and s.dest_subdir and s.dest_subdir in exclude_subdirs:
                continue

            if dest_subdir and s.dest_subdir and s.dest_subdir == dest_subdir:
                sources.append(s)
            elif not dest_subdir:
                sources.append(s)

        if self.parent:
            sources.extend(
                self.parent.get_sources(
                    dest_subdir=dest_subdir, exclude_subdirs=exclude_subdirs
                )
            )

        return sources

    # T is any subtype of SourceFile
    T = TypeVar("T", bound="SourceFile")

    @final
    def get_sources_of_type(self, filter_type: type[T]) -> list[T]:
        """
        Returns the list of sources of a given type

        :param filter_type: The type of source to filter for
        """
        return [s for s in self.get_sources() if isinstance(s, filter_type)]

    """
    Following methods deal with special linker script sources
    """

    @property
    @final
    def linker_scripts(self) -> list[LinkerScriptSourceFile]:
        return self.get_sources_of_type(LinkerScriptSourceFile)

    @property
    def loaders(self) -> tuple[str, ...]:
        """
        Helper to get the list of all loaders defined by the target's
        linker scripts.
        This list can be overriden to expose just a subset of loaders,
        if you're overidding it, make sure it's not cleaner no inherit
        from a SourcesAndFlagsComponentMixin class that does not define
        loaders you need to hide.
        # TODO:3 Check if the comment is correct and check if this
        # practive should be encouraged or not.
        """
        return tuple(
            [
                loader
                for ld in self.linker_scripts
                if ld.loaders
                for loader in ld.loaders
            ]
        )

    @property
    @final
    def linker_switches(self) -> list[LinkerSwitch]:
        """
        Returns the list of additional linker switches for this composer

        Merged with the parent linker switches if any
        """
        all_switches = self._linker_switches.copy()
        if self.parent:
            all_switches.extend(self.parent.linker_switches)
        return all_switches

    @final
    def add_linker_switch(self, switch: str, loader: str | None = None) -> None:
        """
        Adds additional linker switch to the ArchSupport object

        :param switch: Raw options to be passed to the linker
        :param loader: The loader that the switch is applicable to, can be None.
        """
        self._linker_switches.append(LinkerSwitch(switch=switch, loader=loader))
