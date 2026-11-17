#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure.common.source_file import (
    SourceFile,
    TemplateFile,
)
from rts_prebuilder.end_user_data.logger import get_logger

from .infrastructure_interface import get_resolver
from .template import Template

log = get_logger(__name__)


class FilePair:
    """
    This class is a post processed variant of SourceFile dataclass.

    It has two jobs:

    - ask the resolver for a full path to the source file
    - wrap the installer function that copies or links the file to its final
      destination
    - Provide different helper methods.

    Warning: A SourceFile may represent a dir, implementation should
             take that into account.
    """

    _dest_path: Path
    """
    Path of the destination path of the file, relative to the install dir
    """

    _resolved_src_path: Path
    """
    Full resolved path of the source file (it's guaranteed to exist
    if __init__ returns), absolute path or relative to process pwd.
    """

    def __init__(self, source_file: SourceFile) -> None:
        """
        Initializes the FilePair object from a SourceFile dataclass.

        This calls the resolver since FilePair must exist on disk.

        :param source_file: The original source file dataclass as defined by the user, see SourceFile
        """
        # Store source_file for later reference
        self._source_file = source_file

        if source_file.dest_subdir:
            dest_subdir = source_file.dest_subdir
        else:
            dest_subdir = Path()

        if source_file.dest_path is None:
            raise RuntimeError(
                "SourceFile.dest_path should never be None at this point"
                "This is taken care of by SourceFile.__post_init__"
            )

        self._dest_path = dest_subdir / source_file.dest_path

        resolver = get_resolver()
        self._resolved_src_path = resolver.resolve(source_file.unresolved_src_path)

    @override
    def __str__(self) -> str:
        return (
            f"Filepair (src: {self._resolved_src_path} -> dst_path: {self._dest_path})"
        )

    @override
    def __eq__(self, other: object) -> bool:
        """
        Temporary implementation of equality operator to make sure it's not used.
        """
        raise Exception("__eq__ not implemented for FilePair")

    def install(self, install_dir: Path, link: bool) -> None:
        """Copy or link the source into its declared destination."""
        if self._resolved_src_path.is_dir():
            # Copy contents of directory (flatten structure)
            # When source is a directory, we copy all its contents directly to the dest_subdir
            # without preserving the source directory name itself
            # IMPORTANT: We only copy FILES from the directory, NOT subdirectories.
            # Subdirectories are only processed if they are explicitly listed as
            # sources (e.g., by a component registering "common/32" as a
            # separate source).
            # Calculate the target directory (install_dir + dest_subdir if any)
            if self._source_file.dest_subdir:
                target_dir = install_dir / self._source_file.dest_subdir
            else:
                target_dir = install_dir

            log.debug(
                "Copying contents of directory %s to %s",
                self._resolved_src_path,
                target_dir,
            )
            from rts_prebuilder.base_types import UnresolvedPath

            for item in self._resolved_src_path.iterdir():
                if item.is_file():
                    # For each file, create a FilePair and install it to target_dir
                    file_source = SourceFile(
                        unresolved_src_path=UnresolvedPath(item),
                        dest_subdir=None,  # Already included in item
                    )
                    file_pair = FilePair(file_source)
                    file_pair.install(target_dir, link)
                # Note: We intentionally skip subdirectories here.
                # They will be processed separately if they are explicitly listed as sources.
            return

        content = self._resolved_src_path.read_text(encoding="utf-8")

        # Install the file with the content
        _single_file_installer(
            install_dir,
            self._resolved_src_path,
            self._dest_path,
            link,
            content,
        )

    def is_dir(self) -> bool:
        """
        Returns True if the source file is a directory.
        """
        return self._resolved_src_path.is_dir()

    @property
    def suffixes(self) -> set[str]:
        """
        Returns the suffixes of the source file as a set of strings.
        It can be plural in case self.is_dir() is True. Since it will
        represent all suffixes of all files in the directory.
        """
        suffixes: set[str] = set()
        if self.is_dir():
            for item in self._resolved_src_path.iterdir():
                if item.is_file():
                    suffixes.add(item.suffix)
        else:
            suffixes.add(self._resolved_src_path.suffix)

        return suffixes

    def is_in_dir(self, _dir: Path) -> bool:
        """
        Returns True if the destination path is in the given directory.
        """
        if len(self._dest_path.parts) > 0:
            return self._dest_path.parts[0] == _dir.name
        else:
            return False


class TemplateFilePair(FilePair):
    """Resolve a template and install its rendered content at the declared destination."""

    def __init__(self, source_file: TemplateFile, values: dict[str, str]) -> None:
        super().__init__(source_file)
        self._template = Template(self._resolved_src_path)
        self._values = values

    @override
    def install(self, install_dir: Path, link: bool) -> None:
        # Write rendered content even when linking is requested, because linking the
        # template input would leave placeholders in the installed file.
        _single_file_installer(
            install_dir,
            self._resolved_src_path,
            self._dest_path,
            False,
            self._template.render(self._values),
        )

    @property
    @override
    def suffixes(self) -> set[str]:
        # Use the destination suffix for language detection because .tmpl identifies
        # the input as a template without identifying its output language.
        return {self._dest_path.suffix}


def _single_file_installer(
    install_dir: Path, src: Path, dst: Path, link: bool, content: str
) -> None:
    """Copy (or symlink) src to dst.

    :param install_dir: The installation directory
    :param src: Source file path
    :param dst: Destination file path (relative to install_dir)
    :param link: If true, files will be linked instead of copied during install
    :param content: The file content to install
    """
    if not src.is_file():
        raise FileNotFoundError(f"runtime file {src} does not exist")

    # Prepend install dir
    full_dst = install_dir / dst

    log.debug("Installing %s to %s", src, full_dst)

    if full_dst.is_file():
        raise FileExistsError(f"{full_dst} was already installed, from {src}")

    # Ensure parent directory exists
    full_dst.parent.mkdir(parents=True, exist_ok=True)

    if link:
        full_dst.symlink_to(src.absolute())
    else:
        # Write content directly.
        # newline="\n": disable newline to os.linesep translation, otherwise
        # the run-time sources get CRLF terminators on Windows, which GNAT
        # rejects under -gnatyd (incorrect line terminator).
        full_dst.write_text(content, encoding="utf-8", newline="\n")
