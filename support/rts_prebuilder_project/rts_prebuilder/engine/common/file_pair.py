#
# Copyright (C) 2025-2026, AdaCore
#

import re
import sys
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure.common.source_file import (
    TEMPLATE_EXT,
    SourceFile,
)
from rts_prebuilder.end_user_data.logger import get_logger

from .infrastructure_interface import get_resolver

log = get_logger(__name__)

HEALTH_CHECK_WARNING_ENABLED: bool = True
"""
Setting this to false will disable the health check warning messages
Disable this if you temporarily want to suppress the warnings during testing
to fix them later. do not commit with this set to false.
"""

TemplateConfigType = dict[str, str]
"""
A dict of key/value to be applied as template instantiation
(only valid for *.tmpl files)
"""


class FilePair:
    """
    This class is a post processed variant of SourceFile dataclass.

    This class has the following responsibilities:

    - Call resolver to get a full path for the source file
    - Wraps the installer function to copy/link the file to its final
      destination with the template applied if any.
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

    def install(
        self, install_dir: Path, link: bool, template_config: TemplateConfigType
    ) -> None:
        """Install the file to the destination, applying templates if necessary.

        This method handles both template processing and file installation by
        calling the appropriate functions in sequence.
        """

        if self._resolved_src_path.is_dir():
            # Copy contents of directory (flatten structure)
            # When source is a directory, we copy all its contents directly to the dest_subdir
            # without preserving the source directory name itself
            # IMPORTANT: We only copy FILES from the directory, NOT subdirectories.
            # Subdirectories are only processed if they are explicitly listed as sources
            # (e.g., through scenario filtering that adds "common/32" as a separate source).
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
                    file_pair.install(target_dir, link, template_config)
                # Note: We intentionally skip subdirectories here.
                # They will be processed separately if they are explicitly listed as sources.
            return

        # Check if this is a template file that needs processing
        src_is_template = (
            self._resolved_src_path.suffix == TEMPLATE_EXT and len(template_config) > 0
        )

        # Get content - either processed template or original file content
        if src_is_template:
            content = _apply_template_to_file(self._resolved_src_path, template_config)
            # For templates, we can't use symlinks since we've processed the content
            link = False
        else:
            # The run-time sources may contain UTF-8 characters (copyright symbols, etc.)
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


def _apply_template_config(content: str, template_config: TemplateConfigType) -> str:
    """
    Replace all instances matching the format "${key}" with the corresponding
    value in template_config. The matching pattern is enclosed in
    double-quotes (") to avoid invalid characters in Ada source code.

    :param content: The original content to be modified
    :param template_config: See TemplateConfigType
    :return: The modified content with applied template configuration
    """

    def lookup(match: re.Match[str]) -> str:
        key = match.group(1)
        if key not in template_config:
            raise KeyError(f"key '{key}' not defined in template configuration")
        return str(template_config.get(key))

    return re.sub(r"\"\$\{([^\$\"\}]*)\}\"", lookup, content)


def _apply_template_to_file(src: Path, template_config: TemplateConfigType) -> str:
    """Apply template configuration to a source file and return the processed content.

    :param src: The source file path (must be a template file)
    :param template_config: Template configuration to apply
    :return: The processed file content with template variables replaced
    """
    if not src.is_file():
        log.error(f"runtime file {src} does not exists")
        sys.exit(4)

    src_cnt = src.read_text(encoding="us-ascii")

    if len(template_config) > 0:
        log.debug(f"Apply template config to {src}: {template_config}")
        src_cnt = _apply_template_config(src_cnt, template_config)

    return src_cnt


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
    # Remove template extension from destination filename, if any
    if dst.name.endswith(TEMPLATE_EXT):
        dst = dst.with_suffix("")

    if not src.is_file():
        log.error(f"runtime file {src} does not exists")
        sys.exit(4)

    # Prepend install dir
    full_dst = install_dir / dst

    log.debug("Installing %s to %s", src, full_dst)

    # Check if destination file already exists with same content
    if full_dst.is_file():
        dst_cnt = full_dst.read_text()
        if dst_cnt != content:
            log.error(f"File {full_dst} already exists with different content")
            sys.exit(4)
        else:
            if HEALTH_CHECK_WARNING_ENABLED:
                log.warning(
                    "File already exists & Same content, skip: %s, %s. "
                    "This works but it is not healthy, consider tracing the source of "
                    "duplicated sources (in verbose mode). and correctly discriminating"
                    "which one you want in the final install.",
                    src,
                    full_dst,
                )
            return

    # Ensure parent directory exists
    full_dst.parent.mkdir(parents=True, exist_ok=True)

    if link:
        full_dst.symlink_to(src.absolute())
    else:
        # Write content directly
        full_dst.write_text(content, encoding="utf-8")
