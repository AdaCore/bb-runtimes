#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass
from pathlib import Path

from rts_prebuilder.base_types import UnresolvedPath

TEMPLATE_EXT = ".tmpl"


@dataclass()
class SourceFile:
    """
    This is a frozen dataclass usable to declare source files to be used in
    a RTS assembly or targetization. "file" here means any kind of file
    including directory, binary file, text file, linker script...

    This is frozen because it's considered as Data from the user (infrastructure
    extender user), any change to that data should be done in post processing
    in later transfomation steps.
    """

    unresolved_src_path: UnresolvedPath
    """
    Original Path of the source file/dir that needs to be resolved to an actual file
    see UnresolvedPath
    """

    dest_subdir: Path | None = None
    """
    Subdirectory in the output tree where the file/dir should be placed

    If None, the file is placed at the root of the output tree.
    """

    dest_path: Path | None = None
    """
    Output path within the dest_subdir, if None the same unresolved_src_path is used.
    If unresolved_src_path has a `__XXX.ext` suffix, it is removed in dest_path
    only the extension is kept.
    """

    def __post_init__(self) -> None:
        """
        Post-initialization to compute the destination basename.

        This makes sure that dest_path is never None after initialization.
        And some other defensive checks.
        """

        if self.dest_path is not None:
            if str(self.dest_path).endswith(TEMPLATE_EXT):
                raise ValueError(
                    f"dest_path cannot contain {TEMPLATE_EXT} extension/marker - "
                    f"it is only acceptable in unresolved_src_path"
                )

        else:
            # Compute dest_path from unresolved_src_path
            basename = self.unresolved_src_path.name

            # Remove .tmpl extension if present
            if basename.endswith(TEMPLATE_EXT):
                basename = basename[: -len(TEMPLATE_EXT)]

            if "__" in basename:
                # Split in parts to isolate the extension
                parts = basename.rsplit(".", 1)
                if len(parts) != 2:
                    raise ValueError(f"Unexpected filename {basename}")
                # Reconstitute the base name without the variant part
                self.dest_path = Path(parts[0].rsplit("__", 1)[0] + f".{parts[1]}")
            else:
                self.dest_path = Path(basename)

    def is_template(self) -> bool:
        """
        Returns True if this source file is a template file (ends with .tmpl).
        """
        return self.unresolved_src_path.name.endswith(TEMPLATE_EXT)


@dataclass()
class LinkerScriptSourceFile(SourceFile):
    """
    LinkerScriptSourceFile are specialization of SourceFile dataclass
    with an additional information specifi to linker scripts.
    """

    loaders: tuple[str, ...] | None = None
    """Linkers scripts can be associated with one or many loaders (RAM, EEPROM...)
    """


@dataclass()
class LinkerSwitch:
    """
    Linker related switch (example -L...)
    """

    switch: str
    """Flag value"""

    loader: str | None
    """Each flag can be associated to one loader or None"""
