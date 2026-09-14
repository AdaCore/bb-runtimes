#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass
from pathlib import Path

from rts_prebuilder.base_types import UnresolvedPath

TEMPLATE_EXT = ".tmpl"


def dest_name(source: Path) -> Path:
    """The destination name a source path maps to: its basename without the
    ``.tmpl`` marker and without a ``__variant`` part, so every variant of a
    file maps to the same name.

    :param source: the source path.
    :returns: the destination file name.
    :raises ValueError: if a ``__variant`` name has no extension to keep.
    """
    basename = source.name

    if basename.endswith(TEMPLATE_EXT):
        basename = basename[: -len(TEMPLATE_EXT)]

    if "__" not in basename:
        return Path(basename)

    parts = basename.rsplit(".", 1)
    if len(parts) != 2:
        raise ValueError(f"Unexpected filename {basename}")
    return Path(parts[0].rsplit("__", 1)[0] + f".{parts[1]}")


@dataclass()
class SourceFile:
    """
    Declares a source file of a RTS assembly or prebuilding. "file" here means
    any kind of file: a directory, a binary file, a text file, a linker script
    and so on.

    This is data from the user (the infrastructure extender), so treat it as
    read-only once built: any change belongs in a later transformation step.
    It cannot be a frozen dataclass because ``__post_init__`` fills in
    ``dest_path``.
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
            self.dest_path = dest_name(self.unresolved_src_path)

    def is_template(self) -> bool:
        """
        Returns True if this source file is a template file (ends with .tmpl).
        """
        return self.unresolved_src_path.name.endswith(TEMPLATE_EXT)


@dataclass(frozen=True)
class Loader:
    """One link configuration of the end user's program, selected with
    ``-XLOADER`` by the user when building the application against this runtime.

    Any component declares loaders, with ``add_loaders``, and the runtime offers
    every one its tree declares. Which of them the project defaults to is
    ``AbstractTarget.default_loader``.

    A loader is a whole value. Two never merge, so their names must stay
    distinct across a runtime, and a component cannot put a script into a
    loader another component declared. A loader with no scripts and no
    switches is bare: it adds no loader-specific link options."""

    name: str
    """The name as it appears in the generated project's ``Loaders`` type and in
    ``-XLOADER``."""

    linker_scripts: tuple[Path, ...] = ()
    """The scripts this loader links, in ``-T`` order. They go under ``ld`` in
    the runtime. A script whose name carries a ``__variant`` suffix drops it
    there, so the ``-T`` is the same whichever variant was selected."""

    included_scripts: tuple[Path, ...] = ()
    """The scripts this loader puts under ``ld`` without linking them:
    those a ``linker_scripts`` entry pulls in with ``INCLUDE``, which passing
    with ``-T`` as well would define twice, and those shipped for the ``USER``
    arm to link by hand."""

    link_switches: tuple[str, ...] = ()
    """Extra switches for linking the end user's program with this loader, after
    its scripts."""


@dataclass(frozen=True)
class GprSwitches:
    """One set of gprbuild switches, split the way a gpr project splits them.

    Both builds use this same type. The runtime's own build lands in
    ``target_options.gpr``. The end user's build against the runtime lands in
    ``runtime.xml``. Each field has a slot on either side, so no field is dead
    on one of them.

    There is no Ada field, because neither generated file has an Ada-only slot.
    The runtime's Ada switches are fixed in the ``target_options.gpr`` template.
    An application's Ada compilation takes ``common`` alone."""

    common: tuple[str, ...] = ()
    """Switches for every language: -mcpu/-march/-mabi and the like.
    ``Common_Switches`` for the runtime, ``Common_Required_Switches`` for the
    app."""

    c: tuple[str, ...] = ()
    """Extra switches for the C family, meaning C, C++ and Asm, which the C
    driver preprocesses. Not Ada."""

    asm: tuple[str, ...] = ()
    """Extra switches for Asm alone, on top of the C family's."""

    link: tuple[str, ...] = ()
    """Switches for the link. Which link depends on which build holds the set.
    The runtime build links its own library (``Runtime_Link_Switches``, and only
    when it is shared). The app build links the end user's program
    (``Linker'Required_Switches``)."""

    def merge(self, other: "GprSwitches") -> "GprSwitches":
        """This set followed by ``other``, field by field, in a new set.
        Neither operand is touched."""
        return GprSwitches(
            common=self.common + other.common,
            c=self.c + other.c,
            asm=self.asm + other.asm,
            link=self.link + other.link,
        )

    def __bool__(self) -> bool:
        """Whether any field carries a switch."""
        return bool(self.common or self.c or self.asm or self.link)
