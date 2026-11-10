#
# Copyright (C) 2025-2026, AdaCore
#

"""Registering a PikeOS linker script as a source under ``ld``.

Every other target names its scripts on a loader, so the generated runtime.xml
carries the ``-T``. PikeOS ships its own runtime.xml and links with its own
tooling, so its scripts only need copying: there is no loader to name them and
no ``-T`` to generate. That is specific enough to PikeOS that it stays here
rather than in the shared component API.
"""

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import (
    SourceFile,
    SourcesAndFlagsComponent,
)
from rts_prebuilder.base_types import UnresolvedPath


def add_linker_script(component: SourcesAndFlagsComponent, source: Path) -> None:
    """Register a linker script that goes under ``ld``, linked by nothing.

    :param component: the component to register it on, from its ``contribute``.
    :param source: the script's source path.
    """
    component.append_sources(
        SourceFile(
            unresolved_src_path=UnresolvedPath(source),
            dest_subdir=Path("ld"),
        )
    )
