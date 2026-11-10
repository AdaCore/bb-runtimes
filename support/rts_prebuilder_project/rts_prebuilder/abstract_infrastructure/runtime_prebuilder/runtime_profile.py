#
# Copyright (C) 2026, AdaCore
#

"""
A runtime profile a target offers.

A ``Profile`` is a pure-data value object describing one of a target's
runtimes. It carries the commercial name, the base profile it maps to, the
system.ads, and the components that make up the runtime. It holds no selection
logic: the target's
factory fills ``components`` (from the profile's components factory) when
building the profile. A target declares its runtimes by returning a list of
these from ``AbstractTarget.profiles``.

The libraries a runtime provides are not declared here. The specializer derives
them from the sources the components actually install.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

from rts_prebuilder.base_types import ProfileNameType, ValidBaseProfileType

from .component import SourcesAndFlagsComponent


@dataclass
class Profile:
    """One runtime a target offers, as a value object (see module docstring)."""

    name: ProfileNameType
    """Commercial name of the profile (e.g. ``light-tasking``, ``auto-nvdrive``).
    Names must be unique within a target: they drive the runtime fan-out."""

    base_profile: ValidBaseProfileType
    """The base profile this maps to, one of ``VALID_BASE_PROFILES_SET``.
    Drives component selection and the runtime name."""

    system_ads: Path
    """The system.ads to install. A bare filename is looked up under
    ``system/``; a path carrying a directory component is used as-is."""

    components: list[SourcesAndFlagsComponent] = field(default_factory=list)
    """The components that make up this runtime, on top of the target's own
    always-on tree. The factory fills it from the profile's components factory
    for the standard profiles; a custom runtime can hand-pick its own set
    here."""
