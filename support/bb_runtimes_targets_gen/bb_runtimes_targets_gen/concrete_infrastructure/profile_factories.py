#
# Copyright (C) 2026, AdaCore
#

"""Factories for the standard runtime profiles.

A target declares its runtimes by returning ``Profile`` instances from
``profiles``. These helpers build the five standard ones. They set the right
base profile and fill ``Profile.components`` from that profile's components
factory, so a target only supplies the system.ads.

A custom commercial profile can pass a different ``name`` here. It can also
build a ``Profile`` directly with a hand-picked ``components`` set.
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import ProfileNameType

from .profile_components import (
    cert_components,
    embedded_components,
    light_components,
    light_tasking_components,
    no_profile_components,
)

if TYPE_CHECKING:
    from .target import Target


def none_profile(
    target: Target, system_ads: Path, *, name: ProfileNameType = "none"
) -> Profile:
    """Minimal "none" base profile (a base for custom, hand-picked profiles)."""
    return Profile(
        name=name,
        base_profile="none",
        system_ads=system_ads,
        components=no_profile_components(target),
    )


def light_profile(
    target: Target, system_ads: Path, *, name: ProfileNameType = "light"
) -> Profile:
    """The light profile (no tasking, no GNARL)."""
    return Profile(
        name=name,
        base_profile="light",
        system_ads=system_ads,
        components=light_components(target),
    )


def light_tasking_profile(
    target: Target,
    system_ads: Path,
    *,
    name: ProfileNameType = "light-tasking",
) -> Profile:
    """The light-tasking profile: Ravenscar SFP, and it provides GNARL."""
    return Profile(
        name=name,
        base_profile="light-tasking",
        system_ads=system_ads,
        components=light_tasking_components(target),
    )


def embedded_profile(
    target: Target,
    system_ads: Path,
    *,
    name: ProfileNameType = "embedded",
) -> Profile:
    """The embedded (full) profile (provides GNARL)."""
    return Profile(
        name=name,
        base_profile="embedded",
        system_ads=system_ads,
        components=embedded_components(target),
    )


def cert_profile(
    target: Target, system_ads: Path, *, name: ProfileNameType = "cert"
) -> Profile:
    """The cert profile (no GNARL)."""
    return Profile(
        name=name,
        base_profile="cert",
        system_ads=system_ads,
        components=cert_components(target),
    )
