#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass

from rts_prebuilder.abstract_infrastructure import GprSwitches, SourceFile
from rts_prebuilder.base_types import (
    GnatOrGnarlType,
    ProfileNameType,
    ValidBaseProfileType,
)


@dataclass
class RuntimeSpec:
    """
    Profile-specific description of one target x profile runtime.

    Produced by ``specialize`` (which resolves every target and profile
    decision into it) and consumed by ``RuntimeWriter`` (which turns it
    into installed files). It holds no reference to the target: all the values
    the writer needs are already resolved here.
    """

    name: str
    """The runtime's official name (distinct from the profile / target name)."""

    profile_name: ProfileNameType
    """Profile this runtime is built for."""

    base_profile_name: ValidBaseProfileType
    """Base profile this profile maps to."""

    libs_tuple: tuple[GnatOrGnarlType, ...]
    """Libraries (gnat, gnarl) this runtime provides."""

    sources: list[SourceFile]
    """The target's component tree and the profile's components, resolved for
    this profile and flattened. system.ads is added separately by the writer.
    Holding a flat list rather than the tree is what keeps the spec
    target-free."""

    runtime_build_switches: GprSwitches
    """Switches for building the runtime itself. The writer substitutes these
    into the generated ``target_options.gpr``."""

    runtime_gnarl_switches: list[str]
    """Switches that build the runtime's tasking half alone."""

    runtime_debug_switches: list[str]
    """Switches that apply only to the Debug and Gnatcov builds."""

    system_ads_source_file: SourceFile
    """The system.ads source file to install, resolved from the target."""

    runtime_xml: str
    """The rendered runtime.xml content."""

    template_config: dict[str, str]
    """Template key/value mapping applied when installing templated sources."""

    target_triple: str
    """The gprbuild target triple. Ignored when ``is_native``."""

    is_native: bool
    """Whether the target is native (no ``for Target use`` directive)."""
