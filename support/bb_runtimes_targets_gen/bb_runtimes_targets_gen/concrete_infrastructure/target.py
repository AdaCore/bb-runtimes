#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.base_types import (
    PlatformIdType,
    ValidBaseProfileType,
)
from rts_prebuilder.end_user_data.compiler_selector import using_gcc_compiler

from ..components import TargetDefaultSwitchesComponent
from .def_gpr_xml_gen import RuntimeXmlSource


class Target(AbstractTarget):
    """
    Mainly same things as AbstractTarget but with some default definitions.

    The attributes that are added here (that are not overrides) are
    attributes that are not needed by the `engine` when prebuilding
    the runtime.
    """

    def __init__(self) -> None:
        super().__init__()
        # Baseline runtime-build switches every target starts from. Added before
        # any subclass composition so its switches lead the merged groups. The
        # profile's own components live on the Profile (filled by the factory
        # from the profile's components factory), not in this always-on tree.
        self.add_dependency(TargetDefaultSwitchesComponent())

    @property
    @override
    def build_script(self) -> Path | None:
        return Path("shared/default_build.py")

    @property
    @override
    def prepare_build_script(self) -> Path | None:
        return Path("shared/empty_prebuild.py")

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        """Linker spec file to install at the runtime root for this base
        profile, or None if none is needed.

        The default supplies the spec that provides C++ ctors/dtors and
        exception tables. It does so only when the build needs it, meaning bare
        metal, GCC and no CHERI. It returns None for OS, CHERI and LLVM builds.
        Override to point at a target-specific spec.
        """
        if self.is_os_target or self.has_cheri or not using_gcc_compiler():
            return None
        if base_profile in ("light", "light-tasking"):
            return Path("datafiles/link-noexceptions.spec")
        if base_profile == "embedded":
            return Path("datafiles/link-zcx.spec")
        return None

    @property
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        """Declares where this target's runtime.xml comes from. ``None`` (the
        default) means *generate it* from the target's attributes and component
        tree. OS targets that ship a hand-maintained file override this and
        return a ``RuntimeXmlSource``, which is data, not a generation
        method."""
        return None


class DFBBTarget(Target):
    """BB target with single and double FPU"""

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "bb"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_timer_64(self) -> bool:
        return False
