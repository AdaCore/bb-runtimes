#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import (
    DFBBTarget,
    RuntimeXmlSource,
    light_profile,
)
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import ValidBaseProfileType


class Windows(DFBBTarget):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb", "shared/s-textio__stdio.adb")

    @property
    @override
    def name(self) -> str:
        return self.target

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("windows/runtime.xml"))

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)


class X86Windows(Windows):
    @property
    @override
    def target(self) -> str:
        return "x86-windows"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-native-x86-light.ads"))]


class X8664Windows(Windows):
    @property
    @override
    def target(self) -> str:
        return "x86_64-windows"

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-native-x86-light.ads"))]


TARGETS = [X86Windows(), X8664Windows()]
