#
# Copyright (C) 2025-2026, AdaCore
#

"""Linux target definitions."""

from pathlib import Path
from typing import override

from bb_runtimes_targets_gen.components.atomics import AtomicSupport

from bb_runtimes_targets_gen.concrete_infrastructure import (
    DFBBTarget,
    light_profile,
    light_tasking_profile,
)
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType


class Linux(DFBBTarget):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb")
        self.add_gnarl_sources("linux/adaint.c")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "linux"

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @property
    @override
    def use_certifiable_packages(self) -> bool:
        return True

    @property
    @override
    def name(self) -> str:
        return self.target

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)


class X86Linux(Linux):
    @property
    @override
    def atomic_support(self) -> AtomicSupport:
        return AtomicSupport.UP_TO_64_BITS

    @property
    @override
    def target(self) -> str:
        return "x86-linux"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-native-x86-light.ads")),
            light_tasking_profile(self, Path("system-native-x86-light-tasking.ads")),
        ]


class X8664Linux(Linux):
    @property
    @override
    def target(self) -> str:
        return "x86_64-linux"

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-native-x86-light.ads")),
            light_tasking_profile(self, Path("system-native-x86-light-tasking.ads")),
        ]


class Aarch64Linux(Linux):
    @property
    @override
    def has_fma(self) -> bool:
        return True

    @property
    @override
    def target(self) -> str:
        return "aarch64-linux"

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-native-arm-light.ads")),
            light_tasking_profile(self, Path("system-native-arm-light-tasking.ads")),
        ]


TARGETS = [X86Linux(), X8664Linux(), Aarch64Linux()]
