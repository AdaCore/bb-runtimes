#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import (
    Target,
    light_profile,
)
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType


class ArmFreeRTOS(Target):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb")
        self.add_app_switches(link_switches=("-Wl,-r", "-nostdlib"))

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "freertos"

    @property
    @override
    def target(self) -> str:
        return "arm-eabi"

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-xi-arm.ads"))]


class ArmV7AFP_FreeRTOS(ArmFreeRTOS):
    @property
    @override
    def has_timer_64(self) -> bool:
        return True

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
    def name(self) -> str:
        return "v7a-fp"

    @property
    @override
    def cli_name(self) -> str:
        return "arm-freertos-v7a-fp"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-march=armv7-a+fp",
                "-mfloat-abi=hard",
                "-marm",
                "-mno-unaligned-access",
            ),
        )


TARGETS = [ArmV7AFP_FreeRTOS()]
