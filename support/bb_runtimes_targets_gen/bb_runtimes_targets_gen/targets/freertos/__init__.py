#
# Copyright (C) 2025-2026, AdaCore
#

from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import Target
from rts_prebuilder.base_types import PlatformIdType


class ArmFreeRTOS(Target):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb")
        self.add_linker_switch("-Wl,-r", loader=None)
        self.add_linker_switch("-nostdlib", loader=None)

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "freertos"

    @property
    def target(self):
        return "arm-eabi"

    def has_libc(self, base_profile):
        return True

    @property
    def is_os_target(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
        }


class ArmV7AFP_FreeRTOS(ArmFreeRTOS):
    @property
    def has_timer_64(self) -> bool:
        return True

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    def name(self):
        return "v7a-fp"

    @property
    def cli_name(self):
        return "arm-freertos-v7a-fp"

    @property
    def compiler_switches(self):
        return (
            "-march=armv7-a+fp",
            "-mfloat-abi=hard",
            "-marm",
            "-mno-unaligned-access",
        )


TARGETS = [ArmV7AFP_FreeRTOS()]
