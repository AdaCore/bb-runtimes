#
# Copyright (C) 2025-2026, AdaCore
#

"""Linux target definitions."""

from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import DFBBTarget
from rts_prebuilder.base_types import ProfileNameType, PlatformIdType


class Linux(DFBBTarget):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb")
        self.add_gnarl_sources("linux/adaint.c")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "linux"

    def has_libc(self, base_profile):
        return True

    @property
    def is_os_target(self) -> bool:
        return True

    @property
    def use_certifiable_packages(self):
        return True

    @property
    def name(self):
        return self.target

    def runtime_name_generator(self, profile: ProfileNameType) -> str:
        return self.legacy_runtime_name_generator(profile)


class X86Linux(Linux):
    @property
    def target(self):
        return "x86-linux"

    @property
    def system_ads(self):
        return {
            "light": "system-native-x86-light.ads",
            "light-tasking": "system-native-x86-light-tasking.ads",
        }


class X8664Linux(Linux):
    @property
    def target(self):
        return "x86_64-linux"

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-native-x86-light.ads",
            "light-tasking": "system-native-x86-light-tasking.ads",
        }


class Aarch64Linux(Linux):
    @property
    def target(self):
        return "aarch64-linux"

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-native-arm-light.ads",
            "light-tasking": "system-native-arm-light-tasking.ads",
        }


TARGETS = [X86Linux(), X8664Linux(), Aarch64Linux()]
