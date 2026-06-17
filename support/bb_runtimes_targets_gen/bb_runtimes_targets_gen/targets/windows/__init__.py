#
# Copyright (C) 2025-2026, AdaCore
#

from bb_runtimes_targets_gen.concrete_infrastructure import DFBBTarget
from rts_prebuilder.base_types import ProfileNameType


class Windows(DFBBTarget):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb", "shared/s-textio__stdio.adb")

    @property
    def name(self):
        return self.target

    def has_libc(self, base_profile):
        return True

    @property
    def is_os_target(self) -> bool:
        return True

    def dump_runtime_xml(self, rts_name, rts):
        return (
            '<?xml version="1.0" ?>\n'
            "<gprconfig>\n"
            "  <configuration>\n"
            "  </configuration>\n"
            "</gprconfig>\n"
        )

    def runtime_name_generator(self, profile: ProfileNameType) -> str:
        return self.legacy_runtime_name_generator(profile)


class X86Windows(Windows):
    @property
    def target(self):
        return "x86-windows"

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}


class X8664Windows(Windows):
    @property
    def target(self):
        return "x86_64-windows"

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}


TARGETS = [X86Windows(), X8664Windows()]
