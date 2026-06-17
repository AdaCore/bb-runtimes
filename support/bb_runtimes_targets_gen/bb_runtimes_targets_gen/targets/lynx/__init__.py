#
# Copyright (C) 2025-2026, AdaCore
#

from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import Target
from rts_prebuilder.base_types import ProfileNameType, PlatformIdType


class Lynx(Target):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb", "lynx/stdio_symbols.c")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "lynx"

    def has_libc(self, base_profile):
        return True

    @property
    def is_os_target(self) -> bool:
        return True

    @property
    def has_command_line_arguments(self) -> bool:
        return True

    def runtime_name_generator(self, profile: ProfileNameType) -> str:
        return self.legacy_runtime_name_generator(profile)


class PPCLynx(Lynx):
    def __init__(self):
        super().__init__()

    @property
    def has_double_precision_fpu(self) -> bool:
        # Disable FPU rts_prebuilder Otherwise, `__builtins_*` could fallbacks
        # on missing features of LynxOS libm.
        return False

    @property
    def target(self):
        return "ppc-lynx178"

    @property
    def name(self):
        return "lynx"

    @property
    def cli_name(self) -> str:
        return "ppc-lynx178"

    @property
    def system_ads(self):
        return {
            "light": "system-lynxos178-ppc.ads",
        }


class Aarch64Lynx(Lynx):
    def __init__(self):
        super().__init__()

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    def target(self):
        return "aarch64-lynx178"

    @property
    def name(self):
        return "lynx"

    @property
    def cli_name(self) -> str:
        return "aarch64-lynx178"

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-lynxos178-aarch64-light.ads",
            "light-tasking": "system-lynxos178-aarch64-light-tasking.ads",
        }

    def amend_rts(self, rts_profile, cfg):
        cfg.build_flags["common_flags"] += [
            # Build against "production" mode.
            "-DPRODUCTION=2",
            "-DBUILD_MODE=2",
        ]


TARGETS = [PPCLynx(), Aarch64Lynx()]
