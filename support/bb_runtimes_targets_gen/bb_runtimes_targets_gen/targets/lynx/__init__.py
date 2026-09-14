#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import (
    Target,
    light_profile,
    light_tasking_profile,
)
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType


class Lynx(Target):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb", "lynx/stdio_symbols.c")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "lynx"

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @property
    @override
    def has_command_line_arguments(self) -> bool:
        return True

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)


class PPCLynx(Lynx):
    def __init__(self) -> None:
        super().__init__()

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        # Disable FPU rts_prebuilder Otherwise, `__builtins_*` could fallbacks
        # on missing features of LynxOS libm.
        return False

    @property
    @override
    def target(self) -> str:
        return "ppc-lynx178"

    @property
    @override
    def name(self) -> str:
        return "lynx"

    @property
    @override
    def cli_name(self) -> str:
        return "ppc-lynx178"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-lynxos178-ppc.ads"))]


class Aarch64Lynx(Lynx):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # Build against "production" mode.
        self.add_runtime_switches(
            common_switches=("-DPRODUCTION=2", "-DBUILD_MODE=2"),
        )

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
    def target(self) -> str:
        return "aarch64-lynx178"

    @property
    @override
    def name(self) -> str:
        return "lynx"

    @property
    @override
    def cli_name(self) -> str:
        return "aarch64-lynx178"

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-lynxos178-aarch64-light.ads")),
            light_tasking_profile(
                self, Path("system-lynxos178-aarch64-light-tasking.ads")
            ),
        ]


TARGETS = [PPCLynx(), Aarch64Lynx()]
