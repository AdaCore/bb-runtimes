#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType
from bb_runtimes_targets_gen.concrete_infrastructure import (
    Target,
    light_profile,
    light_tasking_profile,
)


class QNX(Target):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb")

    @property
    @override
    def prepare_build_script(self) -> Path:
        # Installed at the runtime root as prepare_build.py. build.py calls
        # its prebuild_step_run(obj_dir) before gprbuild, to create the dummy
        # last-chance handler lib the runtime links against.
        return Path("qnx/prepare_build.py")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "qnx"

    @property
    @override
    def has_command_line_arguments(self) -> bool:
        return True

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @property
    @override
    def use_certifiable_packages(self) -> bool:
        return True


class Aarch64QNX(QNX):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # The traceback implementation in our restricted runtimes for this
        # platform relies on all frames having a frame pointer, so make sure
        # it is always there. See V217-008 for more info.
        self.add_runtime_switches(
            common_switches=("-fno-omit-frame-pointer",),
        )
        # Add an explicit dependency on libada_lch so that the last chance
        # handler is loaded when we use the Ada runtime.
        self.add_runtime_switches(
            link_switches=("-lada_lch",),
        )

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def target(self) -> str:
        return "aarch64-qnx"

    @property
    @override
    def name(self) -> str:
        return "qnx"

    @property
    @override
    def cli_name(self) -> str:
        return "aarch64-qnx"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-qnx-arm-light.ads")),
            light_tasking_profile(self, Path("system-qnx-arm-light-tasking.ads")),
        ]


class ARMQNX(QNX):
    def __init__(self) -> None:
        super().__init__()

    @property
    @override
    def is_64bit(self) -> bool:
        return False

    @property
    @override
    def target(self) -> str:
        return "arm-qnx"

    @property
    @override
    def name(self) -> str:
        return "qnx"

    @property
    @override
    def cli_name(self) -> str:
        return "arm-qnx"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-qnx-arm-light.ads")),
            light_tasking_profile(self, Path("system-qnx-arm-light-tasking.ads")),
        ]


TARGETS = [Aarch64QNX(), ARMQNX()]
