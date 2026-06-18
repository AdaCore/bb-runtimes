#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override
from rts_prebuilder.base_types import ProfileNameType, PlatformIdType
from bb_runtimes_targets_gen.concrete_infrastructure import Target


class QNX(Target):
    def __init__(self):
        super(QNX, self).__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb")

    @property
    @override
    def prebuild_script(self) -> Path:
        # Installed at the runtime root as pre_build.py; build.py calls its
        # prebuild_step_run(obj_dir) before gprbuild to create the dummy
        # last-chance handler lib the runtime links against.
        return Path("qnx/pre_build.py")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "qnx"

    @property
    def has_command_line_arguments(self) -> bool:
        return True

    def has_libc(self, base_profile):
        return True

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @override
    def runtime_name_generator(self, base_profile: ProfileNameType) -> str:
        return self.legacy_runtime_name_generator(base_profile)

    @property
    def is_os_target(self) -> bool:
        return True

    @property
    def use_certifiable_packages(self):
        return True


class Aarch64QNX(QNX):
    def __init__(self):
        super(Aarch64QNX, self).__init__()

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def target(self):
        return "aarch64-qnx"

    @property
    def name(self):
        return "qnx"

    @property
    def cli_name(self) -> str:
        return "aarch64-qnx"

    @property
    def system_ads(self):
        return {
            "light": "system-qnx-arm-light.ads",
            "light-tasking": "system-qnx-arm-light-tasking.ads",
        }

    def amend_rts(self, rts_profile, cfg):
        cfg.build_flags["common_flags"] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            "-fno-omit-frame-pointer",
        ]
        cfg.build_flags["shared_linker_flags"] += [
            # Add an explicit dependency on libada_lch so that the last
            # chance handler is loaded when we use the Ada runtime.
            "-lada_lch",
        ]


class ARMQNX(QNX):
    def __init__(self):
        super(ARMQNX, self).__init__()

    @property
    def is_64bit(self) -> bool:
        return False

    @property
    def target(self):
        return "arm-qnx"

    @property
    def name(self):
        return "qnx"

    @property
    def cli_name(self) -> str:
        return "arm-qnx"

    @property
    def system_ads(self):
        return {
            "light": "system-qnx-arm-light.ads",
            "light-tasking": "system-qnx-arm-light-tasking.ads",
        }


TARGETS = [Aarch64QNX(), ARMQNX()]
