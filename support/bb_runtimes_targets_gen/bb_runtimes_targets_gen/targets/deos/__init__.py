#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.base_types import ProfileNameType, PlatformIdType
from rts_prebuilder.engine import resolve_and_read_file
from bb_runtimes_targets_gen.concrete_infrastructure import Target


class Deos(Target):
    def __init__(self):
        super(Deos, self).__init__()
        self.add_gnat_sources("shared/s-macres__deos.adb")

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "deos"

    @property
    def has_command_line_arguments(self) -> bool:
        return True

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    def has_small_memory(self) -> bool:
        return True

    def runtime_name_generator(self, profile: ProfileNameType) -> str:
        return self.legacy_runtime_name_generator(profile)

    @property
    def is_os_target(self) -> bool:
        return True

    def has_libc(self, base_profile):
        return True

    def dump_runtime_xml(self, rts_name, rts):
        return resolve_and_read_file(Path("deos/runtime.xml"))

    def amend_rts(self, rts_profile, conf):
        # conf.build_flags["common_flags"] += ["-DCERT", '-I" & DESK_HOME & "/include']
        # conf.build_flags["external_paths"] += ['Desk_Home := external ("DESKHOME");']
        # Temporary workaround while we work on a more cleaner implementation of
        # providing paths in target_options.py
        conf.build_flags["common_flags"] += [
            "-DCERT",
            "-I../../../../../../../../../src/deos-20220524-linux/desk/include",
        ]


class ArmDeos(Deos):
    def __init__(self):
        super(ArmDeos, self).__init__()

    @property
    def target(self):
        return "arm-elf"

    @property
    def name(self):
        return "arm-deos"

    @property
    def system_ads(self):
        return {"cert": "system-deos-arm-light.ads"}

    def amend_rts(self, rts_profile, conf):
        super(ArmDeos, self).amend_rts(rts_profile, conf)
        conf.build_flags["common_flags"] += [
            "-mabi=aapcs-linux",
            "-march=armv7-a",
            "-mfloat-abi=hard",
            "-mfpu=vfpv3",
            "-mthumb",
            "-mthumb-interwork",
            "-mno-unaligned-access",
            "-mrestrict-it",
            "-fPIC",
        ]


TARGETS: list[AbstractTarget] = [ArmDeos()]
