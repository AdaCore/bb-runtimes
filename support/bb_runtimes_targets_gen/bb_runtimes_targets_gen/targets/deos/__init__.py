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
    RuntimeXmlSource,
    Target,
    cert_profile,
)


class Deos(Target):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__deos.adb")
        # Temporary workaround while we work on a cleaner way of providing
        # include paths through target_options.
        self.add_runtime_switches(
            common_switches=(
                "-DCERT",
                "-I../../../../../../../../../src/deos-20220524-linux/desk/include",
            ),
        )

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "deos"

    @property
    @override
    def has_command_line_arguments(self) -> bool:
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
    def has_small_memory(self) -> bool:
        return True

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("deos/runtime.xml"))


class ArmDeos(Deos):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_runtime_switches(
            common_switches=(
                "-mabi=aapcs-linux",
                "-march=armv7-a",
                "-mfloat-abi=hard",
                "-mfpu=vfpv3",
                "-mthumb",
                "-mthumb-interwork",
                "-mno-unaligned-access",
                "-mrestrict-it",
                "-fPIC",
            ),
        )

    @property
    @override
    def target(self) -> str:
        return "arm-elf"

    @property
    @override
    def name(self) -> str:
        return "arm-deos"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [cert_profile(self, Path("system-deos-arm-light.ads"))]


TARGETS: list[AbstractTarget] = [ArmDeos()]
