#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for PowerPC/e500v2
from abc import abstractmethod
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)
from rts_prebuilder.base_types import PlatformIdType
from rts_prebuilder.base_types import ValidBaseProfileType
from bb_runtimes_targets_gen.concrete_infrastructure import (
    RuntimeXmlSource,
    Target,
    light_profile,
    light_tasking_profile,
    embedded_profile,
)
from bb_runtimes_targets_gen.targets.pikeos.linker_script import add_linker_script


class PikeOSBuildFlags(SourcesAndFlagsComponent):
    """PikeOS runtime-build flags that depend on the profile."""

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if profile.base_profile == "embedded":
            # Register ZCX frames (for pikeos-cert-app.c)
            self.add_runtime_switches(
                c_switches=("-DUSE_ZCX",),
            )


class PikeOS(Target):
    @property
    @abstractmethod
    @override
    def platform_variant(self) -> str:
        """The PikeOS version, as the GNARL component's source table names it."""
        ...

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "pikeos"

    @property
    @abstractmethod
    def pikeos_target(self) -> str:
        """To be defined by all subclasses"""
        ...

    @property
    @override
    def has_command_line_arguments(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def is_os_target(self) -> bool:
        return True

    @override
    def runtime_name_generator(self, profile: Profile) -> str:
        return self.legacy_runtime_name_generator(profile)

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        # PikeOS is considered Bare Metal, and we don't provide newlib on
        # this target
        return False

    @property
    @override
    def supports_gc_sections(self) -> bool:
        # PikeOS 3's linker script can't cope with function/data sections.
        return self.platform_variant != "pikeos3"

    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PikeOSBuildFlags())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        add_linker_script(self, Path("pikeos/memory.ld"))
        self.add_gnat_sources(
            "shared/s-textio__pikeos.adb", "shared/s-macres__libc.adb"
        )
        self.add_gnarl_source("shared/a-intnam__dummy.ads")

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        version_tag = (
            "pikeos-3.4" if self.platform_variant == "pikeos3" else "pikeos-4.1"
        )
        return RuntimeXmlSource(
            Path("pikeos/runtime.xml"),
            {"@version@": version_tag, "@target@": self.pikeos_target},
        )


class ArmPikeOS(PikeOS):
    @property
    @override
    def name(self) -> str:
        return "arm-pikeos"

    @property
    @override
    def target(self) -> str:
        return "arm-sysgo-pikeos"

    @property
    @override
    def platform_variant(self) -> str:
        return "pikeos4"

    @property
    @override
    def pikeos_target(self) -> str:
        return "arm/v7hf"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-pikeos-arm.ads")),
            light_tasking_profile(
                self,
                Path("system-pikeos-arm-ravenscar-sfp.ads"),
            ),
            embedded_profile(
                self,
                Path("system-pikeos-arm-embedded.ads"),
            ),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("pikeos/pikeos-cert-app.c")
        self.add_gnarl_source("pikeos/adaint-pikeos.c")
        add_linker_script(self, Path("pikeos/arm-app.ld"))


class ArmPikeOS42(PikeOS):
    @property
    @override
    def name(self) -> str:
        return "arm-pikeos4.2"

    @property
    @override
    def target(self) -> str:
        return "arm-sysgo-pikeos4"

    @property
    @override
    def platform_variant(self) -> str:
        return "pikeos4.2"

    @property
    @override
    def pikeos_target(self) -> str:
        return "arm/v7hf"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-pikeos42-arm.ads")),
            light_tasking_profile(
                self,
                Path("system-pikeos42-arm-ravenscar-sfp.ads"),
            ),
            embedded_profile(
                self,
                Path("system-pikeos42-arm-embedded.ads"),
            ),
        ]

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("pikeos/runtime_p4ext.xml"))

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("pikeos/p4ext-cert-app.c")
        self.add_gnarl_source("pikeos/adaint-pikeos-p4ext.c")
        add_linker_script(self, Path("pikeos/arm-app.ld"))


class AArch64PikeOS5(PikeOS):
    @property
    @override
    def name(self) -> str:
        return "aarch64-pikeos5"

    @property
    @override
    def target(self) -> str:
        return "aarch64-sysgo-pikeos5"

    @property
    @override
    def platform_variant(self) -> str:
        return "pikeos5"

    @property
    @override
    def pikeos_target(self) -> str:
        return "arm/v8hf"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-pikeos5-aarch64.ads")),
            light_tasking_profile(
                self,
                Path("system-pikeos5-aarch64-ravenscar-sfp.ads"),
            ),
            embedded_profile(
                self,
                Path("system-pikeos5-aarch64-embedded.ads"),
            ),
        ]

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("pikeos/runtime_p4ext.xml"))

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("pikeos/p4ext-cert-app.c")
        self.add_gnarl_source("pikeos/adaint-pikeos-p4ext.c")
        add_linker_script(self, Path("pikeos/aarch64-pikeos5.ld"))


class ArmPikeOS5(PikeOS):
    @property
    @override
    def name(self) -> str:
        return "arm-pikeos5"

    @property
    @override
    def target(self) -> str:
        return "arm-sysgo-pikeos5"

    @property
    @override
    def platform_variant(self) -> str:
        return "pikeos5"

    @property
    @override
    def pikeos_target(self) -> str:
        return "arm/v7hf"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-pikeos5-arm.ads")),
            light_tasking_profile(
                self,
                Path("system-pikeos5-arm-ravenscar-sfp.ads"),
            ),
            embedded_profile(
                self,
                Path("system-pikeos5-arm-embedded.ads"),
            ),
        ]

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("pikeos/runtime_p4ext.xml"))

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("pikeos/p4ext-cert-app.c")
        self.add_gnarl_source("pikeos/adaint-pikeos-p4ext.c")
        add_linker_script(self, Path("pikeos/arm-pikeos5.ld"))


class PPCPikeOS5(PikeOS):
    @property
    @override
    def name(self) -> str:
        return "ppc-pikeos5"

    @property
    @override
    def target(self) -> str:
        return "powerpc-sysgo-pikeos5"

    @property
    @override
    def platform_variant(self) -> str:
        return "pikeos5"

    @property
    @override
    def pikeos_target(self) -> str:
        return "ppc/e500mc-4g"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-pikeos5-ppc.ads")),
            light_tasking_profile(
                self,
                Path("system-pikeos5-ppc-ravenscar-sfp.ads"),
            ),
            embedded_profile(
                self,
                Path("system-pikeos5-ppc-embedded.ads"),
            ),
        ]

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("pikeos/runtime_p4ext.xml"))

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("pikeos/p4ext-cert-app.c")
        self.add_gnarl_source("pikeos/adaint-pikeos-p4ext.c")
        add_linker_script(self, Path("pikeos/ppc-pikeos5.ld"))


TARGETS = [ArmPikeOS(), ArmPikeOS42(), AArch64PikeOS5(), ArmPikeOS5(), PPCPikeOS5()]
