#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for Sparc/Leon
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import ValidBaseProfileType

from bb_runtimes_targets_gen.concrete_infrastructure import (
    SourcesAndFlagsComponent,
    DFBBTarget,
    light_profile,
    light_tasking_profile,
    embedded_profile,
)


class LeonArch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(link_switches=("-Wl,-u_start",))
        self.add_gnat_sources(
            "sparc/leon/crt0.S",
            "sparc/leon/hw_init.S",
            "sparc/src/sparc.h",
            "shared/s-macres__leon.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbcppr__old.ads",
            "shared/s-bbcppr__sparc.adb",
            "shared/s-bcpith__sparc.adb",
            "shared/s-bbcpsp__leon.ads",
            "sparc/src/context_switch.S",
            "sparc/src/trap_handler.S",
            "sparc/src/interrupt_masking.S",
            "shared/s-bbcaco.ads",
            "shared/s-bbcaco__leon.adb",
        )


class LeonTarget(DFBBTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(LeonArch())

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-sparc.ads")),
            light_tasking_profile(self, Path("system-xi-sparc-ravenscar.ads")),
            embedded_profile(self, Path("system-xi-sparc-full.ads")),
        ]

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        # Constructors and destructors are executed from the .ctors and .dtors
        # sections, so we need the crti.o and crtn.o objects.
        if base_profile in ("light", "light-tasking"):
            return Path("sparc/leon/link-noexceptions.spec")
        return super().link_spec_file(base_profile)


class Leon2(LeonTarget):
    @property
    @override
    def name(self) -> str:
        return "leon"

    @property
    @override
    def target(self) -> str:
        return "leon-elf"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(c_switches=("-DLEON", "-DLEON2"))

        self.add_gnat_sources("shared/s-textio__leon.adb", "shared/s-bbbopa__leon.ads")
        self.add_gnarl_sources(
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbsule__leon.ads",
            "shared/s-bbbosu__leon.adb",
            "shared/s-bbpara__leon.ads",
            "shared/a-intnam__leon.ads",
        )

        self.add_loaders(
            Loader("DEFAULT", linker_scripts=(Path("sparc/leon/leon.ld"),))
        )


class Leon3or4(LeonTarget):
    """
    Common class for leon3 and leon4 targets
    """

    def __init__(self, smp: bool) -> None:
        super().__init__()
        self.smp = smp

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        # see R409-022: -mcpu=leon3 makes gcc generate CASA instruction when
        # expanding compare_and_swap_4 intrinsic, which is invalid SPARCv8 insn
        # on most leon3.
        common: tuple[str, ...] = ("-mcpu=leon",) if not self.smp else ("-mcpu=leon3",)
        if self.need_fix_ut699:
            common += ("-mfix-ut699",)
        self.add_app_switches(common_switches=common)

        c: tuple[str, ...] = ("-DLEON", "-DLEON3")
        if self.need_fix_ut699:
            c += ("-DFIX_UT699",)
        self.add_app_switches(c_switches=c)

        self.add_gnat_sources(
            "shared/s-textio__leon3.adb",
            "shared/i-leon3.ads",
            "shared/i-leon3-uart.ads",
            "shared/i-leon3-cache.ads",
        )
        self.add_gnarl_sources(
            "shared/i-leon3-timers.ads",
            "shared/i-leon3-irqmp.ads",
            "shared/s-bbbosu__leon3.adb",
            "shared/s-bbpara__leon.ads",
            "shared/a-intnam__leon3.ads",
        )

        self.add_loaders(
            Loader("DEFAULT", linker_scripts=(Path("sparc/leon3/leon.ld"),))
        )

    @property
    @override
    def profiles(self) -> list[Profile]:
        # The Light runtime makes no sense in the context of the SMP variant.
        return [
            p for p in super().profiles if not (self.smp and p.base_profile == "light")
        ]

    @property
    def need_fix_ut699(self) -> bool:
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        # Single precision sqrt is buggy on UT699
        return not self.need_fix_ut699

    @property
    @override
    def has_compare_and_swap(self) -> bool:
        if not self.smp:
            # see R409-022
            return False
        else:
            return True

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("sparc/leon3/README")

    @property
    @override
    def target(self) -> str:
        return "leon3-elf"


class Leon3(Leon3or4):
    @property
    @override
    def name(self) -> str:
        if self.smp:
            return "leon3-smp"
        else:
            return "leon3"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source(
            f"shared/s-bbbopa__leon3-{'smp' if self.smp else 'up'}.ads"
        )


class Leon4(Leon3or4):
    @property
    @override
    def name(self) -> str:
        if self.smp:
            return "leon4-smp"
        else:
            return "leon4"

    @property
    @override
    def need_fix_ut699(self) -> bool:
        return False

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source(
            f"shared/s-bbbopa__leon4-{'smp' if self.smp else 'up'}.ads"
        )


TARGETS = [
    Leon2(),
    Leon3(smp=False),
    Leon3(smp=True),
    Leon4(smp=False),
    Leon4(smp=True),
]
