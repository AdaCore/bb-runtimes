#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for PowerPC/e500v2
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import ValidBaseProfileType

from bb_runtimes_targets_gen.concrete_infrastructure import (
    SourcesAndFlagsComponent,
    DFBBTarget,
    embedded_profile,
    light_profile,
    light_tasking_profile,
)


class PPCArch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources("shared/s-bbcppr__new.ads", "shared/s-bbcppr__ppc.adb")
        # kill shrink-wrap-separate when building the runtime as this prevents
        # the frame to be properly built and thus prevents gdb from unwinding
        # the runtime (see R220-013).
        self.add_runtime_switches(
            common_switches=("-fno-shrink-wrap-separate",),
        )


class PPC6XXArch(SourcesAndFlagsComponent):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPCArch())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "powerpc/6xx/context_switch.S",
            "powerpc/6xx/handler.S",
            "shared/s-bbcpsp__6xx.ads",
            "shared/s-bbcpsp__6xx.adb",
        )


class PPCBookEArch(SourcesAndFlagsComponent):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPCArch())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "powerpc/booke/context_switch.S",
            "powerpc/booke/handler.S",
            "shared/s-bbcpsp__ppc-booke.ads",
            "shared/s-bbcpsp__ppc-booke.adb",
        )


class PPCSPEArch(SourcesAndFlagsComponent):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPCArch())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "powerpc/spe/context_switch.S",
            "powerpc/spe/handler.S",
            "shared/s-bbcpsp__spe.ads",
            "shared/s-bbcpsp__spe.adb",
        )


class PPC6XXTarget(DFBBTarget):
    @property
    @override
    def has_fma(self) -> bool:
        return True

    @property
    @override
    def target(self) -> str:
        return "powerpc-elf"

    @property
    @override
    def has_timer_64(self) -> bool:
        return True

    @property
    @override
    def has_fpu(self) -> bool:
        # Add fpu support
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        # But use soft float in the math lib
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        # But use soft float in the math lib
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-ppc.ads")),
            light_tasking_profile(self, Path("system-xi-ppc-sfp.ads")),
            embedded_profile(self, Path("system-xi-ppc-full.ads")),
        ]

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        # PowerPC uses ncrti.o and ncrtn.o which are EABI compliant.
        if base_profile == "embedded":
            return Path("powerpc/prep/link-zcx.spec")
        if base_profile in ("light", "light-tasking"):
            return Path("powerpc/prep/link-noexceptions.spec")
        return super().link_spec_file(base_profile)

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.use_certifiable_packages:
            self.add_gnat_sources(
                "powerpc/6xx/restfpr.S",
                "powerpc/6xx/restgpr.S",
                "powerpc/6xx/restxfpr.S",
                "powerpc/6xx/restxgpr.S",
                "powerpc/6xx/savefpr.S",
                "powerpc/6xx/savegpr.S",
            )


class MPC5200(PPC6XXTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPC6XXArch())

    @property
    @override
    def name(self) -> str:
        return "mpc5200"

    @property
    @override
    def use_certifiable_packages(self) -> bool:
        return True

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("powerpc/mpc5200/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-ppc.ads")),
            light_tasking_profile(self, Path("system-xi-ppc-mpc5200-sfp.ads")),
            embedded_profile(self, Path("system-xi-ppc-mpc5200-full.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mhard-float", "-mcpu=603e"))

        self.add_gnat_sources(
            "powerpc/mpc5200/start.S",
            "powerpc/mpc5200/setup.S",
            "shared/s-macres__mpc5200.adb",
            "shared/s-bbbopa__mpc5200.ads",
            "shared/s-textio__mpc5200.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbbosu__mpc5200.adb",
            "shared/s-bbsuti__ppc.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbpara__mpc5200.ads",
            "shared/a-intnam__mpc5200.ads",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("powerpc/mpc5200/ram.ld"),),
                link_switches=("-Wl,-u_start_ram",),
            ),
        )


class MPC8349e(PPC6XXTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPC6XXArch())

    @property
    @override
    def name(self) -> str:
        return "mpc8349e"

    @property
    @override
    def cli_name(self) -> str:
        return "8349e"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-msoft-float", "-mno-sdata"))

        self.add_app_switches(
            link_switches=("-Wl,-z,max-page-size=0x1000",),
        )
        self.add_gnat_sources(
            "powerpc/mpc8349/start-ram.S",
            "powerpc/mpc8349/setup.S",
            "shared/s-macres__8349e.adb",
            "shared/s-bbbopa__8349e.ads",
            "shared/s-textio__p2020.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbbosu__8349e.adb",
            "shared/s-bbpara__ppc.ads",
            "shared/s-bbsuti__ppc.adb",
            "shared/a-intnam__8349e.ads",
        )

        self.add_loaders(
            Loader("DEFAULT", linker_scripts=(Path("powerpc/mpc8349/ram.ld"),))
        )


class MPC8641(PPC6XXTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPC6XXArch())

    @property
    @override
    def name(self) -> str:
        return "mpc8641"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("powerpc/mpc8641/README")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mhard-float",))

        self.add_gnat_sources(
            "powerpc/mpc8641/start.S",
            "powerpc/mpc8641/setup.S",
            "shared/s-macres__p2020.adb",
            "shared/s-bbbopa__8641d.ads",
            "shared/s-textio__p2020.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbbosu__ppc-openpic.adb",
            "shared/s-bbsuti__ppc.adb",
            "shared/s-bbsumu__8641d.adb",
            "shared/s-bbpara__8641d.ads",
            "shared/a-intnam__ppc-openpic.ads",
        )

        self.add_loaders(
            Loader(
                "ROM",
                linker_scripts=(Path("powerpc/mpc8641/rom.ld"),),
                included_scripts=(Path("powerpc/mpc8641/common.ld"),),
            ),
            Loader(
                "RAM",
                linker_scripts=(Path("powerpc/mpc8641/ram.ld"),),
                included_scripts=(Path("powerpc/mpc8641/common.ld"),),
            ),
        )


class PPCBookETarget(DFBBTarget):
    @property
    @override
    def has_fma(self) -> bool:
        return True

    @property
    @override
    def target(self) -> str:
        return "powerpc-elf"

    @property
    @override
    def has_timer_64(self) -> bool:
        return True

    @property
    @override
    def has_fpu(self) -> bool:
        # Add fpu support
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        # But use soft float in the math lib
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        # But use soft float in the math lib
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-ppc.ads")),
            light_tasking_profile(self, Path("system-xi-ppc-sfp.ads")),
            embedded_profile(self, Path("system-xi-ppc-full.ads")),
        ]

    @property
    @override
    def uses_eh_frame_unwinding(self) -> bool:
        # PowerPC embedded propagates exceptions via DWARF .eh_frame. Under
        # gcc the link-zcx spec injects the FDE-lookup switches. Under llvm
        # there is no spec, so the generator adds them explicitly.
        return True

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        if base_profile == "embedded":
            return Path("powerpc/prep/link-zcx.spec")
        return super().link_spec_file(base_profile)

    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPCBookEArch())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.use_certifiable_packages:
            self.add_gnat_sources(
                "powerpc/6xx/restfpr.S",
                "powerpc/6xx/restgpr.S",
                "powerpc/6xx/restxfpr.S",
                "powerpc/6xx/restxgpr.S",
                "powerpc/6xx/savefpr.S",
                "powerpc/6xx/savegpr.S",
            )


class Virtex5(PPCBookETarget):
    @property
    @override
    def name(self) -> str:
        return "virtex5"

    @property
    @override
    def use_certifiable_packages(self) -> bool:
        return True

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("powerpc/virtex5/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-ppc.ads")),
            light_tasking_profile(self, Path("system-xi-ppc-xilinx-sfp.ads")),
            embedded_profile(self, Path("system-xi-ppc-xilinx-full.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-mhard-float", "-mcpu=440fp"),
        )

        self.add_gnat_sources(
            "powerpc/virtex5/start.S",
            "powerpc/virtex5/setup.S",
            "shared/s-bbbopa__virtex5.ads",
            "shared/s-macres__none.adb",
            "shared/s-textio__xps_uart16550.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbbosu__virtex5-xps.adb",
            "shared/s-bbsuti__ppc.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbpara__ppc-vertix5.ads",
            "shared/a-intnam__xps.ads",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("powerpc/virtex5/ram.ld"),),
                link_switches=("-Wl,-u_start_ram",),
            ),
        )


class PPCSPETarget(PPC6XXTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(PPCSPEArch())

    @property
    @override
    def target(self) -> str:
        return "powerpc-eabispe"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-e500v2.ads")),
            light_tasking_profile(self, Path("system-xi-e500v2-sfp.ads")),
            embedded_profile(self, Path("system-xi-e500v2-full.ads")),
        ]


class P5634(PPCSPETarget):
    @property
    @override
    def name(self) -> str:
        return "p5634"

    @property
    @override
    def cli_name(self) -> str:
        # Legacy CLI selector kept for backward compatibility (was "mpc5634").
        return "mpc5634"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mfloat-gprs=single",))
        self.add_gnat_sources(
            "powerpc/mpc5634/start.S",
            "shared/s-macres__p55.adb",
            "shared/s-textio__p55.adb",
        )

        self.add_loaders(
            Loader("DEFAULT", linker_scripts=(Path("powerpc/mpc5634/5634.ld"),))
        )


class P2020(PPCSPETarget):
    @property
    @override
    def name(self) -> str:
        return "p2020"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("powerpc/p2020/README")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mfloat-gprs=double",))
        self.add_gnat_sources(
            "powerpc/p2020/start-ram.S",
            "powerpc/p2020/setup.S",
            "shared/s-macres__p2020.adb",
            "shared/s-bbbopa__p2020.ads",
            "shared/s-textio__p2020.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbbosu__ppc-openpic.adb",
            "shared/s-bbsuti__ppc.adb",
            "shared/s-bbpara__ppc.ads",
            "shared/a-intnam__ppc-openpic.ads",
        )

        self.add_loaders(
            Loader("DEFAULT", linker_scripts=(Path("powerpc/p2020/p2020.ld"),))
        )


class P5566(PPCSPETarget):
    @property
    @override
    def name(self) -> str:
        return "p5566"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("powerpc/p5566/README")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mfloat-gprs=single",))
        self.add_gnat_sources(
            "powerpc/p5566/start-bam.S",
            "powerpc/p5566/start-ram.S",
            "powerpc/p5566/start-flash.S",
            "powerpc/p5566/setup.S",
            "powerpc/p5566/setup-pll.S",
            "shared/s-macres__p55.adb",
            "shared/s-textio__p55.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbbopa__p55.ads",
            "shared/s-bbsuti__ppc.adb",
            "shared/s-bbbosu__p55.adb",
            "shared/s-bbpara__p55.ads",
            "shared/a-intnam__p55.ads",
        )

        self.add_loaders(
            Loader("EXTRAM", linker_scripts=(Path("powerpc/p5566/ram.ld"),)),
            Loader("BAM", linker_scripts=(Path("powerpc/p5566/bam.ld"),)),
            Loader("FLASH", linker_scripts=(Path("powerpc/p5566/flash.ld"),)),
        )


TARGETS = [
    MPC5200(),
    MPC8349e(),
    MPC8641(),
    Virtex5(),
    P5634(),
    P2020(),
    P5566(),
]
