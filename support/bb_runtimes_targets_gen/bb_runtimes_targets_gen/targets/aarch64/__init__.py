#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for ARM64
from pathlib import Path
from typing import override


from rts_prebuilder.end_user_data.compiler_selector import using_llvm_compiler
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import Profile
from bb_runtimes_targets_gen.concrete_infrastructure import (
    SourcesAndFlagsComponent,
    DFBBTarget,
    light_profile,
    light_tasking_profile,
    embedded_profile,
)


class Aarch64Arch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "shared/i-aarch64.ads",
            "shared/i-aarch64.adb",
            "shared/i-cache.ads",
            "shared/i-cache__aarch64.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbcpsp__aarch64.ads",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbcppr__aarch64.adb",
            "aarch64/context_switch.S",
        )
        _add_aarch64_gnarl_flags(self)


class MorelloArch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "shared/i-aarch64.ads",
            "shared/i-aarch64.adb",
            "shared/i-cache.ads",
            "shared/i-cache__aarch64.adb",
        )
        self.add_gnarl_sources(
            "shared/s-bbcpsp__morello.ads",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbcppr__morello.adb",
            "aarch64/morello/context_switch.S",
        )
        _add_aarch64_gnarl_flags(self)


def _add_aarch64_gnarl_flags(component: SourcesAndFlagsComponent) -> None:
    # The use of FPU registers (to speed up structure init or because SIMD
    # instructions are used) is incompatible with the libgnarl. In fact, the
    # libgnarl needs to take care of FPU registers context switch and does so by
    # doing lazy context switches: this restores the registers only when they
    # are used by apps. This means that if a FPU register is used out of
    # context, then we're doomed.
    component.add_runtime_switches(
        gnarl_switches=(
            (
                "-mgeneral-regs-only"
                if not using_llvm_compiler()
                else "-mno-implicit-float"
            ),
        ),
    )


class Aarch64Target(DFBBTarget):
    @property
    @override
    def has_fma(self) -> bool:
        return True

    @property
    @override
    def target(self) -> str:
        return "aarch64-elf"

    @property
    @override
    def has_huge_memory(self) -> bool:
        return True

    @property
    @override
    def has_timer_64(self) -> bool:
        return True

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm.ads")),
            light_tasking_profile(self, Path("system-xi-arm-sfp.ads")),
            embedded_profile(self, Path("system-xi-arm-full.ads")),
        ]


class MorelloTarget(Aarch64Target):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(MorelloArch())

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=morello+c64", "-mabi=purecap"),
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("aarch64/morello/ram.ld"),),
                included_scripts=(Path("aarch64/morello/common.ld"),),
            ),
        )

    @property
    @override
    def target(self) -> str:
        return "morello-elf"

    @property
    @override
    def uses_eh_frame_unwinding(self) -> bool:
        # Morello propagates exceptions via DWARF .eh_frame, so the embedded
        # link keeps the FDE lookup symbol (see the generator's embedded branch).
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm-nxstack-light.ads")),
            light_tasking_profile(
                self, Path("system-xi-arm-nxstack-light-tasking.ads")
            ),
            embedded_profile(self, Path("system-xi-arm-nxstack-embedded.ads")),
        ]


class ZynqMP(Aarch64Target):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(Aarch64Arch())

    @property
    @override
    def name(self) -> str:
        return "zynqmp"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("aarch64/zynqmp/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm-nxstack-light.ads")),
            light_tasking_profile(
                self, Path("system-xi-arm-nxstack-light-tasking.ads")
            ),
            embedded_profile(self, Path("system-xi-arm-nxstack-embedded.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mcpu=cortex-a53",))

        self.add_gnat_sources(
            "aarch64/zynqmp/start.S",
            "aarch64/zynqmp/trap_vector.S",
            "aarch64/zynqmp/memmap.S",
            "shared/trap_dump__aarch64.ads",
            "shared/trap_dump__aarch64.adb",
            "shared/s-textio__zynqmp.adb",
            "shared/s-macres__zynqmp.adb",
            "shared/s-mmu.ads",
            "shared/s-mmu__aarch64.adb",
        )
        self.add_gnarl_sources(
            "shared/a-intnam__zynqmp.ads",
            "shared/s-bbbosu__armv8a.adb",
            "shared/s-armgic__400.ads",
            "shared/s-armgic__400.adb",
            "shared/s-bbpara__zynqmp.ads",
        )

        common = Path("aarch64/zynqmp/common.ld")
        self.add_loaders(
            *(
                Loader(
                    name,
                    linker_scripts=(Path(f"aarch64/zynqmp/{script}"),),
                    included_scripts=(common,),
                )
                for name, script in (
                    ("RAM", "ram.ld"),
                    ("QSPI", "qspi.ld"),
                    ("HELIX", "helix.ld"),
                )
            )
        )


class Rpi3Base(Aarch64Target):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(Aarch64Arch())

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/rpi2/README")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mcpu=cortex-a53",))

        self.add_gnat_sources(
            "shared/i-raspberry_pi.ads",
            "shared/trap_dump__aarch64.ads",
            "shared/trap_dump__aarch64.adb",
            "shared/s-macres__rpi2.adb",
        )
        self.add_gnarl_sources("shared/a-intnam__rpi2.ads", "shared/s-bbbosu__rpi3.adb")

        self.add_loaders(Loader("RAM", linker_scripts=(Path("aarch64/rpi3/ram.ld"),)))


class Rpi3(Rpi3Base):
    @property
    @override
    def name(self) -> str:
        return "rpi3"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "aarch64/rpi3/start-ram.S",
            "aarch64/rpi3/memmap.S",
            "shared/s-textio__rpi2-mini.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class Rpi3Mc(Rpi3Base):
    @property
    @override
    def name(self) -> str:
        return "rpi3mc"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "aarch64/rpi3-mc/start-ram.S",
            "aarch64/rpi3-mc/traps_el3.S",
            "aarch64/rpi3-mc/traps_el2cur.S",
            "aarch64/rpi3-mc/traps_el2low.S",
            "aarch64/rpi3-mc/traps_common.h",
            "aarch64/rpi3-mc/memmap.S",
            "shared/s-textio__rpi2-pl011.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2-hyp.ads")


class Morello(MorelloTarget):
    uart_io: bool
    """ If true, use UART for IO instead of semihosting"""

    @property
    @override
    def name(self) -> str:
        if self.use_semihosting_io:
            return "morello-semihosting"
        else:
            return "morello"

    @property
    @override
    def use_semihosting_io(self) -> bool:
        return not self.uart_io

    @property
    @override
    def has_cheri(self) -> bool:
        return True

    def __init__(self, uart_io: bool) -> None:
        self.uart_io = uart_io

        super().__init__()

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "aarch64/morello/start.S",
            "aarch64/morello/memmap.S",
            "aarch64/morello/trap_vector.S",
            "aarch64/morello/reloc_symbols.S",
            "aarch64/morello/s-morell.ads",
            "aarch64/morello/s-morini.adb",
            "aarch64/morello/s-morini.ads",
            "shared/s-bbpara__morello.ads",
            "shared/trap_dump__aarch64.ads",
            "shared/trap_dump__aarch64.adb",
        )
        self.add_gnarl_sources(
            "shared/a-intnam__morello.ads",
            "shared/s-bbbosu__morello.adb",
            "shared/s-armgic__600.ads",
            "shared/s-armgic__600.adb",
        )

        if target.use_semihosting_io:
            self.add_gnat_sources(
                "shared/s-macres__semihosting.adb",
                "shared/s-sgshca__cortexar_c64.adb",
            )
        else:
            self.add_gnat_sources(
                "shared/s-macres__none.adb",
                "shared/s-textio__pl011.adb",
            )

        # Remove files needed to support C++ static constructors and
        # destructors because C++ is not supported for this target.
        self.add_app_switches(link_switches=("-nostartfiles",))


# Register the targets
TARGETS = (Morello(uart_io=False), Morello(uart_io=True), ZynqMP(), Rpi3(), Rpi3Mc())
