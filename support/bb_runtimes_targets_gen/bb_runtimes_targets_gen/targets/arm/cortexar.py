#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for Cortex-A/R
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Loader,
    Profile,
)
from rts_prebuilder.base_types import ValidBaseProfileType

from bb_runtimes_targets_gen.concrete_infrastructure import (
    SourcesAndFlagsComponent,
    DFBBTarget,
    light_profile,
    light_tasking_profile,
    embedded_profile,
)


class CortexARArch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "shared/i-arm_v7ar.ads",
            "shared/i-arm_v7ar.adb",
            "shared/i-cache.ads",
            "shared/i-cache__armv7.adb",
        )
        self.add_gnarl_sources("shared/s-bbcpsp__arm.ads", "shared/s-bbcppr__new.ads")

        if profile.base_profile in ("embedded", "light-tasking"):
            # s-bbcppr.adb uses the r7 register during context switching: this
            # is not compatible with the use of frame pointers that is emited
            # at -O0 by gcc. Let's disable fp even at -O0.
            self.add_runtime_switches(
                common_switches=("-fomit-frame-pointer",),
            )

            # The use of FPU registers (to speed up structure init or because
            # SIMD instructions are used) is incompatible with the libgnarl.
            # In fact, the libgnarl needs to take care of FPU registers context
            # switch and does so by doing lazy context switches: this restores
            # the registers only when they are used by apps. This means that if
            # a FPU register is used out of context, then we're doomed.
            self.add_runtime_switches(
                gnarl_switches=("-mgeneral-regs-only",),
            )


class CortexARTarget(DFBBTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(CortexARArch())

    @property
    @override
    def target(self) -> str:
        return "arm-eabi"

    @property
    @override
    def has_timer_64(self) -> bool:
        return True

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        # Exception propagation uses ARM unwind tables in .ARM.exidx and not the
        # DWARF .eh_frame.
        if base_profile == "embedded":
            return Path("arm/src/link-zcx.spec")
        return super().link_spec_file(base_profile)


class AM64xR5(CortexARTarget):
    @property
    @override
    def name(self) -> str:
        return "am64xr5"

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @property
    def cpu(self) -> str:
        return "cortex-r5"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/am64xr5/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_tasking_profile(
                self, Path("system-xi-arm-light-tasking-no-irq-nesting.ads")
            )
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-mlittle-endian",
                "-mfloat-abi=hard",
                f"-mcpu={self.cpu}",
                "-mfpu=vfpv3-d16",
                "-marm",
            ),
        )

        self.add_gnat_sources(
            "arm/am64xr5/crt0.S",
            "arm/src/s-mpudef.ads",
            "arm/src/s-mpuini.ads",
            "arm/src/s-mpuini.adb",
            "shared/s-boapar__am64xr5.ads",
            "shared/s-macres__none.adb",
            "shared/s-textio__16C750.adb",
        )

        self.add_gnarl_sources(
            "shared/a-intnam__am64xr5.ads",
            "shared/g-interr__ti_vim.ads",
            "shared/g-interr__ti_vim.adb",
            "shared/s-bbpara__am64xr5.ads",
            "shared/s-bbbosu__am64xr5.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbcppr__arm.adb",
            "shared/s-ti.ads",
            "shared/s-tvinma.ads",
            "shared/s-tvinma.adb",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("arm/am64xr5/ram.ld"),),
                included_scripts=(
                    Path("arm/am64xr5/common.ld"),
                    Path("arm/am64xr5/memmap.ld"),
                ),
            ),
        )


class Rpi2Base(CortexARTarget):

    @property
    def mcu(self) -> str:
        return "cortex-a7"

    @property
    def fpu(self) -> str:
        return "vfpv4"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/rpi2/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm.ads")),
            light_tasking_profile(self, Path("system-xi-arm-sfp.ads")),
            embedded_profile(self, Path("system-xi-arm-full.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-mlittle-endian",
                "-mfloat-abi=hard",
                f"-mcpu={self.mcu}",
                f"-mfpu={self.fpu}",
                "-marm",
                "-mno-unaligned-access",
            ),
        )

        self.add_gnat_sources("shared/i-raspberry_pi.ads", "shared/s-macres__rpi2.adb")
        self.add_gnarl_sources(
            "shared/a-intnam__rpi2.ads",
            "shared/s-bbbosu__rpi2.adb",
            "shared/s-bbcppr__arm.adb",
        )

        self.add_loaders(Loader("RAM", linker_scripts=(Path("arm/rpi2/ram.ld"),)))


class Rpi2(Rpi2Base):
    @property
    @override
    def name(self) -> str:
        return "rpi2"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "arm/rpi2/start-ram.S",
            "arm/rpi2/memmap.S",
            "shared/s-textio__rpi2-mini.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class Rpi2Mc(Rpi2Base):
    @property
    @override
    def name(self) -> str:
        return "rpi2mc"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "arm/rpi2-mc/start-ram.S",
            "arm/rpi2-mc/memmap.S",
            "shared/s-textio__rpi2-pl011.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class TMS570(CortexARTarget):
    @property
    @override
    def name(self) -> str:
        # The TMS570LS31 runtime originally used the Debug Communication
        # Channel (DCC) for Text_IO as it facilitated inhouse board testing.
        # For other TMS570 runtimes, we default to using SCI1 for Text_IO as
        # this is usable out of the box with the TI TMS570 developer kits
        # where DCC is not as easily available.

        if self.variant == "tms570ls31":
            base = "tms570"

            if self.uart_io:
                return f"{base}_sci"
            else:
                return base

        else:
            base = "tms570lc"

            if self.uart_io:
                return base
            else:
                return f"{base}_dcc"

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @property
    def cpu(self) -> str:
        if self.variant == "tms570ls31":
            return "cortex-r4f"
        else:
            return "cortex-r5"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/tms570/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm.ads")),
            light_tasking_profile(self, Path("system-xi-arm-sfp.ads")),
            embedded_profile(self, Path("system-xi-arm-full.ads")),
        ]

    def __init__(self, variant: str = "tms570ls31", uart_io: bool = False) -> None:
        self.variant = variant
        self.uart_io = uart_io
        super().__init__()

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-mbig-endian",
                "-mfloat-abi=hard",
                f"-mcpu={self.cpu}",
                "-mfpu=vfpv3-d16",
                "-marm",
                "-mbe32",
            ),
        )

        self.add_app_switches(
            link_switches=("-Wl,-z,max-page-size=0x1000",),
        )

        self.add_gnat_sources(
            "arm/tms570/crt0.S",
            f"arm/tms570/system_{self.variant}.c",
            "arm/tms570/s-tms570.ads",
            "arm/tms570/s-tms570.adb",
            "shared/s-macres__tms570.adb",
            f"shared/s-boapar__{self.variant}.ads",
        )
        if self.cpu == "cortex-r4f":
            self.add_gnat_source("arm/tms570/cortex-r4.S")
        if self.uart_io:
            self.add_gnat_source("shared/s-textio__tms570-sci.adb")
        else:
            self.add_gnat_source("shared/s-textio__tms570-dcc.adb")

        self.add_gnarl_sources(
            f"shared/a-intnam__{self.variant}.ads",
            f"shared/s-bbpara__{self.variant}.ads",
            "shared/s-bbbosu__tms570.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbcppr__arm.adb",
        )

        included = (
            Path("arm/tms570/common.ld"),
            Path(f"arm/tms570/tms570__{self.variant}.ld"),
        )
        self.add_loaders(
            *(
                Loader(
                    name,
                    linker_scripts=(Path(f"arm/tms570/{script}"),),
                    included_scripts=included,
                )
                for name, script in (
                    ("LORAM", "loram.ld"),
                    ("FLASH", "flash.ld"),
                    ("HIRAM", "hiram.ld"),
                )
            )
        )


class ZynqmpR5(CortexARTarget):
    @property
    @override
    def name(self) -> str:
        return "zynqmpr5"

    @property
    @override
    def has_small_memory(self) -> bool:
        return False

    @property
    def cpu(self) -> str:
        return "cortex-r5"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/zynqmpr5/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm.ads")),
            light_tasking_profile(self, Path("system-xi-arm-gic-sfp.ads")),
            embedded_profile(self, Path("system-xi-arm-gic-full.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-mlittle-endian",
                "-mfloat-abi=hard",
                f"-mcpu={self.cpu}",
                "-mfpu=vfpv3-d16",
                "-marm",
            ),
        )

        self.add_gnat_sources(
            "arm/zynqmpr5/crt0.S",
            "arm/src/s-mpudef.ads",
            "arm/src/s-mpuini.ads",
            "arm/src/s-mpuini.adb",
            "shared/s-boapar__zynqmpr5.ads",
        )
        self.add_gnat_sources(
            "shared/s-textio__zynqmp.adb", "shared/s-macres__zynqmp.adb"
        )

        self.add_gnarl_sources(
            "arm/armgic_irqtrap.s",
            "shared/a-intnam__zynqmp.ads",
            "shared/s-armgic__400.ads",
            "shared/s-armgic__400.adb",
            "shared/s-bbpara__zynqmpr5.ads",
            "shared/s-bbbosu__zynqmpr5.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbcppr__armgic.adb",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("arm/zynqmpr5/ram.ld"),),
                included_scripts=(
                    Path("arm/zynqmpr5/common.ld"),
                    Path("arm/zynqmpr5/memmap.ld"),
                ),
            ),
        )


class Zynq7000(CortexARTarget):
    @property
    @override
    def name(self) -> str:
        return "zynq7000"

    @property
    def mcu(self) -> str:
        return "cortex-a9"

    @property
    def fpu(self) -> str:
        return "vfpv3"

    @property
    @override
    def has_huge_memory(self) -> bool:
        return True

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("arm/zynq/README")

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-arm.ads")),
            light_tasking_profile(self, Path("system-xi-arm-gic-sfp.ads")),
            embedded_profile(self, Path("system-xi-arm-gic-full.ads")),
        ]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=(
                "-mlittle-endian",
                "-mfloat-abi=hard",
                f"-mcpu={self.mcu}",
                f"-mfpu={self.fpu}",
                "-marm",
                "-mno-unaligned-access",
            ),
        )
        self.add_gnat_sources(
            "arm/zynq/start-ram.S",
            "arm/zynq/memmap.inc",
            "shared/s-textio__zynq.adb",
            "shared/s-macres__zynq.adb",
        )
        self.add_gnarl_sources(
            "arm/armgic_irqtrap.s",
            "shared/a-intnam__zynq.ads",
            "shared/s-bbpara__cortexa9.ads",
            "shared/s-armgic__400.ads",
            "shared/s-armgic__400.adb",
            "shared/s-bbbosu__cortexa9.adb",
            "shared/s-bbcppr__armgic.adb",
        )

        self.add_loaders(Loader("RAM", linker_scripts=(Path("arm/zynq/ram.ld"),)))
