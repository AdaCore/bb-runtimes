#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from bb_runtimes_targets_gen.concrete_infrastructure import (
    DFBBTarget,
    embedded_profile,
    light_profile,
    light_tasking_profile,
)
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Profile


class RiscV64(DFBBTarget):
    @property
    @override
    def has_fma(self) -> bool:
        return self.has_double_precision_fpu

    @property
    @override
    def name(self) -> str:
        return "riscv64"

    @property
    @override
    def target(self) -> str:
        return "riscv64-elf"

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
        return [light_profile(self, Path("system-xi-riscv.ads"))]


class Spike(RiscV64):
    @property
    @override
    def name(self) -> str:
        return "spike"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(common_switches=("-mcmodel=medany",))
        self.add_gnat_sources(
            "riscv/start-ram.S",
            "riscv/src/riscv_host_target_interface.ads",
            "riscv/src/riscv_host_target_interface.adb",
            "shared/s-macres__riscv-htif.adb",
            "shared/s-textio__riscv-htif.adb",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("riscv/spike/common-RAM.ld"),),
                included_scripts=(Path("riscv/spike/memory-map.ld"),),
            ),
        )


class PolarFireSOC(RiscV64):
    """
    This class configures the runtime for the Polarfire SoC MSS
    (Microprocessor Subsystem).
    It is a 5-core RISC-V coherent CPU cluster consisting of one E51 monitor
    hart and four U54 application harts, based on the SiFive U54-MC IP.

    By default, only the U54 core 1 is used (mhartid 1), so the runtime is
    single-core only. If the `smp` flag is set to True, all the cores
    will be used by the runtime and will be usable for multitasking by the
    ADA application.
    """

    smp: bool
    """
    Flag to indicate if the runtime configured by this instance supports SMP.

    It defaults to False (even if the underlying hardware always support it)
    because the multi-core implementation is still not mature. It is intended
    to replace the single-core version in the long run.
    """

    @property
    @override
    def name(self) -> str:
        if self.smp:
            return "polarfiresoc-smp"
        return "polarfiresoc"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-riscv.ads")),
            light_tasking_profile(self, Path("system-xi-riscv-sifive-sfp.ads")),
            embedded_profile(self, Path("system-xi-riscv-sifive-full.ads")),
        ]

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    def __init__(self, smp: bool = False) -> None:
        super().__init__()
        self.smp = smp

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64gc", "-mabi=lp64d"),
        )

        if self.smp:
            # SMP-specific startup code
            self.add_gnat_source(
                "riscv/microchip/polarfiresoc/start-ram-smp.S",
                dest_name="start-ram.S",
            )
            # SMP-specific trap handler
            self.add_gnarl_source(
                "riscv/src/trap_handler_smp.S",
                dest_name="trap_handler.S",
            )

            # Multiprocessig specific sources
            self.add_gnarl_sources(
                "shared/s-bbsumu__riscv.adb",
                "shared/s-bbpara__polarfiresoc_smp.ads",
            )

        else:
            # Single-processor equivalent sources
            self.add_gnat_source("riscv/microchip/polarfiresoc/start-ram.S")
            self.add_gnarl_sources(
                "riscv/src/trap_handler.S",
                "shared/s-bbsumu__generic.adb",
                "shared/s-bbpara__polarfiresoc.ads",
            )

        # Common GNAT sources
        self.add_gnat_sources(
            "riscv/sifive/fe310/svd/i-fe310.ads",
            "riscv/sifive/fe310/svd/i-fe310-plic.ads",
            "riscv/sifive/fe310/s-macres.adb",
            "shared/s-textio__ns16550.adb",
            "shared/s-bbbopa__polarfiresoc.ads",
            "riscv/src/riscv_def.h",
        )
        self.add_gnarl_sources(
            "riscv/microchip/polarfiresoc/a-intnam.ads",
            "shared/s-bbbosu__riscv.adb",
            "shared/s-bbsuti__riscv_clint.adb",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbcppr__riscv.adb",
            "shared/s-bbcpsp__riscv.ads",
            "shared/s-bbcpsp__riscv.adb",
            "riscv/src/context_switch.S",
            "riscv/src/s-bbripl.ads",
            "riscv/microchip/polarfiresoc/s-bbripl.adb",
        )

        # The SMP script gives every core a stack. Both install as
        # common-RAM.ld, so the -T does not depend on the variant.
        script = Path(
            "riscv/microchip/polarfiresoc/common-RAM__smp.ld"
            if self.smp
            else "riscv/microchip/polarfiresoc/common-RAM.ld"
        )
        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(script,),
                included_scripts=(Path("riscv/microchip/polarfiresoc/memory-map.ld"),),
                # Keep __gnat_gdb_cpu_first_id undefined-but-referenced: GDB
                # reads it to learn the hardware id of the run-time's first
                # CPU, and -u stops the linker discarding the symbol.
                link_switches=('-u", "__gnat_gdb_cpu_first_id',),
            ),
        )


class RiscV32(DFBBTarget):
    @property
    @override
    def has_fma(self) -> bool:
        return self.has_double_precision_fpu

    @property
    @override
    def name(self) -> str:
        return "riscv32"

    @property
    @override
    def target(self) -> str:
        return "riscv32-elf"

    @property
    @override
    def has_timer_64(self) -> bool:
        return True


class HiFive1(RiscV32):
    @property
    @override
    def name(self) -> str:
        return "hifive1"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        # Only the Light runtime for this target, the tasking and embedded
        # run-times are not stable for the moment.
        return [light_profile(self, Path("system-xi-riscv.ads"))]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32imac", "-mabi=ilp32"),
        )
        self.add_gnat_sources(
            "riscv/sifive/fe310/start-rom.S",
            "riscv/sifive/fe310/svd/i-fe310.ads",
            "riscv/sifive/fe310/svd/i-fe310-uart.ads",
            "riscv/sifive/fe310/svd/i-fe310-gpio.ads",
            "riscv/sifive/fe310/svd/i-fe310-plic.ads",
            "riscv/sifive/fe310/s-macres.adb",
            "riscv/sifive/hifive1/s-textio.adb",
            "riscv/src/riscv_def.h",
        )
        self.add_gnarl_sources(
            "riscv/sifive/fe310/svd/a-intnam.ads",
            "shared/s-bbpara__riscv.ads",
            "shared/s-bbbopa__hifive1.ads",
            "shared/s-bbbosu__riscv.adb",
            "shared/s-bbsuti__riscv_clint.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbcppr__riscv.adb",
            "shared/s-bbcpsp__riscv.ads",
            "shared/s-bbcpsp__riscv.adb",
            "riscv/src/context_switch.S",
            "riscv/src/trap_handler.S",
            "riscv/src/s-bbripl.ads",
            "riscv/sifive/fe310/s-bbripl.adb",
        )

        self.add_loaders(
            Loader(
                "ROM",
                linker_scripts=(Path("riscv/sifive/hifive1/common-ROM.ld"),),
                included_scripts=(Path("riscv/sifive/hifive1/memory-map.ld"),),
            ),
        )


class MIV_RV32IMAF(RiscV32):
    @property
    @override
    def name(self) -> str:
        return "miv_rv32imaf"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-riscv-rv32imaf-light.ads")),
            light_tasking_profile(
                self, Path("system-xi-riscv-rv32imaf-light-tasking.ads")
            ),
            embedded_profile(self, Path("system-xi-riscv-rv32imaf-embedded.ads")),
        ]

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("riscv/microchip/miv_rv32imaf/README")

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32imaf_zicsr", "-mabi=ilp32f"),
        )
        self.add_gnat_sources(
            "riscv/microchip/miv_rv32imaf/start-ram.S",
            "riscv/sifive/fe310/svd/i-fe310.ads",
            "riscv/sifive/fe310/svd/i-fe310-plic.ads",
            "shared/s-bbbopa__miv.ads",
            "shared/s-macres__none.adb",
            "shared/s-textio__coreuartapb.adb",
            "shared/i-microsemi-coreuartapb.ads",
            "shared/i-microsemi.ads",
            "riscv/src/riscv_def.h",
        )
        self.add_gnarl_sources(
            "riscv/microchip/miv_rv32imaf/a-intnam.ads",
            "shared/s-bbpara__riscv.ads",
            "shared/s-bbbosu__riscv.adb",
            "shared/s-bbsuti__riscv_clint.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbcppr__riscv.adb",
            "shared/s-bbcpsp__riscv.ads",
            "shared/s-bbcpsp__riscv.adb",
            "riscv/src/context_switch.S",
            "riscv/src/trap_handler.S",
            "riscv/src/s-bbripl.ads",
            "riscv/microchip/miv_rv32imaf/s-bbripl.adb",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("riscv/microchip/miv_rv32imaf/common-RAM.ld"),),
                included_scripts=(Path("riscv/microchip/miv_rv32imaf/memory-map.ld"),),
            ),
        )


class MicroblazeV(RiscV64):
    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-xi-riscv.ads"))]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64ima_zicsr_zifencei_zicbom", "-mabi=lp64"),
        )

        self.add_gnat_sources(
            "shared/s-macres__none.adb",
            "shared/s-textio__axi_uartlite.adb",
            "riscv/amd/microblaze/start-ram.S",
            "riscv/src/riscv_def.h",
        )

        self.add_loaders(
            Loader(
                "RAM",
                linker_scripts=(Path("riscv/amd/microblaze/common-RAM.ld"),),
                included_scripts=(Path("riscv/amd/microblaze/memory-map.ld"),),
            ),
        )

    @property
    @override
    def name(self) -> str:
        return "microblazev"


class RV32BASE(RiscV32):
    """
    Generic Light run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        # Only the Light runtime for for this generic target
        return [light_profile(self, Path("system-xi-riscv.ads"))]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "riscv/sifive/fe310/s-macres.adb", "shared/s-textio__stdio.adb"
        )


class RV32I(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32i"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32i", "-mabi=ilp32"),
        )


class RV32IM(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32im"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32im", "-mabi=ilp32"),
        )


class RV32IAC(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32iac"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32iac", "-mabi=ilp32"),
        )


class RV32IMAC(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32imac"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32imac", "-mabi=ilp32"),
        )


class RV32IMAFC(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32imafc"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32imafc", "-mabi=ilp32f"),
        )

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True


class RV32IMAFDC(RV32BASE):
    @property
    @override
    def name(self) -> str:
        return "rv32imafdc"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv32imafdc", "-mabi=ilp32d"),
        )

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True


class RV64BASE(RiscV64):
    """
    Generic Light run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        # Only the Light runtime for for this generic target
        return [light_profile(self, Path("system-xi-riscv.ads"))]

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "riscv/sifive/fe310/s-macres.adb", "shared/s-textio__stdio.adb"
        )


class RV64IM(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64im"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64im", "-mabi=lp64"),
        )


class RV64IMC(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64imc"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64imc", "-mabi=lp64"),
        )


class RV64IMAC(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64imac"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64imac", "-mabi=lp64"),
        )


class RV64IMAFC(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64imafc"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64imafc", "-mabi=lp64f"),
        )


class RV64IMFC(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64imfc"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64imfc", "-mabi=lp64f"),
        )


class RV64IMAFDC(RV64BASE):
    @property
    @override
    def name(self) -> str:
        return "rv64imafdc"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            common_switches=("-march=rv64imafdc", "-mabi=lp64d"),
        )


TARGETS = [
    Spike(),
    HiFive1(),
    MIV_RV32IMAF(),
    PolarFireSOC(smp=False),
    PolarFireSOC(smp=True),
    MicroblazeV(),
    RV32I(),
    RV32IM(),
    RV32IAC(),
    RV32IMAC(),
    RV32IMAFC(),
    RV32IMAFDC(),
    RV64IM(),
    RV64IMC(),
    RV64IMAC(),
    RV64IMAFC(),
    RV64IMFC(),
    RV64IMAFDC(),
]
