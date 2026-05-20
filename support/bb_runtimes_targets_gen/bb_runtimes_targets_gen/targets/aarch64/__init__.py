#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for ARM64
from pathlib import Path

from rts_prebuilder.end_user_data.compiler_selector import using_llvm_compiler
from bb_runtimes_targets_gen.concrete_infrastructure import ArchSupport, DFBBTarget


class Aarch64Arch(ArchSupport):
    @property
    def name(self):
        return "aarch64"

    def __init__(self) -> None:
        super(Aarch64Arch, self).__init__()
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


class MorelloArch(ArchSupport):
    @property
    def name(self):
        return "aarch64"

    def __init__(self):
        super(MorelloArch, self).__init__()
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


class Aarch64Target(DFBBTarget):
    @property
    def target(self) -> str:
        return "aarch64-elf"

    @property
    def parent(self) -> ArchSupport:
        return Aarch64Arch()

    @property
    def has_huge_memory(self) -> bool:
        return True

    @property
    def has_timer_64(self) -> bool:
        return True

    @property
    def is_64bit(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-arm-sfp.ads",
            "embedded": "system-xi-arm-full.ads",
        }

    def amend_rts(self, rts_profile, conf):
        super(Aarch64Target, self).amend_rts(rts_profile, conf)

        # The use of FPU registers (to speed up structure init or because
        # SIMD instructions are used) is incompatible with the libgnarl.
        # In fact, the libgnarl needs to take care of FPU registers context
        # switch and does so by doing lazy context switches: this restores
        # the registers only when they are used by apps. This means that if
        # a FPU register is used out of context, then we're doomed.
        conf.build_flags["common_gnarl_flags"] += [
            (
                "-mgeneral-regs-only"
                if not using_llvm_compiler()
                else "-mno-implicit-float"
            )
        ]


class MorelloTarget(Aarch64Target):
    @property
    def parent(self):
        return MorelloArch()

    @property
    def target(self) -> str:
        return "morello-elf"

    @property
    def compiler_switches(self):
        return ("-march=morello+c64", "-mabi=purecap")

    @property
    def system_ads(self):
        result = {
            "light": "system-xi-arm-nxstack-light.ads",
            "light-tasking": "system-xi-arm-nxstack-light-tasking.ads",
            "embedded": "system-xi-arm-nxstack-embedded.ads",
        }

        return result

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(MorelloTarget, self).dump_runtime_xml(rts_name, rts)
        if rts_name == "embedded":
            # Add options for exception propagation
            cnt = cnt.replace(
                '"-nolibc"',
                (
                    '"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
                    '         "-nolibc"'
                ),
            )
        return cnt


class ZynqMP(Aarch64Target):
    @property
    def name(self):
        return "zynqmp"

    @property
    def parent(self):
        return Aarch64Arch()

    @property
    def readme_file(self):
        return Path("aarch64/zynqmp/README")

    @property
    def loaders(self):
        return ("RAM", "QSPI", "HELIX")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm-nxstack-light.ads",
            "light-tasking": "system-xi-arm-nxstack-light-tasking.ads",
            "embedded": "system-xi-arm-nxstack-embedded.ads",
        }

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mcpu=cortex-a53",)

    def amend_rts(self, rts_profile, conf):
        super(ZynqMP, self).amend_rts(rts_profile, conf)

    def __init__(self) -> None:
        super(ZynqMP, self).__init__()

        self.add_linker_script("aarch64/zynqmp/common.ld")
        self.add_linker_script("aarch64/zynqmp/ram.ld", loaders=("RAM",))
        self.add_linker_script("aarch64/zynqmp/qspi.ld", loaders=("QSPI",))
        self.add_linker_script("aarch64/zynqmp/helix.ld", loaders=("HELIX",))
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


class Rpi3Base(Aarch64Target):
    @property
    def loaders(self):
        return ("RAM",)

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mcpu=cortex-a53",)

    @property
    def readme_file(self):
        return Path("arm/rpi2/README")

    def __init__(self) -> None:
        super(Rpi3Base, self).__init__()

        self.add_linker_script("aarch64/rpi3/ram.ld", loaders=("RAM",))
        self.add_gnat_sources(
            "shared/i-raspberry_pi.ads",
            "shared/trap_dump__aarch64.ads",
            "shared/trap_dump__aarch64.adb",
            "shared/s-macres__rpi2.adb",
        )
        self.add_gnarl_sources("shared/a-intnam__rpi2.ads", "shared/s-bbbosu__rpi3.adb")


class Rpi3(Rpi3Base):
    @property
    def name(self):
        return "rpi3"

    def __init__(self) -> None:
        super(Rpi3, self).__init__()

        self.add_gnat_sources(
            "aarch64/rpi3/start-ram.S",
            "aarch64/rpi3/memmap.S",
            "shared/s-textio__rpi2-mini.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class Rpi3Mc(Rpi3Base):
    @property
    def name(self):
        return "rpi3mc"

    def __init__(self) -> None:
        super(Rpi3Mc, self).__init__()

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
    def name(self):
        if self.use_semihosting_io:
            return "morello-semihosting"
        else:
            return "morello"

    @property
    def loaders(self):
        return ("RAM",)

    @property
    def use_semihosting_io(self):
        return not self.uart_io

    @property
    def has_cheri(self) -> bool:
        return True

    def __init__(self, uart_io: bool) -> None:
        self.uart_io = uart_io

        super(Morello, self).__init__()

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

        if self.use_semihosting_io:
            self.add_gnat_sources(
                "shared/s-macres__semihosting.adb",
                "shared/s-sgshca__cortexar_c64.adb",
            )
        else:
            self.add_gnat_sources(
                "shared/s-macres__none.adb",
                "shared/s-textio__pl011.adb",
            )

        self.add_linker_script("aarch64/morello/common.ld")
        self.add_linker_script("aarch64/morello/ram.ld", loaders=("RAM",))

        # Remove files needed to support C++ static constructors and
        # destructors because C++ is not supported for this target.
        self.add_linker_switch("-nostartfiles")


# Register the targets
TARGETS = (Morello(uart_io=False), Morello(uart_io=True), ZynqMP(), Rpi3(), Rpi3Mc())
