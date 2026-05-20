#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for Cortex-A/R
from pathlib import Path

from rts_prebuilder.engine import resolve_and_read_file
from bb_runtimes_targets_gen.concrete_infrastructure import ArchSupport, DFBBTarget


class CortexARArch(ArchSupport):
    @property
    def name(self):
        return "cortex-ar"

    def __init__(self):
        super(CortexARArch, self).__init__()
        self.add_gnat_sources(
            "shared/i-arm_v7ar.ads",
            "shared/i-arm_v7ar.adb",
            "shared/i-cache.ads",
            "shared/i-cache__armv7.adb",
        )
        self.add_gnarl_sources("shared/s-bbcpsp__arm.ads", "shared/s-bbcppr__new.ads")


class CortexARTarget(DFBBTarget):
    @property
    def target(self):
        return "arm-eabi"

    @property
    def parent(self):
        return CortexARArch()

    @property
    def has_timer_64(self) -> bool:
        return True

    def amend_rts(self, rts_profile, conf):
        super(CortexARTarget, self).amend_rts(rts_profile, conf)
        if "embedded" in rts_profile or "tasking" in rts_profile:
            # s-bbcppr.adb uses the r7 register during context switching: this
            # is not compatible with the use of frame pointers that is emited
            # at -O0 by gcc. Let's disable fp even at -O0.
            conf.build_flags["common_flags"] += ["-fomit-frame-pointer"]

            # The use of FPU registers (to speed up structure init or because
            # SIMD instructions are used) is incompatible with the libgnarl.
            # In fact, the libgnarl needs to take care of FPU registers context
            # switch and does so by doing lazy context switches: this restores
            # the registers only when they are used by apps. This means that if
            # a FPU register is used out of context, then we're doomed.
            conf.build_flags["common_gnarl_flags"] += ["-mgeneral-regs-only"]

        if "embedded" in rts_profile:
            # Exception propagation uses ARM unwind tables in .ARM.exidx and
            # not the DWARF .eh_frame.
            conf.config_files.update(
                {"link-zcx.spec": resolve_and_read_file(Path("arm/src/link-zcx.spec"))}
            )


class AM64xR5(CortexARTarget):
    @property
    def name(self):
        return "am64xr5"

    @property
    def has_small_memory(self) -> bool:
        return True

    @property
    def loaders(self):
        return ("RAM",)

    @property
    def cpu(self):
        return "cortex-r5"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.cpu}",
            "-mfpu=vfpv3-d16",
            "-marm",
        )

    @property
    def readme_file(self):
        return Path("arm/am64xr5/README")

    @property
    def system_ads(self):
        return {"light-tasking": "system-xi-arm-light-tasking-no-irq-nesting.ads"}

    def add_linker_scripts(self):
        self.add_linker_script("arm/am64xr5/common.ld")
        self.add_linker_script("arm/am64xr5/memmap.ld")
        self.add_linker_script("arm/am64xr5/ram.ld", loaders=("RAM",))

    def __init__(self):
        super(AM64xR5, self).__init__()

        self.add_linker_scripts()

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


class Rpi2Base(CortexARTarget):
    @property
    def loaders(self):
        return ("RAM",)

    @property
    def mcu(self):
        return "cortex-a7"

    @property
    def fpu(self):
        return "vfpv4"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.mcu}",
            f"-mfpu={self.fpu}",
            "-marm",
            "-mno-unaligned-access",
        )

    @property
    def readme_file(self):
        return Path("arm/rpi2/README")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-arm-sfp.ads",
            "embedded": "system-xi-arm-full.ads",
        }

    def __init__(self):
        super(Rpi2Base, self).__init__()

        self.add_linker_script("arm/rpi2/ram.ld", loaders=("RAM",))
        self.add_gnat_sources("shared/i-raspberry_pi.ads", "shared/s-macres__rpi2.adb")
        self.add_gnarl_sources(
            "shared/a-intnam__rpi2.ads",
            "shared/s-bbbosu__rpi2.adb",
            "shared/s-bbcppr__arm.adb",
        )


class Rpi2(Rpi2Base):
    @property
    def name(self):
        return "rpi2"

    def __init__(self):
        super(Rpi2, self).__init__()

        self.add_gnat_sources(
            "arm/rpi2/start-ram.S",
            "arm/rpi2/memmap.S",
            "shared/s-textio__rpi2-mini.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class Rpi2Mc(Rpi2Base):
    @property
    def name(self):
        return "rpi2mc"

    def __init__(self):
        super(Rpi2Mc, self).__init__()

        self.add_gnat_sources(
            "arm/rpi2-mc/start-ram.S",
            "arm/rpi2-mc/memmap.S",
            "shared/s-textio__rpi2-pl011.adb",
        )
        self.add_gnarl_source("shared/s-bbpara__rpi2.ads")


class TMS570(CortexARTarget):
    @property
    def name(self):
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
    def has_small_memory(self) -> bool:
        return True

    @property
    def loaders(self):
        return ("LORAM", "FLASH", "HIRAM")

    @property
    def cpu(self):
        if self.variant == "tms570ls31":
            return "cortex-r4f"
        else:
            return "cortex-r5"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mbig-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.cpu}",
            "-mfpu=vfpv3-d16",
            "-marm",
            "-mbe32",
        )

    @property
    def readme_file(self):
        return Path("arm/tms570/README")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-arm-sfp.ads",
            "embedded": "system-xi-arm-full.ads",
        }

    def add_linker_scripts(self):
        self.add_linker_script("arm/tms570/common.ld")
        self.add_linker_script(
            f"arm/tms570/{self.variant}.ld", dest_path_str="tms570.ld"
        )
        self.add_linker_script("arm/tms570/flash.ld", loaders=("FLASH",))
        self.add_linker_script("arm/tms570/hiram.ld", loaders=("HIRAM",))
        self.add_linker_script("arm/tms570/loram.ld", loaders=("LORAM",))
        self.add_linker_switch("-Wl,-z,max-page-size=0x1000", loader=None)

    def __init__(self, variant="tms570ls31", uart_io=False):
        self.variant = variant
        self.uart_io = uart_io
        super(TMS570, self).__init__()

        self.add_linker_scripts()

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


class ZynqmpR5(CortexARTarget):
    @property
    def name(self):
        return "zynqmpr5"

    @property
    def has_small_memory(self) -> bool:
        return False

    @property
    def loaders(self):
        return ("RAM",)

    @property
    def cpu(self):
        return "cortex-r5"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.cpu}",
            "-mfpu=vfpv3-d16",
            "-marm",
        )

    @property
    def readme_file(self):
        return Path("arm/zynqmpr5/README")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-arm-gic-sfp.ads",
            "embedded": "system-xi-arm-gic-full.ads",
        }

    def add_linker_scripts(self):
        self.add_linker_script("arm/zynqmpr5/common.ld")
        self.add_linker_script("arm/zynqmpr5/memmap.ld")
        self.add_linker_script("arm/zynqmpr5/ram.ld", loaders=("RAM",))

    def __init__(self):
        super(ZynqmpR5, self).__init__()

        self.add_linker_scripts()

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


class Zynq7000(CortexARTarget):
    @property
    def name(self):
        return "zynq7000"

    @property
    def loaders(self):
        return ("RAM",)

    @property
    def mcu(self):
        return "cortex-a9"

    @property
    def fpu(self):
        return "vfpv3"

    @property
    def has_huge_memory(self) -> bool:
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.mcu}",
            f"-mfpu={self.fpu}",
            "-marm",
            "-mno-unaligned-access",
        )

    @property
    def readme_file(self):
        return Path("arm/zynq/README")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-arm-gic-sfp.ads",
            "embedded": "system-xi-arm-gic-full.ads",
        }

    def __init__(self):
        super(Zynq7000, self).__init__()
        self.add_linker_script("arm/zynq/ram.ld", loaders=("RAM",))
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
