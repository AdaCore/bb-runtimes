#
# Copyright (C) 2025-2026, AdaCore
#

# This module contains cortex-m bsp support
import re
from pathlib import Path
from typing import override


from rts_prebuilder.engine import resolve_and_read_file
from bb_runtimes_targets_gen.concrete_infrastructure import ArchSupport, Target


class CortexMArch(ArchSupport):
    @property
    def name(self):
        return "cortex-m"

    def __init__(self):
        super(CortexMArch, self).__init__()
        self.add_gnat_sources(
            "shared/s-macres__cortexm3.adb", "arm/src/breakpoint_handler-cortexm.S"
        )
        self.add_gnarl_sources(
            "shared/s-bbcpsp__cortexm.ads",
            "shared/s-bbcppr__old.ads",
            "shared/s-bbcppr__armv7m.adb",
            "shared/s-bbsumu__generic.adb",
            "shared/s-bcpcst__armvXm.ads",
        )


class ArmV7MArch(ArchSupport):
    @property
    def name(self):
        return "armv7-m"

    @property
    def parent(self):
        return CortexMArch()

    def __init__(self):
        super(ArmV7MArch, self).__init__()
        self.add_gnarl_sources(
            "shared/s-bbbosu__armv7m.adb", "shared/s-bcpcst__pendsv.adb"
        )


class ArmV6MTarget(Target):
    @property
    def target(self):
        return "arm-eabi"

    @property
    def parent(self):
        return CortexMArch()

    @property
    @override
    def platform(self):
        return "bb"

    @property
    def has_timer_64(self) -> bool:
        return False

    @property
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    def has_small_memory(self) -> bool:
        return True

    def __init__(self):
        super(ArmV6MTarget, self).__init__()

        self.add_gnat_sources("shared/s-bbarat.ads", "shared/s-bbarat.adb")

        if self.use_semihosting_io:
            self.add_gnat_sources("shared/s-sgshca__cortexm.adb")

    def amend_rts(self, rts_profile, conf):
        super(ArmV6MTarget, self).amend_rts(rts_profile, conf)
        # Exception propagation uses ARM unwind tables in .ARM.exidx and
        # not the DWARF .eh_frame.
        if "embedded" in rts_profile:
            conf.config_files.update(
                {"link-zcx.spec": resolve_and_read_file(Path("arm/src/link-zcx.spec"))}
            )


class ArmV7MTarget(ArmV6MTarget):
    @property
    def parent(self):
        return ArmV7MArch()

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-cortexm4-sfp.ads",
            "embedded": "system-xi-cortexm4-full.ads",
        }

    def __init__(self):
        super(ArmV7MTarget, self).__init__()


class LM3S(ArmV7MTarget):
    @property
    def name(self):
        return "lm3s"

    @property
    def loaders(self):
        return ("ROM", "RAM")

    @property
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    def has_fpu(self) -> bool:
        # Still add floating point attributes
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m3")

    @property
    def readme_file(self):
        return Path("arm/lm3s/README")

    @property
    def system_ads(self):
        # Only the Light runtime is supported here
        ret = super(LM3S, self).system_ads
        return {"light": ret["light"]}

    def __init__(self):
        super(LM3S, self).__init__()

        self.add_linker_script("arm/lm3s/lm3s-rom.ld", loaders=("ROM",))
        self.add_linker_script("arm/lm3s/lm3s-ram.ld", loaders=("RAM",))
        self.add_gnat_sources(
            "arm/lm3s/start-rom.S",
            "arm/lm3s/start-ram.S",
            "arm/lm3s/setup_pll.adb",
            "arm/lm3s/setup_pll.ads",
            "shared/s-textio__lm3s.adb",
        )


class SamCommonArchSupport(ArchSupport):
    @property
    def name(self):
        return "sam"

    @property
    def parent(self):
        return ArmV7MArch()

    @property
    def loaders(self):
        return ("ROM", "SAMBA", "RAM")

    def __init__(self):
        super(SamCommonArchSupport, self).__init__()

        self.add_linker_script("arm/sam/common-SAMBA.ld", loaders=("SAMBA",))
        self.add_linker_script("arm/sam/common-ROM.ld", loaders=("ROM",))
        self.add_linker_script("arm/sam/common-RAM.ld", loaders=("RAM",))

        self.add_gnat_sources(
            "arm/sam/start-rom.S",
            "arm/sam/start-ram.S",
            "arm/sam/start-common.S",
            "arm/sam/setup_pll.ads",
        )


class Sam(ArmV7MTarget):
    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return SamCommonArchSupport()

    @property
    def use_semihosting_io(self):
        return True

    @property
    def has_single_precision_fpu(self) -> bool:
        if self.board == "sam4s":
            return False
        else:
            return True

    @property
    def has_double_precision_fpu(self) -> bool:
        if self.board in ("samv71", "samrh71"):
            return True
        else:
            return False

    @property
    def cortex(self):
        if self.board in ("samv71", "samrh71"):
            return "cortex-m7"
        else:
            return "cortex-m4"

    @property
    def fpu(self):
        if self.cortex == "cortex-m4":
            return "fpv4-sp-d16"
        elif not self.has_double_precision_fpu:
            return "fpv5-sp-d16"
        else:
            return "fpv5-d16"

    @property
    def system_ads(self):
        ret = super(Sam, self).system_ads

        if self.board in ("samv71", "samrh71"):
            return ret
        else:
            # No Embedded runtime
            return {"light": ret["light"], "light-tasking": ret["light-tasking"]}

    @property
    def compiler_switches(self):
        base = ("-mlittle-endian", "-mthumb", f"-mcpu={self.cortex}")

        if not self.has_single_precision_fpu:
            return base
        else:
            return base + (
                "-mfloat-abi=hard",
                f"-mfpu={self.fpu}",
            )

    def __init__(self, board):
        if board not in (
            "sam4s",
            "samg55",
            "samv71",
            "samrh71",
        ):
            raise ValueError(f"Unexpected SAM board {board}")

        self.board = board
        super(Sam, self).__init__()

        self.add_linker_script(f"arm/sam/{self.name}/memory-map.ld")

        self.add_gnat_sources(
            f"arm/sam/{self.name}/setup_pll.adb",
            f"arm/sam/{self.name}/s-bbbopa.ads",
            f"arm/sam/{self.name}/svd/i-sam.ads",
            f"arm/sam/{self.name}/svd/i-sam-pmc.ads",
        )

        # tasking support
        self.add_gnarl_sources(
            f"arm/sam/{self.name}/svd/handler.S",
            f"arm/sam/{self.name}/s-bbmcpa.ads",
            f"arm/sam/{self.name}/svd/a-intnam.ads",
        )

        if self.board == "samv71":
            self.add_gnat_sources(
                "arm/sam/samv71/s-samv71.ads",
                f"arm/sam/{self.name}/svd/i-sam-pio.ads",
                f"arm/sam/{self.name}/svd/i-sam-uart.ads",
                f"arm/sam/{self.name}/svd/i-sam-efc.ads",
            )
            self.add_gnarl_sources("shared/s-bbpara__samv71.ads")
        elif self.board == "samrh71":
            self.add_gnarl_sources("shared/s-bbpara__samv71.ads")
        else:
            self.add_gnat_sources(
                "arm/sam/sam4s/s-sam4s.ads",
                f"arm/sam/{self.name}/board_config.ads",
                f"arm/sam/{self.name}/svd/i-sam-sysc.ads",
                f"arm/sam/{self.name}/svd/i-sam-efc.ads",
            )
            self.add_gnarl_sources("shared/s-bbpara__sam4s.ads")


class SmartFusion2(ArmV7MTarget):
    @property
    def name(self):
        return "smartfusion2"

    @property
    def loaders(self):
        return ("ROM",)

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m3")

    @property
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    def has_fpu(self) -> bool:
        # Still add floating point attributes
        return True

    @property
    def system_ads(self):
        # No Light or Embedded runtime
        ret = super(SmartFusion2, self).system_ads
        return {"light-tasking": ret["light-tasking"]}

    def __init__(self):
        super(SmartFusion2, self).__init__()

        self.add_linker_script("arm/smartfusion2/common-ROM.ld", loaders=("ROM",))
        self.add_linker_script("arm/smartfusion2/memory-map.ld", loaders=("ROM",))

        self.add_gnat_sources(
            "arm/smartfusion2/start-rom.S",
            "arm/smartfusion2/setup_pll.adb",
            "arm/smartfusion2/setup_pll.ads",
            "arm/smartfusion2/s-sf2.ads",
            "arm/smartfusion2/s-sf2.adb",
            "arm/smartfusion2/s-sf2uar.ads",
            "arm/smartfusion2/s-sf2uar.adb",
            "arm/smartfusion2/svd/i-sf2.ads",
            "arm/smartfusion2/svd/i-sf2-system_registers.ads",
            "arm/smartfusion2/svd/i-sf2-mmuart.ads",
            "arm/smartfusion2/svd/i-sf2-gpio.ads",
            "arm/smartfusion2/s-textio.adb",
        )
        self.add_gnarl_sources(
            "arm/smartfusion2/s-sf2gpi.ads",
            "arm/smartfusion2/s-sf2gpi.adb",
            "arm/smartfusion2/svd/handler.S",
            "arm/smartfusion2/svd/a-intnam.ads",
            "shared/s-bbpara__smartfusion2.ads",
        )


class CortexM0CommonArchSupport(ArmV6MTarget):
    @property
    def name(self):
        return "cortex-m0"

    @property
    def loaders(self):
        return ("ROM", "RAM")

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-msoft-float", "-mcpu=cortex-m0")

    @property
    def has_fpu(self) -> bool:
        # We require floating point attributes as soft-float is supported
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-armv6m-sfp.ads",
            "embedded": "system-xi-armv6m-full.ads",
        }

    def __init__(self):
        super(CortexM0CommonArchSupport, self).__init__()

        self.add_gnat_sources("shared/s-bbpara__cortexm0.ads")

        self.add_gnarl_sources(
            "shared/s-bbbosu__armv6m.adb", "shared/s-bcpcst__pendsv.adb"
        )


class Stm32F0(CortexM0CommonArchSupport):
    # Flash memory size is determined by the "User code memory size"
    # part of the device name
    flash_sizes = {"4": 16, "6": 32, "8": 64, "b": 128, "c": 256}

    # RAM size for STM32F03x is determined by the
    # device package (pin count) and user code memory size
    # (i.e. the last two characters of the device name, e.g. stm32f030f4)
    f03x_ram_sizes = {  # STM32F03xxx
        "c4": 4,
        "c6": 4,
        "c8": 8,
        "cc": 32,
        "e6": 4,
        "f4": 4,
        "f6": 4,
        "g4": 4,
        "g6": 4,
        "k4": 4,
        "k6": 4,
        "r8": 8,
        "rc": 32,
    }

    # RAM size for STM32F07x is also determined by the
    # device package (pin count) and user code memory size.
    f07x_ram_sizes = {
        "c6": 6,
        "c8": 16,
        "cb": 16,
        "f6": 6,
        "rb": 16,
        "v8": 16,
        "vb": 16,
    }

    # Board parameters (for s-bbmcpa.ads) when HSE is used.
    # For now these are hard-coded. These could be calculated
    # from a configurable HSE clock frequency in a future
    # improvement.
    board_parameters_hse = {
        "STM32_Main_Clock_Frequency": "48_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_HSE_Bypass": "False",
        "STM32_LSI_Enabled": "True",
        "STM32_PLL_Src": "System.STM32.PLL_SRC_HSE_PREDIV",
        "STM32_SYSCLK_Src": "System.STM32.SYSCLK_SRC_PLL",
        "STM32_PREDIV": "1",
        "STM32_PLLMUL_Value": "6",
        "STM32_AHB_PRE": "System.STM32.AHBPRE_DIV1",
        "STM32_APB_PRE": "System.STM32.APBPRE_DIV1",
    }

    # Board parameters (for s-bbmcpa.ads) when HSI is used.
    # These parameters are suitable for the clock tree in
    # F04x, F07x, and F09x devices.
    board_parameters_hsi = {
        "STM32_Main_Clock_Frequency": "48_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_HSE_Bypass": "False",
        "STM32_LSI_Enabled": "True",
        "STM32_PLL_Src": "System.STM32.PLL_SRC_HSI_PREDIV",
        "STM32_SYSCLK_Src": "System.STM32.SYSCLK_SRC_PLL",
        "STM32_PREDIV": "1",
        "STM32_PLLMUL_Value": "6",
        "STM32_AHB_PRE": "System.STM32.AHBPRE_DIV1",
        "STM32_APB_PRE": "System.STM32.APBPRE_DIV1",
    }

    # Board parameters (for s-bbmcpa.ads) when HSI is used.
    # These parameters are suitable for the clock tree in
    # F03x and F05x devices (fixed /2 divider on HSI).
    board_parameters_hsi2 = {
        "STM32_Main_Clock_Frequency": "48_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_HSE_Bypass": "False",
        "STM32_LSI_Enabled": "True",
        "STM32_PLL_Src": "System.STM32.PLL_SRC_HSI_2",
        "STM32_SYSCLK_Src": "System.STM32.SYSCLK_SRC_PLL",
        "STM32_PREDIV": "1",
        "STM32_PLLMUL_Value": "12",
        "STM32_AHB_PRE": "System.STM32.AHBPRE_DIV1",
        "STM32_APB_PRE": "System.STM32.APBPRE_DIV1",
    }

    @property
    def name(self):
        return self.board

    @property
    def use_semihosting_io(self):
        return True

    @property
    def loaders(self):
        return ("ROM", "RAM")

    def __init__(self, board):
        super(Stm32F0, self).__init__()

        # Determine MCU features from board name (e.g. 'stm32f071rb-hse')
        # The -hse or -hsi suffix specifies which clock source to
        # use for the runtime (either HSE or HSI)
        m = re.match(r".*f0([34579])([0128])([cefgkrv])([468bc])-(hsi|hse)", board)
        if m is None:
            raise RuntimeError("Unknown STM32F0 target: " + board)
        sub_family_major = m.group(1)
        sub_family_minor = m.group(2)
        package = m.group(3)
        user_code_memory_size = m.group(4)
        clock_source = m.group(5)

        self.board = board

        # Determine RAM size from sub-family, package, and user code mem. size
        if sub_family_major == "3":
            ram_size = self.f03x_ram_sizes[package + user_code_memory_size]
        elif sub_family_major in "45":
            ram_size = 4  # All STM32F04x/STM32F05x devices have 4K RAM
        elif sub_family_major == "7":
            ram_size = self.f07x_ram_sizes[package + user_code_memory_size]
        else:
            ram_size = 32  # All STM32F09x devices have 32k RAM

        flash_size = self.flash_sizes[user_code_memory_size]

        self.add_linker_script("arm/stm32/stm32f0xx/common-RAM.ld", loaders=("RAM",))
        self.add_linker_script("arm/stm32/stm32f0xx/common-ROM.ld", loaders=("ROM",))

        # Select memory map based on available memory size
        # of the specific device
        self.add_linker_script("arm/stm32/stm32f0xx/memory-map.ld.tmpl")

        # Set template variables required by linker script
        self.add_template_config_value("STM32_Linker_Flash_Size", f"{flash_size}K")
        self.add_template_config_value("STM32_Linker_RAM_Size", f"{ram_size}K")

        # Common source files
        self.add_gnat_sources(
            "arm/stm32/stm32f0xx/s-stm32.ads",
            "arm/stm32/stm32f0xx/s-stm32.adb",
            "arm/stm32/stm32f0xx/start-rom.S",
            "arm/stm32/stm32f0xx/start-ram.S",
            "arm/stm32/stm32f0xx/setup_pll.ads",
            "arm/stm32/stm32f0xx/setup_pll.adb",
            "arm/stm32/stm32f0xx/s-bbmcpa.ads.tmpl",
            "arm/stm32/stm32f0xx/s-bbbopa.ads.tmpl",
            f"arm/stm32/stm32f0xx/stm32f0x{sub_family_minor}/svd/i-stm32.ads",
            f"arm/stm32/stm32f0xx/stm32f0x{sub_family_minor}/svd/i-stm32-flash.ads",
            f"arm/stm32/stm32f0xx/stm32f0x{sub_family_minor}/svd/i-stm32-rcc.ads",
        )

        # Configure MCU parameters based on family.
        if sub_family_major in "479":
            self.add_template_config_value("STM32_Simple_Clock_Tree", "False")
        else:
            self.add_template_config_value("STM32_Simple_Clock_Tree", "True")

        # Configure board parameters based on chosen clock source (HSE or HSI)
        # and the device family.
        if clock_source == "hse":
            for key, value in self.board_parameters_hse.items():
                self.add_template_config_value(key, value)

        elif sub_family_major in "479":
            # STM32F04x/STM32F07x/STM32F09x can use HSI directly
            # as PLL input.
            for key, value in self.board_parameters_hsi.items():
                self.add_template_config_value(key, value)

        else:
            # STM32F03x/STM32F05x are forced to HSI/2 as PLL input.
            for key, value in self.board_parameters_hsi2.items():
                self.add_template_config_value(key, value)

        # Choose interrupt names based on family
        self.add_gnarl_sources(
            f"arm/stm32/stm32f0xx/stm32f0x{sub_family_minor}/svd/a-intnam.ads"
        )


class CortexM1CommonArchSupport(ArmV6MTarget):
    @property
    def name(self):
        return "microsemim1"

    @property
    def loaders(self):
        return (
            "TCM",
            "RAM",
        )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m1")

    @property
    def has_fpu(self) -> bool:
        # We require floating point attributes as soft-float is supported
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-armv6m-sfp.ads",
            "embedded": "system-xi-armv6m-full.ads",
        }

    def __init__(self):
        super(CortexM1CommonArchSupport, self).__init__()

        self.add_linker_script("arm/cortex-m1/common-RAM.ld", loaders=("RAM",))
        self.add_linker_script("arm/cortex-m1/common-TCM.ld", loaders=("TCM",))

        self.add_gnat_sources("arm/cortex-m1/start-tcm.S", "arm/cortex-m1/start-ram.S")

        self.add_gnarl_sources(
            "shared/s-bbpara__cortexm1.ads",
            "shared/s-bbbosu__armv6m.adb",
            "shared/s-bcpcst__pendsv.adb",
        )


class MicrosemiM1(CortexM1CommonArchSupport):
    @property
    def name(self):
        return "microsemi-m1"

    def __init__(self):
        super(MicrosemiM1, self).__init__()

        self.add_linker_script("arm/cortex-m1/microsemi/memory-map.ld")

        self.add_gnat_sources(
            "arm/cortex-m1/microsemi/s-bbbopa.ads",
            "arm/cortex-m1/microsemi/s-bbmcpa.ads",
            "shared/s-textio__coreuartapb.adb",
            "shared/i-microsemi-coreuartapb.ads",
            "shared/i-microsemi.ads",
        )

        self.add_gnarl_sources("arm/cortex-m1/microsemi/a-intnam.ads")


class NRF51(ArmV6MTarget):
    @property
    def name(self):
        return "nRF51"

    @property
    def loaders(self):
        return ("ROM",)

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m0")

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-armv6m-sfp.ads",
            "embedded": "system-xi-armv6m-full.ads",
        }

    def __init__(self):
        super(NRF51, self).__init__()

        self.add_linker_script("arm/nordic/nrf51/common-ROM.ld", loaders=("ROM",))

        self.add_gnat_sources(
            "arm/nordic/nrf51/svd/i-nrf51.ads",
            "arm/nordic/nrf51/svd/i-nrf51-clock.ads",
            "arm/nordic/nrf51/svd/i-nrf51-rtc.ads",
            "arm/nordic/nrf51/svd/i-nrf51-uart.ads",
            "arm/nordic/nrf51/svd/handler.S",
            "arm/nordic/nrf51/start-rom.S",
            "arm/nordic/nrf51/s-bbmcpa.ads",
        )

        self.add_gnarl_sources(
            "arm/nordic/nrf51/svd/a-intnam.ads",
            "shared/s-bbpara__nrf51.ads",
            "shared/s-bbbosu__nrf51.adb",
            "shared/s-bcpcst__pendsv.adb",
        )


class Microbit(NRF51):
    @property
    def name(self):
        return "microbit"

    @property
    def use_semihosting_io(self):
        return False

    def __init__(self):
        super(Microbit, self).__init__()

        self.add_linker_script(
            "arm/nordic/nrf51/memory-map_nRF51822xxAA.ld", dest_path_str="memory-map.ld"
        )

        self.add_gnat_sources(
            "arm/nordic/nrf51/s-bbbopa__microbit.ads", "shared/s-textio__microbit.adb"
        )


class NRF52(ArmV7MTarget):
    @property
    def name(self):
        return "nRF52"

    @property
    def parent(self):
        return CortexMArch()

    @property
    def loaders(self):
        return ("ROM",)

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def system_ads(self):
        # Use custom System package since system-xi-cortexm4 assumes
        # 4-bit interrupt priorities, but the nRF52 only supports
        # 3-bit interrupt priorities. This requires different
        # definitions for Priority and Interrupt_Priority in System.
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "arm/nordic/nrf52/system-xi-nrf52-sfp.ads",
            "embedded": "arm/nordic/nrf52/system-xi-nrf52-full.ads",
        }

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mfpu=fpv4-sp-d16",
            "-mcpu=cortex-m4",
        )

    def __init__(self):
        super(NRF52, self).__init__()

        self.add_linker_script("arm/nordic/nrf52/common-ROM.ld", loaders=("ROM",))
        self.add_linker_script(
            f"arm/nordic/nrf52/memory-map_{self.name}.ld",
            dest_path_str="memory-map.ld",
        )

        self.add_gnat_sources(
            "arm/nordic/nrf52/s-bbmcpa.ads",
            "arm/nordic/nrf52/start-common.S",
            "arm/nordic/nrf52/start-rom.S",
            "arm/nordic/nrf52/setup_board.ads",
        )

        self.add_gnarl_sources(
            "shared/s-bbpara__nrf52.ads",
            "shared/s-bbbosu__nrf52.adb",
            "shared/s-bcpcst__pendsv.adb",
        )


class NRF52833(NRF52):
    @property
    def name(self):
        return "nrf52833"

    @property
    def use_semihosting_io(self):
        return True

    def __init__(self):
        super(NRF52833, self).__init__()

        self.add_gnat_sources(
            "arm/nordic/nrf52/nrf52833/s-bbbopa.ads",
            "arm/nordic/nrf52/nrf52833/setup_board.adb",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-clock.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-ficr.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-gpio.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-uicr.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-nvmc.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-rtc.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-uart.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-temp.ads",
            "arm/nordic/nrf52/nrf52833/svd/i-nrf52-approtect.ads",
        )

        # ravenscar support
        self.add_gnarl_sources(
            "arm/nordic/nrf52/nrf52833/svd/handler.S",
            "arm/nordic/nrf52/nrf52833/svd/a-intnam.ads",
        )


class NRF52840(NRF52):
    @property
    def name(self):
        return "nrf52840"

    @property
    def use_semihosting_io(self):
        return True

    def __init__(self):
        super(NRF52840, self).__init__()

        self.add_gnat_sources(
            "arm/nordic/nrf52/nrf52840/s-bbbopa.ads",
            "arm/nordic/nrf52/nrf52840/setup_board.adb",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-ccm.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-clock.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-ficr.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-gpio.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-uicr.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-nvmc.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-rtc.ads",
            "arm/nordic/nrf52/nrf52840/svd/i-nrf52-temp.ads",
        )
        self.add_gnarl_sources(
            "arm/nordic/nrf52/nrf52840/svd/handler.S",
            "arm/nordic/nrf52/nrf52840/svd/a-intnam.ads",
        )


class NRF52832(NRF52):
    @property
    def name(self):
        return "nrf52832"

    @property
    def use_semihosting_io(self):
        return True

    def __init__(self):
        super(NRF52832, self).__init__()

        self.add_gnat_sources(
            "arm/nordic/nrf52/nrf52832/s-bbbopa.ads",
            "arm/nordic/nrf52/nrf52832/setup_board.adb",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-clock.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-ficr.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-gpio.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-uicr.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-nvmc.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-rtc.ads",
            "arm/nordic/nrf52/nrf52832/svd/i-nrf52-temp.ads",
        )

        self.add_gnarl_sources(
            "arm/nordic/nrf52/nrf52832/svd/handler.S",
            "arm/nordic/nrf52/nrf52832/svd/a-intnam.ads",
        )


class Stm32CommonArchSupport(ArchSupport):
    """Holds sources common to all stm32 boards"""

    @property
    def name(self):
        return "stm32"

    @property
    def parent(self):
        return ArmV7MArch()

    @property
    def loaders(self):
        return ("ROM", "RAM")

    def __init__(self):
        super(Stm32CommonArchSupport, self).__init__()

        self.add_linker_script("arm/stm32/common-RAM.ld", loaders=("RAM",))
        self.add_linker_script("arm/stm32/common-ROM.ld", loaders=("ROM",))

        self.add_gnat_sources(
            "shared/s-bbpara__stm32f4.ads",
            "arm/stm32/s-stm32.ads",
            "arm/stm32/start-rom.S",
            "arm/stm32/start-ram.S",
            "arm/stm32/start-common.S",
            "arm/stm32/setup_pll.adb",
            "arm/stm32/setup_pll.ads",
        )


stm32_board_configuration = {
    "stm32f4": {
        "STM32_Main_Clock_Frequency": "168_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "nucleo_f401re": {
        "STM32_Main_Clock_Frequency": "168_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "feather_stm32f405": {
        "STM32_Main_Clock_Frequency": "168_000_000",
        "STM32_HSE_Clock_Frequency": "12_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "stm32f429disco": {
        "STM32_Main_Clock_Frequency": "180_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "openmv2": {
        "STM32_Main_Clock_Frequency": "180_000_000",
        "STM32_HSE_Clock_Frequency": "12_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "stm32f469disco": {
        "STM32_Main_Clock_Frequency": "180_000_000",
        "STM32_HSE_Clock_Frequency": "8_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "stm32f746disco": {
        "STM32_Main_Clock_Frequency": "200_000_000",
        "STM32_HSE_Clock_Frequency": "25_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "stm32756geval": {
        "STM32_Main_Clock_Frequency": "180_000_000",
        "STM32_HSE_Clock_Frequency": "25_000_000",
        "STM32_FLASH_Latency": "5",
    },
    "stm32f769disco": {
        "STM32_Main_Clock_Frequency": "200_000_000",
        "STM32_HSE_Clock_Frequency": "25_000_000",
        "STM32_FLASH_Latency": "6",
    },
}


class Stm32(ArmV7MTarget):
    """Generic handling of stm32 boards"""

    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return Stm32CommonArchSupport()

    @property
    def readme_file(self):
        return Path("arm/stm32/README")

    @property
    def use_semihosting_io(self):
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        if self.mcu == "stm32f7x9":
            return True
        else:
            return False

    @property
    def cortex(self):
        if self.mcu.startswith("stm32f4"):
            return "cortex-m4"
        elif self.mcu.startswith("stm32f7"):
            return "cortex-m7"
        else:
            raise ValueError(f"Unexpected MCU {self.mcu}")

    @property
    def fpu(self):
        if self.cortex == "cortex-m4":
            return "fpv4-sp-d16"
        elif not self.has_double_precision_fpu:
            return "fpv5-sp-d16"
        else:
            return "fpv5-d16"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.cortex}",
            f"-mfpu={self.fpu}",
            "-mthumb",
        )

    def __init__(self, board):
        self.board = board
        if self.board in ["stm32f4", "feather_stm32f405"]:
            self.mcu = "stm32f40x"
        elif self.board in ["nucleo_f401re"]:
            self.mcu = "stm32f401"
        elif self.board in ["stm32f429disco", "openmv2"]:
            self.mcu = "stm32f429x"
        elif self.board in ["stm32f469disco"]:
            self.mcu = "stm32f469x"
        elif self.board in ["stm32f746disco", "stm32756geval"]:
            self.mcu = "stm32f7x"
        elif self.board in ["stm32f769disco"]:
            self.mcu = "stm32f7x9"
        else:
            raise ValueError(f"Unknown stm32 board: {self.board}")

        super(Stm32, self).__init__()

        # Add board template configuration
        for key, value in stm32_board_configuration[self.board].items():
            self.add_template_config_value(key, value)

        self.add_template_config_value("Board_Name", self.board)
        self.add_template_config_value("MCU_Name", self.mcu)

        self.add_linker_script(f"arm/stm32/{self.mcu}/memory-map.ld")

        # startup code
        self.add_gnat_sources(
            f"arm/stm32/{self.mcu}/s-bbmcpa.ads",
            f"arm/stm32/{self.mcu}/s-bbmcpa.adb",
            f"arm/stm32/{self.mcu}/svd/i-stm32.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-flash.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-gpio.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-pwr.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-rcc.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-syscfg.ads",
            f"arm/stm32/{self.mcu}/svd/i-stm32-usart.ads",
        )

        self.add_gnat_source("arm/stm32/s-bbbopa.ads.tmpl")

        if self.mcu in ["stm32f40x"]:
            self.add_gnat_source("arm/stm32/stm32f40x/s-stm32.adb")

        elif self.mcu in ["stm32f401"]:
            self.add_gnat_source("arm/stm32/stm32f401/s-stm32.adb")

        elif self.mcu in ["stm32f429x", "stm32f469x"]:
            self.add_gnat_source("arm/stm32/stm32f429x/s-stm32.adb")

        elif self.mcu in ["stm32f7x", "stm32f7x9"]:
            self.add_gnat_source("arm/stm32/stm32f7x/s-stm32.adb")

        # tasking support
        self.add_gnarl_sources(
            f"arm/stm32/{self.mcu}/svd/handler.S",
            f"arm/stm32/{self.mcu}/svd/a-intnam.ads",
        )


class Stm32lCommonArchSupport(ArchSupport):
    """Holds sources common to all stm32l boards. This is currently seperate
    from stm32 board support due to the differences between PLL and memory
    setup.
    """

    @property
    def name(self):
        return "stm32l"

    @property
    def parent(self):
        return ArmV7MArch()

    @property
    def loaders(self):
        return ("ROM", "RAM")

    def __init__(self):
        super(Stm32lCommonArchSupport, self).__init__()

        self.add_linker_script("arm/stm32l/common-RAM.ld")
        self.add_linker_script("arm/stm32l/common-ROM.ld")

        self.add_gnat_sources(
            "shared/s-bbpara__stm32l5.ads",
            "arm/stm32l/s-stm32.ads",
            "arm/stm32l/start-rom.S",
            "arm/stm32l/start-ram.S",
            "arm/stm32l/start-common.S",
            "arm/stm32l/setup_pll.adb",
            "arm/stm32l/setup_pll.ads",
        )


class Stm32l(ArmV7MTarget):
    """Generic handling of stm32l boards"""

    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return Stm32lCommonArchSupport()

    @property
    def readme_file(self):
        return Path("arm/stm32l/README")

    @property
    def use_semihosting_io(self):
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    def cortex(self):
        if self.mcu.startswith("stm32l5"):
            return "cortex-m33"
        else:
            raise ValueError(f"Unexpected MCU {self.mcu}")

    @property
    def fpu(self):
        if self.cortex == "cortex-m4":
            return "fpv4-sp-d16"
        elif not self.has_double_precision_fpu:
            return "fpv5-sp-d16"
        else:
            return "fpv5-d16"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mfloat-abi=hard",
            f"-mcpu={self.cortex}",
            f"-mfpu={self.fpu}",
            "-mthumb",
        )

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-stm32l5-sfp.ads",
        }

    @property
    def loaders(self):
        return (
            "ROM",
            "RAM",
            "ROM_TZNONSECURE",
            "ROM_TZSECURE",
            "RAM_TZNONSECURE",
            "RAM_TZSECURE",
        )

    def __init__(self, board):
        self.board = board
        if self.board in ["stm32l562disco"]:
            self.mcu = "stm32l5x2"
        else:
            raise ValueError(f"Unknown stm32l board: {self.board}")

        super(Stm32l, self).__init__()

        # Add board template configuration
        # for key, value in stm32_board_configuration[self.board].items():
        #    self.add_template_config_value(key, value)

        # self.add_template_config_value('Board_Name', self.board)
        # self.add_template_config_value('MCU_Name', self.mcu)

        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-RAM.ld", loaders=("RAM",)
        )
        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-ROM.ld", loaders=("ROM",)
        )
        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-ROM-TZ_NONSECURE.ld",
            loaders=("ROM_TZNONSECURE",),
        )
        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-ROM-TZ_SECURE.ld",
            loaders=("ROM_TZSECURE",),
        )
        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-RAM-TZ_NONSECURE.ld",
            loaders=("RAM_TZNONSECURE",),
        )
        self.add_linker_script(
            f"arm/stm32l/{self.mcu}/memory-map-RAM-TZ_SECURE.ld",
            loaders=("RAM_TZSECURE",),
        )

        # startup code
        self.add_gnat_sources(
            f"arm/stm32l/{self.mcu}/s-bbmcpa.ads",
            f"arm/stm32l/{self.mcu}/s-bbmcpa.adb",
            f"arm/stm32l/{self.mcu}/svd/i-stm32.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-flash.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-gpio.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-pwr.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-rcc.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-syscfg.ads",
            f"arm/stm32l/{self.mcu}/svd/i-stm32-usart.ads",
        )

        # self.add_gnat_source('arm/stm32/s-bbbopa.ads.tmpl')
        self.add_gnat_source(f"arm/stm32l/{self.mcu}/s-bbbopa.ads")

        if self.mcu in ["stm32l5x2"]:
            self.add_gnat_source("arm/stm32l/stm32l5x2/s-stm32.adb")

        # tasking support
        self.add_gnarl_sources(
            f"arm/stm32l/{self.mcu}/svd/handler.S",
            f"arm/stm32l/{self.mcu}/svd/a-intnam.ads",
        )


class CortexM0(ArmV6MTarget):
    @property
    def name(self):
        return "cortex-m0"

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m0")

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM0P(CortexM0):
    @property
    def name(self):
        return "cortex-m0p"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m0plus")


class CortexM1(ArmV6MTarget):
    @property
    def name(self):
        return "cortex-m1"

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m1")

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM3(ArmV7MTarget):
    @property
    def name(self):
        return "cortex-m3"

    @property
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m3")

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM4(ArmV7MTarget):
    @property
    def name(self):
        return "cortex-m4"

    @property
    def has_single_precision_fpu(self) -> bool:
        return False

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m4")

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM4F(CortexM4):
    @property
    def name(self):
        return "cortex-m4f"

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m4",
            "-mfpu=fpv4-sp-d16",
        )


class CortexM7F(ArmV7MTarget):
    @property
    def name(self):
        return "cortex-m7f"

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m7",
            "-mfpu=fpv5-sp-d16",
        )

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM7DF(CortexM7F):
    @property
    def name(self):
        return "cortex-m7df"

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m7",
            "-mfpu=fpv5-d16",
        )


class ArmV8MArch(ArchSupport):
    @property
    def name(self):
        return "armv8-m"

    @property
    def parent(self):
        return CortexMArch()

    def __init__(self):
        super(ArmV8MArch, self).__init__()
        self.add_gnarl_sources(
            "shared/s-bbbosu__armv7m.adb", "shared/s-bcpcst__pendsv.adb"
        )


class ArmV8MTarget(ArmV7MTarget):
    @property
    def parent(self):
        return ArmV8MArch()

    @property
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
            "light-tasking": "system-xi-cortexm4-sfp.ads",
            "embedded": "system-xi-cortexm4-full.ads",
        }

    def __init__(self):
        super(ArmV7MTarget, self).__init__()


class CortexM23(ArmV8MTarget):
    @property
    def name(self):
        return "cortex-m23"

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ("-mlittle-endian", "-mthumb", "-mfloat-abi=soft", "-mcpu=cortex-m23")

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM33F(ArmV8MTarget):
    @property
    def name(self):
        return "cortex-m33f"

    @property
    def has_fpu(self) -> bool:
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m33",
            "-mfpu=fpv5-sp-d16",
        )

    @property
    def system_ads(self):
        return {"light": "system-xi-arm.ads"}


class CortexM33DF(CortexM33F):
    @property
    def name(self):
        return "cortex-m33df"

    @property
    def compiler_switches(self):
        # The required compiler switches
        return (
            "-mlittle-endian",
            "-mthumb",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m33",
            "-mfpu=fpv5-d16",
        )


class RP2040(CortexM0P):
    @property
    def name(self):
        if self.smp:
            return "rp2040-smp"
        else:
            return "rp2040"

    @property
    def parent(self):
        if self.smp:
            # Don't refer to any parent since we need to override certain
            # sources from CortexMArch (e.g. replace src/s-bbsumu__generic.adb)
            return None
        else:
            return CortexMArch()

    @property
    def loaders(self):
        return ("ROM",)

    @property
    def system_ads(self):
        return {
            "light-tasking": "system-xi-armv6m-sfp.ads",
            "embedded": "system-xi-armv6m-full.ads",
        }

    def __init__(self, smp):
        self.smp = smp

        super(RP2040, self).__init__()

        smp_template_values = {
            # The SMP runtime uses the TIMER for task delays, which runs from
            # the 1 MHz Watchdog tick.
            "RP2040_Runtime_Uses_SysTick": "False",
            # The MP runtime overrides the SIO_IRQ_PROC0 and SIO_IRQ_PROC1
            # handers to call the poke handler. See start-rom.S.tmpl
            "RP2040_SIO_IRQ_PROC0_Handler": "__gnat_poke_handler",
            "RP2040_SIO_IRQ_PROC1_Handler": "__gnat_poke_handler",
            # 32 IRQs on both cores are available in the MP runtime (64 total)
            "RP2040_Number_Of_Interrupts": "64",
            # Config values for bbpara__cortexm0p.ads.tmpl
            "S_BBPara_Interrupt_Stack_Size": "1024",
            "S_BBPara_Interrupt_Sec_Stack_Size": "128",
            "S_BBPara_Max_Number_Of_CPUs": "2",
        }

        sp_template_values = {
            # The SP runtime uses the SysTick for task delays, which runs from
            # the system clock (up to 133 MHz).
            "RP2040_Runtime_Uses_SysTick": "True",
            # The SP runtime does not override SIO_IRQ_PROC0 or SIO_IRQ_PROC1.
            # They can be used as ordinary interrupts. See start-rom.S.tmpl
            "RP2040_SIO_IRQ_PROC0_Handler": "__gnat_irq_trap",
            "RP2040_SIO_IRQ_PROC1_Handler": "__gnat_irq_trap",
            # Only the 32 IRQs on core0 are available in the SP runtime.
            "RP2040_Number_Of_Interrupts": "32",
            # Config values for bbpara__cortexm0p.ads.tmpl
            "S_BBPara_Interrupt_Stack_Size": "1024",
            "S_BBPara_Interrupt_Sec_Stack_Size": "128",
            "S_BBPara_Max_Number_Of_CPUs": "1",
        }

        if self.smp:
            template_values = smp_template_values
        else:
            template_values = sp_template_values

        for key, value in template_values.items():
            self.add_template_config_value(key, value)

        # Common GNAT sources
        self.add_gnat_sources(
            "arm/rpi/rp2040/svd/i-rp2040.ads",
            "arm/rpi/rp2040/svd/i-rp2040-clocks.ads",
            "arm/rpi/rp2040/svd/i-rp2040-pll_sys.ads",
            "arm/rpi/rp2040/svd/i-rp2040-psm.ads",
            "arm/rpi/rp2040/svd/i-rp2040-resets.ads",
            "arm/rpi/rp2040/svd/i-rp2040-rosc.ads",
            "arm/rpi/rp2040/svd/i-rp2040-sio.ads",
            "arm/rpi/rp2040/svd/i-rp2040-timer.ads",
            "arm/rpi/rp2040/svd/i-rp2040-watchdog.ads",
            "arm/rpi/rp2040/svd/i-rp2040-xosc.ads",
            "arm/rpi/rp2040/s-bbmcpa.ads.tmpl",
            "arm/rpi/rp2040/start-rom.S.tmpl",
            "arm/rpi/rp2040/s-bootro.ads",
            "arm/rpi/rp2040/s-bootro.adb",
            "arm/rpi/rp2040/setup_clocks.adb",
            "arm/rpi/rp2040/s-bbbopa.ads.tmpl",
        )

        if self.smp:
            # s-maxres__cortexm3.adb is also compatible with Cortex-M0+
            self.add_gnat_sources("shared/s-macres__cortexm3.adb")

        # Common GNARL sources
        self.add_gnarl_sources("shared/s-bbpara__cortexm0p.ads.tmpl")

        if self.smp:
            self.add_gnarl_sources(
                "arm/rpi/rp2040/a-intnam__mp.ads",
                "shared/s-bbbosu__rp2040.adb",
                "shared/s-bbcppr__armv7m.adb",
                "shared/s-bbcppr__old.ads",
                "shared/s-bbcpsp__cortexm.ads",
                "shared/s-bbpara__cortexm0p.ads.tmpl",
                "shared/s-bbsumu__rp2040.adb",
                "shared/s-bcpcst__rp2040.adb",
                "shared/s-bcpcst__armvXm.ads",
            )
        else:
            self.add_gnarl_sources(
                "arm/rpi/rp2040/a-intnam__sp.ads",
                "shared/s-bbbosu__armv6m.adb",
                "shared/s-bcpcst__pendsv.adb",
            )


class RP2040Target(RP2040):
    # List of flash chips used by the supported targets
    _flash_properties = {
        "generic_qspi_128": {"size": "16M", "boot2": "generic_qspi"},
        "at25sf128a": {"size": "16M", "boot2": "generic_qspi"},
        "gd25q64c": {"size": "8M", "boot2": "generic_qspi"},
        "w25q16jv": {"size": "2M", "boot2": "w25qxx"},
        "w25q32jv": {"size": "4M", "boot2": "w25qxx"},
        "w25q64jv": {"size": "8M", "boot2": "w25qxx"},
        "w25q128jv": {"size": "16M", "boot2": "w25qxx"},
    }

    _target_properties = {
        # RaspberryPi targets
        "rpi-pico": {"flash": "w25q16jv", "xosc_startup_delay_mult": "1"},
        # Adafruit targets
        "adafruit-feather-rp2040": {
            "flash": "gd25q64c",
            "xosc_startup_delay_mult": "64",
        },
        "adafruit-itsybitsy-rp2040": {
            "flash": "w25q64jv",
            "xosc_startup_delay_mult": "64",
        },
        "adafruit-macropad-rp2040": {
            "flash": "w25q64jv",
            "xosc_startup_delay_mult": "64",
        },
        "adafruit-qt2040-trinkey": {
            "flash": "w25q64jv",
            "xosc_startup_delay_mult": "64",
        },
        "adafruit-qtpy-rp2040": {"flash": "w25q64jv", "xosc_startup_delay_mult": "64"},
        # Arduino targets
        "arduino-nano-rp2040-connect": {
            # Some batches of the Nano RP2040 Connect use different flash
            # chips (AT25SF128A and IS25LP128F) which have differences in
            # how the QE bit is set. The generic_qspi boot2 is compatible
            # with both of these chips.
            "flash": "generic_qspi_128",
            "xosc_startup_delay_mult": "1",
        },
        # Pimoroni targets
        "pimoroni-interstate75": {"flash": "w25q64jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-keybow2040": {"flash": "w25q16jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-pga2040": {"flash": "w25q64jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-picolipo-4m": {"flash": "w25q32jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-picolipo-16m": {"flash": "w25q128jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-picosystem": {"flash": "w25q128jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-plasma2040": {"flash": "w25q64jv", "xosc_startup_delay_mult": "1"},
        "pimoroni-tiny2040": {"flash": "w25q64jv", "xosc_startup_delay_mult": "1"},
        # Sparkfun targets
        "sparkfun-micromod": {"flash": "w25q128jv", "xosc_startup_delay_mult": "1"},
        "sparkfun-promicro": {"flash": "w25q128jv", "xosc_startup_delay_mult": "1"},
        "sparkfun-thingplus": {"flash": "w25q128jv", "xosc_startup_delay_mult": "1"},
    }

    # List of supported targets. Each target has an SMP and non-SMP variant
    # For example, 'rpi-pico' and 'rpi-pico-smp'
    supported_targets = list(_target_properties.keys()) + [
        t + "-smp" for t in _target_properties.keys()
    ]

    @property
    def name(self):
        return self._name

    def __init__(self, name):
        super(RP2040Target, self).__init__(smp=name.endswith("-smp"))

        # Check if the base name (without -smp suffix) is one of the
        # targets supported by this class
        self._name = name
        base_name = name.removesuffix("-smp")
        if base_name not in self._target_properties:
            raise RuntimeError(f"Unknown RP2040 target: {name}")

        # Lookup the flash properties of this target
        target_properties = self._target_properties[base_name]
        flash_properties = self._flash_properties[target_properties["flash"]]
        flash_size = flash_properties["size"]
        boot2_variant = flash_properties["boot2"]

        # Get other properties
        xosc_startup_delay_mult = target_properties["xosc_startup_delay_mult"]

        template_values = {
            # All targets use a 12 MHz XOSC
            "RP2040_XOSC_Frequency": "12_000_000",
            "RP2040_XOSC_Startup_Delay_Multiplier": xosc_startup_delay_mult,
            # pll_sys configuration
            # ((12 MHz / 1) * 125) / (6 * 2) = 125 MHz
            "RP2040_PLL_Sys_Reference_Div": "1",
            "RP2040_PLL_Sys_VCO_Multiple": "125",
            "RP2040_PLL_Sys_Post_Div_1": "6",
            "RP2040_PLL_Sys_Post_Div_2": "2",
            # pll_usb configuration
            # ((12 MHz / 1) * 40) / (5 * 2) = 48 MHz
            "RP2040_PLL_USB_Reference_Div": "1",
            "RP2040_PLL_USB_VCO_Multiple": "40",
            "RP2040_PLL_USB_Post_Div_1": "5",
            "RP2040_PLL_USB_Post_Div_2": "2",
            # Linker script settings
            # Striped SRAM is aliased to 0x20000000
            # Non-striped SRAM is aliased to 0x21000000
            "RP2040_Linker_Flash_Size": flash_size,
            "RP2040_Linker_SRAM_Origin": "0x20000000",
        }

        for key, value in template_values.items():
            self.add_template_config_value(key, value)

        self.add_linker_script("arm/rpi/rp2040/memory-map.ld.tmpl")
        self.add_linker_script("arm/rpi/rp2040/common-ROM.ld", loaders=("ROM",))
        self.add_linker_script("arm/rpi/rp2040/common-RAM.ld", loaders=("RAM",))

        self.add_gnat_sources(
            f"arm/rpi/rp2040/boot2/generated/boot2__{boot2_variant}.S"
        )
