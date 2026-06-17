#
# Copyright (C) 2025-2026, AdaCore
#

from rts_prebuilder.abstract_infrastructure import AbstractTargetGenerator

from .cortexm import (
    Sam,
    Stm32F0,
    Stm32,
    Stm32l,
    stm32_board_configuration,
    RP2040Target,
)


class Stm32F0Generator(AbstractTargetGenerator):
    """Generator for STM32F0 target family.

    - Pattern matches the f0 family variants with optional clock source suffix
      e.g., "stm32f071rb-hse" or "stm32f030f4-hsi".
    - Variants are generated lazily from Stm32F0 class tables.
    """

    @property
    def cli_name_pattern(self) -> str:
        return "stm32f0{3,4,5,7,9}[0128][cefgkrv][468bc]-hs[ie]"

    def generate_variants(self):
        # Build valid combinations based on the same constraints used by Stm32F0
        flash_sizes = Stm32F0.flash_sizes
        f03 = Stm32F0.f03x_ram_sizes
        f07 = Stm32F0.f07x_ram_sizes

        for sf in ["3", "4", "5", "7", "9"]:
            for sfm in ["0", "1", "2", "8"]:
                for pkg in ["c", "e", "f", "g", "k", "r", "v"]:
                    for mem in ["4", "6", "8", "b", "c"]:
                        valid = False
                        if sf == "3":
                            valid = (pkg + mem) in f03
                        elif sf in "45":
                            valid = mem in flash_sizes
                        elif sf == "7":
                            valid = (pkg + mem) in f07
                        else:  # sf == "9"
                            valid = mem in flash_sizes

                        if not valid:
                            continue
                        for clk in ("hsi", "hse"):
                            yield f"stm32f0{sf}{sfm}{pkg}{mem}-{clk}"

    def instantiate(self, name: str) -> Stm32F0:
        return Stm32F0(name)


class Stm32Generator(AbstractTargetGenerator):
    """Generator for STM32 F4/F7 board targets backed by explicit config."""

    @property
    def cli_name_pattern(self) -> str:
        return "{stm32f4,stm32f4*disco,stm32f769disco,nucleo_f401re,feather_stm32f405,openmv2}"

    def generate_variants(self):
        yield from stm32_board_configuration.keys()

    def instantiate(self, name: str) -> Stm32:
        return Stm32(name)


class Stm32lGenerator(AbstractTargetGenerator):
    """Generator for STM32L family (currently limited to stm32l562disco)."""

    @property
    def cli_name_pattern(self) -> str:
        return "stm32l562disco"

    def generate_variants(self):
        # Currently only one supported L5 discovery board
        # We still use the generator pattern since the Stm32l.__init__ takes
        # a name argument for future extensibility.
        yield "stm32l562disco"

    def instantiate(self, name: str) -> Stm32l:
        return Stm32l(name)


class SamGenerator(AbstractTargetGenerator):
    """Generator for Microchip/Atmel SAM family boards supported in Sam target."""

    @property
    def cli_name_pattern(self) -> str:
        return "sam{4s,g55,v71,rh71}"

    def generate_variants(self):
        # Valid boards as enforced in Sam.__init__
        yield from ("sam4s", "samg55", "samv71", "samrh71")

    def instantiate(self, name: str) -> Sam:
        return Sam(name)


class RP2040Generator(AbstractTargetGenerator):
    """Generator for RP2040 boards (SP and SMP variants)."""

    @property
    def cli_name_pattern(self) -> str:
        return "{rpi-pico,adafruit-*,sparkfun-*,pimoroni-*,arduino-nano-rp2040-*}[-smp]"

    def generate_variants(self):
        # Reuse the existing target list exposed by the class
        yield from RP2040Target.supported_targets

    def instantiate(self, name: str) -> RP2040Target:
        return RP2040Target(name)
