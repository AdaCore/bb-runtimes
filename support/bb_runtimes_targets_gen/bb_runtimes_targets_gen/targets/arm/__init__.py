#
# Copyright (C) 2025-2026, AdaCore
#

"""ARM target definitions."""

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)

# Star import is necessary because all concrete targets need to exported as well
from .cortexm import (
    CortexM0,
    CortexM0P,
    CortexM1,
    CortexM3,
    CortexM4,
    CortexM4F,
    CortexM7F,
    CortexM7DF,
    CortexM23,
    CortexM33F,
    CortexM33DF,
    LM3S,
    Microbit,
    NRF52833,
    NRF52840,
    NRF52832,
)
from .generators import (
    Stm32F0Generator,
    Stm32Generator,
    Stm32lGenerator,
    SamGenerator,
    RP2040Generator,
)
from .cortexar import AM64xR5, Rpi2, Rpi2Mc, TMS570, ZynqmpR5, Zynq7000

TARGETS: list[AbstractTarget | AbstractTargetGenerator] = [
    # Cortex-M targets
    CortexM0(),
    CortexM0P(),
    CortexM1(),
    CortexM3(),
    CortexM4(),
    CortexM4F(),
    CortexM7F(),
    CortexM7DF(),
    CortexM23(),
    CortexM33F(),
    CortexM33DF(),
    LM3S(),
    Microbit(),
    NRF52833(),
    NRF52840(),
    NRF52832(),
    # Cortex-M: Generators for large families
    RP2040Generator(),
    SamGenerator(),
    Stm32F0Generator(),
    Stm32Generator(),
    Stm32lGenerator(),
    # ------------------------------
    # Cortex-A R targets
    AM64xR5(),
    Rpi2(),
    Rpi2Mc(),
    TMS570(variant="tms570ls31", uart_io=False),
    TMS570(variant="tms570ls31", uart_io=True),
    # TMS570(variant="tms570lc31", uart_io=False), # TODO:2 Does not build, fix ?
    # TMS570(variant="tms570lc31", uart_io=True),  # TODO:2 Does not build, fix ?
    ZynqmpR5(),
    Zynq7000(),
]
