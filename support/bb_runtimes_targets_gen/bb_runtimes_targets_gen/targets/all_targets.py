#
# Copyright (C) 2025-2026, AdaCore
#

"""
Aggregates all TARGETS from all target subpackages.

This simplifies some use cases where all targets need to be accessed
from a single location. such as general CLI or All targets testing.
"""

# Import TARGETS from all subpackages
from .aarch64 import TARGETS as AARCH64_TARGETS
from .arm import TARGETS as ARM_TARGETS
from .deos import TARGETS as DEOS_TARGETS
from .freertos import TARGETS as FREERTOS_TARGETS
from .linux import TARGETS as LINUX_TARGETS
from .lynx import TARGETS as LYNX_TARGETS
from .pikeos import TARGETS as PIKEOS_TARGETS
from .powerpc import TARGETS as POWERPC_TARGETS
from .qnx import TARGETS as QNX_TARGETS
from .riscv import TARGETS as RISCV_TARGETS
from .sparc import TARGETS as SPARC_TARGETS
from .visium import TARGETS as VISIUM_TARGETS
from .vx7r2cert import TARGETS as VX7R2CERT_TARGETS
from .windows import TARGETS as WINDOWS_TARGETS
from .x86_64 import TARGETS as X86_64_TARGETS

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)


# Aggregate all targets into a single list
ALL_TARGETS: list[AbstractTargetGenerator | AbstractTarget] = [
    *AARCH64_TARGETS,
    *ARM_TARGETS,
    *DEOS_TARGETS,
    *FREERTOS_TARGETS,
    *LINUX_TARGETS,
    *LYNX_TARGETS,
    *PIKEOS_TARGETS,
    *POWERPC_TARGETS,
    *QNX_TARGETS,
    *RISCV_TARGETS,
    *SPARC_TARGETS,
    *VISIUM_TARGETS,
    *VX7R2CERT_TARGETS,
    *WINDOWS_TARGETS,
    *X86_64_TARGETS,
]

__all__ = ["ALL_TARGETS"]
