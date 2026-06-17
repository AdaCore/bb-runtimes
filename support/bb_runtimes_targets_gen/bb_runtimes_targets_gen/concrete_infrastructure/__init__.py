#
# Copyright (C) 2025-2026, AdaCore
#

"""
This is a concrete implementation of abstract_infrastructure module
"""

from .engine_interface import engine_interface
from .runtime_targetizer.arch_support import ArchSupport
from .runtime_targetizer.target import DFBBTarget, Target

__all__ = [  # For engine
    "engine_interface",
    # For target definition by inheritance
    "Target",
    "ArchSupport",
    # Custom Targets pre-filled by this infrastructure
    "DFBBTarget",
]
