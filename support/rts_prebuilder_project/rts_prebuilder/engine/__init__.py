#
# Copyright (C) 2025-2026, AdaCore
#

"""
Refer to the sphinx documentation for more details on this package.
"""

from .runtime_assembler.assembler import RuntimeAssembler
from .runtime_assembler.cli import RuntimeAssemblerCLI
from .runtime_targetizer.cli import RuntimeTargetizerCLI
from .runtime_targetizer.targetizer import RuntimeTargetizer
from .common.infrastructure_interface import resolve_and_read_file

__all__ = [
    "RuntimeAssembler",
    "RuntimeAssemblerCLI",
    "RuntimeTargetizer",
    "RuntimeTargetizerCLI",
    "resolve_and_read_file",
]
