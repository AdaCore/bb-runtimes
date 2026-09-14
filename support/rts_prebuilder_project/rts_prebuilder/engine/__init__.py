#
# Copyright (C) 2025-2026, AdaCore
#

"""
Refer to the sphinx documentation for more details on this package.
"""

from .cli import RuntimePrebuilderCLI
from .prebuild_driver import RuntimePrebuildDriver
from .infrastructure_interface import resolve_and_read_file

__all__ = [
    "RuntimePrebuildDriver",
    "RuntimePrebuilderCLI",
    "resolve_and_read_file",
]
