#
# Copyright (C) 2025-2026, AdaCore
#

"""Runtime targetizer module for RTS Prebuilder."""

from .cli import RuntimeTargetizerCLI
from .targetizer import RuntimeTargetizer

__all__ = ["RuntimeTargetizerCLI", "RuntimeTargetizer"]
