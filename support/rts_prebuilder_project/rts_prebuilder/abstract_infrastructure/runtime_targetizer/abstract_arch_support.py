#
# Copyright (C) 2025-2026, AdaCore
#

from abc import ABC, abstractmethod

from .component_mixin import SourcesAndFlagsComponentMixin


class AbsractArchSupport(ABC, SourcesAndFlagsComponentMixin):
    """
    Base class for architecture,
    Child classes can be CortexAArchSupport, RiscVArchSupport, ...
    On top of holding Architecture specific info,
    it has sources and flags composer capabilities.

    Do not confuse with Target, the relation to target is done through the
    composition using the ComponentMixin (through parent property).
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """Architecture name, as used to name the runtime (e.g., aarch32, aarch64)"""
        ...
