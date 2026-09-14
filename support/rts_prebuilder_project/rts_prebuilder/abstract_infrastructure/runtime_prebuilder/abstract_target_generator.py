#
# Copyright (C) 2025-2026, AdaCore
#

from abc import ABC, abstractmethod
from typing import Iterator
from .abstract_target import AbstractTarget


class AbstractTargetGenerator(ABC):
    """Lazy target family generator for targets with many variants.

    Use it when writing one concrete class per variant would be tedious, and
    the variants can be generated instead. The STM32F0 family is the example.
    It has many part numbers, and they differ only in flash and ram size and in
    clock source.

    The instantiate() method returns a concrete AbstractTarget subclass instance
    for the given variant name.
    """

    @property
    @abstractmethod
    def cli_name_pattern(self) -> str:
        """Short glob/brace-glob used to describe the family in help.

        This will be show for the user to explain how to select the target,
        it is not used for matching.

        Recommendation: Use unix-style globbing with braces inspired of regex.

        Example: "stm32f0{3,4,5,7,9}[0128][cefgkrv][468bc]-(hsi|hse)".
        """
        ...

    @abstractmethod
    def generate_variants(self) -> Iterator[str]:
        """
        Generate valid variant names lazily

        The generated names MUST match those accepted by instantiate().
        """
        ...

    @abstractmethod
    def instantiate(self, name: str) -> AbstractTarget:
        """Create a concrete AbstractTarget instance for the given name."""
        ...

    def match_variant(self, name: str) -> bool:
        for variant in self.generate_variants():
            if name == variant:
                return True
        return False
