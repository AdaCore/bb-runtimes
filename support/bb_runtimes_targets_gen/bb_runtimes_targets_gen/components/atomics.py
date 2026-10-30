#
# Copyright (C) 2026, AdaCore
#

"""Atomic primitives support."""

from enum import Enum, auto
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class AtomicSupport(Enum):
    """Selects atomic packages and the width of the System.Atomic_Primitives interface."""

    NONE = auto()
    UP_TO_32_BITS = auto()
    UP_TO_64_BITS = auto()


class Atomics(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-atopri.adb",
            "s-atocou__builtin.adb",
            "s-atocou.ads",
        )


class Atomics32(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-atopri__32.ads",
        )


class Atomics64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-atopri.ads",
        )
