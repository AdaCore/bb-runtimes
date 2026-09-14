#
# Copyright (C) 2026, AdaCore
#

"""Atomic primitives support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Atomics(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-atopri.adb",
            "libgnat/s-atocou__builtin.adb",
            "libgnat/s-atocou.ads",
        )


class Atomics32(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-atopri__32.ads",
        )


class Atomics64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-atopri.ads",
        )
