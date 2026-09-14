#
# Copyright (C) 2026, AdaCore
#

"""'Value support for integer types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueInt(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valint.ads",
            "libgnat/s-valueu.ads",
            "libgnat/s-valueu.adb",
            "libgnat/s-valuei.ads",
            "libgnat/s-valuei.adb",
            "libgnat/s-valuns.ads",
        )


class ValueIntLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-vallli.ads",
            "libgnat/s-valllu.ads",
        )


class ValueIntLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valllli.ads",
            "libgnat/s-vallllu.ads",
        )
