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
            "s-valint.ads",
            "s-valueu.ads",
            "s-valueu.adb",
            "s-valuei.ads",
            "s-valuei.adb",
            "s-valuns.ads",
        )


class ValueIntLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-vallli.ads",
            "s-valllu.ads",
        )


class ValueIntLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-valllli.ads",
            "s-vallllu.ads",
        )
