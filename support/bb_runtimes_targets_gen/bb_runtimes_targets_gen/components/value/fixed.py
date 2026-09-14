#
# Copyright (C) 2026, AdaCore
#

"""'Value support for ordinary fixed-point types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueFixed(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valuer.ads",
            "libgnat/s-valuer.adb",
            "libgnat/s-valuef.ads",
            "libgnat/s-valuef.adb",
            "libgnat/s-vafi32.ads",
        )


class ValueFixedLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-vafi64.ads",
        )


class ValueFixedLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-vafi128.ads",
        )
