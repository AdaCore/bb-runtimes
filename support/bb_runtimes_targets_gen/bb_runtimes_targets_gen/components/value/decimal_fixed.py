#
# Copyright (C) 2026, AdaCore
#

"""'Value support for decimal fixed-point types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueDecimal(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valuer.ads",
            "libgnat/s-valuer.adb",
            "libgnat/s-valued.ads",
            "libgnat/s-valued.adb",
            "libgnat/s-vade32.ads",
        )


class ValueDecimalLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-vade64.ads",
        )


class ValueDecimalLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-vade128.ads",
        )
