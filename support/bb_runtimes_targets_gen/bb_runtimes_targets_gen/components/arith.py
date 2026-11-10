#
# Copyright (C) 2026, AdaCore
#

"""64/128-bit arithmetic runtime support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Arith64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-arit64.ads",
            "s-arit64.adb",
            "s-aridou.ads",
            "s-aridou.adb",
        )


class Arith128(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-arit128.ads",
            "s-arit128.adb",
        )
