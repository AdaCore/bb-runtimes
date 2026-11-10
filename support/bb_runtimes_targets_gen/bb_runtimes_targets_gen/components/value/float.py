#
# Copyright (C) 2026, AdaCore
#

"""'Value support for floating-point types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueFloat(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-valuer.ads",
            "s-valuer.adb",
            "s-valrea.ads",
            "s-valrea.adb",
            "s-valflt.ads",
            "s-vallfl.ads",
            "s-valllf.ads",
        )
