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
            "libgnat/s-valuer.ads",
            "libgnat/s-valuer.adb",
            "libgnat/s-valrea.ads",
            "libgnat/s-valrea.adb",
            "libgnat/s-valflt.ads",
            "libgnat/s-vallfl.ads",
            "libgnat/s-valllf.ads",
        )
