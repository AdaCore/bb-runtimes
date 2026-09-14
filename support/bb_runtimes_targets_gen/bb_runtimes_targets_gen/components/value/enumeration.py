#
# Copyright (C) 2026, AdaCore
#

"""'Value support for enumeration types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueEnum(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valuen.ads",
            "libgnat/s-valuen.adb",
            "libgnat/s-vaen16.ads",
            "libgnat/s-vaen32.ads",
            "libgnat/s-vaenu8.ads",
        )
