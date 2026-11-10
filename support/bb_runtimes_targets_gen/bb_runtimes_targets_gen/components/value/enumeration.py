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
            "s-valuen.ads",
            "s-valuen.adb",
            "s-vaen16.ads",
            "s-vaen32.ads",
            "s-vaenu8.ads",
        )
