#
# Copyright (C) 2026, AdaCore
#

"""'Value support for character types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueChar(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valcha.ads",
            "libgnat/s-valcha.adb",
        )


class ValueWideChar(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valwch.ads",
            "libgnat/s-valwch.adb",
        )
