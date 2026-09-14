#
# Copyright (C) 2026, AdaCore
#

"""Shared 'Value support utilities."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueUtils(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-valuti.ads",
            "libgnat/s-valuti.adb",
        )
