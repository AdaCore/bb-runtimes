#
# Copyright (C) 2026, AdaCore
#

"""'Value scanner shared by the real types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueReal(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-valuer.ads",
            "s-valuer.adb",
        )
