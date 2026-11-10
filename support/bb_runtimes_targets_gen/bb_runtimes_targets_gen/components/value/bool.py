#
# Copyright (C) 2026, AdaCore
#

"""'Value support for Boolean."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ValueBoolean(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-valboo.ads",
            "s-valboo.adb",
        )
