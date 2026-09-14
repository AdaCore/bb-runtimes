#
# Copyright (C) 2026, AdaCore
#

"""Ada.Command_Line support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class CommandLine(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/a-comlin.ads",
            "libgnat/a-comlin.adb",
            "argv.c",
            "runtime.h",
        )
