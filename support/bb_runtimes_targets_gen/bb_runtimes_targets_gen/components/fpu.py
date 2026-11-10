#
# Copyright (C) 2026, AdaCore
#

"""Floating-point attribute support core."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Fpu(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-fatgen.ads",
            "s-fatgen.adb",
            "s-fatflt.ads",
            "s-fatlfl.ads",
            "s-fatllf.ads",
        )
