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
            "libgnat/s-fatgen.ads",
            "libgnat/s-fatgen.adb",
            "libgnat/s-fatflt.ads",
            "libgnat/s-fatlfl.ads",
            "libgnat/s-fatllf.ads",
        )
