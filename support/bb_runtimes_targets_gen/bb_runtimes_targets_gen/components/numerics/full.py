#
# Copyright (C) 2026, AdaCore
#

"""Full-profile additions to the math library."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class MathFull(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "a-ngcoar.ads",
            "a-ngcoar.adb",
            "a-ngrear.ads",
            "a-ngrear.adb",
            "a-nurear.ads",
            "s-gearop.ads",
            "s-gearop.adb",
        )
