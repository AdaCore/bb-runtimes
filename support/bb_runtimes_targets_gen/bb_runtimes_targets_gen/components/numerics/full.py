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
            "libgnat/a-ngcoar.ads",
            "libgnat/a-ngcoar.adb",
            "libgnat/a-ngrear.ads",
            "libgnat/a-ngrear.adb",
            "libgnat/a-nurear.ads",
            "libgnat/s-gearop.ads",
            "libgnat/s-gearop.adb",
        )
