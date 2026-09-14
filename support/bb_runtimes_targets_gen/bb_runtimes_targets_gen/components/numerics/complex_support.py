#
# Copyright (C) 2026, AdaCore
#

"""Complex types support for the math library."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class MathComplex(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/a-ncelfu.ads",
            "libgnat/a-ngcefu.ads",
            "libgnat/a-ngcefu.adb",
            "libgnat/a-ngcoty.ads",
            "hie/a-ngcoty__ada.adb",
            "libgnat/a-nlcefu.ads",
            "libgnat/a-nlcoty.ads",
            "libgnat/a-nllcef.ads",
            "libgnat/a-nllcty.ads",
            "libgnat/a-nscefu.ads",
            "libgnat/a-nscoty.ads",
            "libgnat/a-nucoty.ads",
        )
