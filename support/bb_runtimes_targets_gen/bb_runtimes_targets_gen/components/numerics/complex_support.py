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
            "a-ncelfu.ads",
            "a-ngcefu.ads",
            "a-ngcefu.adb",
            "a-ngcoty.ads",
            "a-ngcoty__ada.adb",
            "a-nlcefu.ads",
            "a-nlcoty.ads",
            "a-nllcef.ads",
            "a-nllcty.ads",
            "a-nscefu.ads",
            "a-nscoty.ads",
            "a-nucoty.ads",
        )
