#
# Copyright (C) 2026, AdaCore
#

"""'Image support for floating-point types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageFloat(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-imager.ads",
            "s-imager.adb",
            "s-imgflt.ads",
            "s-imglfl.ads",
            "s-imgllf.ads",
        )
