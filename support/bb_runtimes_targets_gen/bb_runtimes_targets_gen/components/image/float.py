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
            "libgnat/s-imager.ads",
            "libgnat/s-imager.adb",
            "libgnat/s-imgflt.ads",
            "libgnat/s-imglfl.ads",
            "libgnat/s-imgllf.ads",
        )
