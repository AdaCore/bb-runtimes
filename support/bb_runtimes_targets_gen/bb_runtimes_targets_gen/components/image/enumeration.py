#
# Copyright (C) 2026, AdaCore
#

"""'Image support for enumeration types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageEnum(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imagen.ads",
            "libgnat/s-imagen.adb",
            "libgnat/s-imen16.ads",
            "libgnat/s-imen32.ads",
            "libgnat/s-imenu8.ads",
        )
