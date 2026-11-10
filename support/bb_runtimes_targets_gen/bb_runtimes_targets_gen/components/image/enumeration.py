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
            "s-imagen.ads",
            "s-imagen.adb",
            "s-imen16.ads",
            "s-imen32.ads",
            "s-imenu8.ads",
        )
