#
# Copyright (C) 2026, AdaCore
#

"""'Image support for integer types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageInt(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imagew.ads",
            "libgnat/s-imagew.adb",
            "libgnat/s-imgwiu.ads",
        )


class ImageIntLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imgllw.ads",
        )


class ImageIntLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imglllw.ads",
        )
