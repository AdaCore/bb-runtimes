#
# Copyright (C) 2026, AdaCore
#

"""'Image support for character types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageChar(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imgcha.adb",
            "libgnat/s-imgcha.ads",
        )


class ImageWideChar(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imgwch.ads",
            "libgnat/s-imgwch.adb",
        )
