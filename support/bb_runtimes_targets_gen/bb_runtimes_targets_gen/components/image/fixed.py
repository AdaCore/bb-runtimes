#
# Copyright (C) 2026, AdaCore
#

"""'Image support for ordinary fixed-point types."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageFixed(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imagef.ads",
            "libgnat/s-imagef.adb",
            "libgnat/s-imfi32.ads",
        )


class ImageFixedLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imfi64.ads",
        )


class ImageFixedLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imfi128.ads",
        )
