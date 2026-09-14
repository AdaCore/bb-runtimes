#
# Copyright (C) 2026, AdaCore
#

"""Shared 'Image support utilities."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ImageUtil(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-imguti.ads",
            "libgnat/s-imguti.adb",
        )
