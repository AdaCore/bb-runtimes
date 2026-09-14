#
# Copyright (C) 2026, AdaCore
#

"""Ada.Streams support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Streams(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/a-stream.ads",
            "libgnat/a-stream.adb",
            "libgnat/s-stratt.ads",
            "libgnat/s-statxd.ads",
            "libgnat/s-statxd.adb",
        )


class StreamsNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-stratt.adb",
        )


class StreamsCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-stratt__cheri.adb",
        )
