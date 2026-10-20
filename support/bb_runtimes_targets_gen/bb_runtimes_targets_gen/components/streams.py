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
            "a-stream.ads",
            "a-stream.adb",
            "s-stratt.ads",
            "s-statxd.ads",
            "s-statxd.adb",
        )


class StreamsNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-stratt.adb",
        )


class StreamsCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-stratt__cheri.adb",
        )
