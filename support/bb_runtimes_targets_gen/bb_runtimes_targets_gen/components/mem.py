#
# Copyright (C) 2026, AdaCore
#

"""Memory operation intrinsics (memcpy & co) when no libc is available."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Mem(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-memcom.ads",
            "hie/s-memcom.adb",
            "hie/s-memcop.ads",
            "hie/s-memmov.ads",
            "hie/s-memset.ads",
            "hie/s-memset.adb",
        )


class MemNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-memtyp.ads",
            "hie/s-memcop.adb",
            "hie/s-memmov.adb",
        )


class MemCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-memtyp__cheri.ads",
            "hie/s-memcop__cheri.adb",
            "hie/s-memmov__cheri.adb",
        )
