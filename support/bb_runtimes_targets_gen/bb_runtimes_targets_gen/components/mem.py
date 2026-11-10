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
            "s-memcom.ads",
            "s-memcom.adb",
            "hie/s-memcop.ads",
            "s-memmov.ads",
            "s-memset.ads",
            "s-memset.adb",
        )


class MemNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-memtyp.ads",
            "s-memcop.adb",
            "s-memmov.adb",
        )


class MemCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-memtyp__cheri.ads",
            "s-memcop__cheri.adb",
            "s-memmov__cheri.adb",
        )
