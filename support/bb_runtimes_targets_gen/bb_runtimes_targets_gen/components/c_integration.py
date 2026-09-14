#
# Copyright (C) 2026, AdaCore
#

"""C library integration variants."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class AdaLibc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-c.ads",
            "hie/s-cerrno.ads",
            "hie/s-cerrno.adb",
            "hie/s-cmallo.ads",
            "hie/s-cmallo.adb",
            "hie/s-cstrle.ads",
            "hie/s-cstrle.adb",
        )


class Newlib(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/newlib-bb.c",
        )
