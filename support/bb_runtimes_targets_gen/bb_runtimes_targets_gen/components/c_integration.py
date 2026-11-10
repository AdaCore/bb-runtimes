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
            "s-c.ads",
            "s-cerrno.ads",
            "s-cerrno.adb",
            "s-cmallo.ads",
            "s-cmallo.adb",
            "s-cstrle.ads",
            "s-cstrle.adb",
        )


class Newlib(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "newlib-bb.c",
        )
