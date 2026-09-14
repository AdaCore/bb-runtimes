#
# Copyright (C) 2026, AdaCore
#

"""Certifiable Ada implementation of libgcc intrinsics."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Libgcc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-gcc.ads",
            "hie/s-gcc-di.ads",
            "hie/s-gcc-di.adb",
            "hie/s-gcdidi.ads",
            "hie/s-gcdidi.adb",
        )


class LibgccArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-gcdifl__arm.ads",
            "hie/s-gcdifl__arm.adb",
            "hie/s-gcdish__arm.ads",
            "hie/s-gcdish.adb",
        )


class LibgccOtherArchs(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-gcdifl.ads",
            "hie/s-gcdifl.adb",
            "hie/s-gcdish.ads",
            "hie/s-gcdish.adb",
        )
