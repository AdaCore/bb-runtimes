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
            "s-gcc.ads",
            "s-gcc-di.ads",
            "s-gcc-di.adb",
            "s-gcdidi.ads",
            "s-gcdidi.adb",
        )


class LibgccArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-gcdifl__arm.ads",
            "s-gcdifl__arm.adb",
            "s-gcdish__arm.ads",
            "s-gcdish.adb",
        )


class LibgccOtherArchs(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-gcdifl.ads",
            "s-gcdifl.adb",
            "s-gcdish.ads",
            "s-gcdish.adb",
        )
