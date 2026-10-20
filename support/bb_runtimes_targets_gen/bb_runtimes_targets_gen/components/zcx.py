#
# Copyright (C) 2026, AdaCore
#

"""Zero-cost exception propagation support, per unwinder flavor."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class FullZcxArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-excmac__arm.adb",
            "s-excmac__arm.ads",
            "s-traceb__armeabi.adb",
        )


class FullZcxDw2(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-excmac__gcc.ads",
            "s-excmac__gcc.adb",
        )


class FullZcxDw2Gcc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "libgcc/unwind-dw2-fde.h",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "unwind-dw2-fde-bb.c",
                )


class FullZcxAarch64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__dwarf.adb",
        )


class FullZcxPpc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__ppc.adb",
        )


class FullZcxRiscv(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__dwarf.adb",
        )


class FullZcxLeon(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__sparc.adb",
        )


class FullZcxX86(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__vx653-sim.adb",
        )


class FullZcxX8664(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-traceb__dwarf.adb",
        )
