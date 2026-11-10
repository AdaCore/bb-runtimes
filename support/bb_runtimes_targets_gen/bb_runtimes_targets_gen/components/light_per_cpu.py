#
# Copyright (C) 2026, AdaCore
#

"""Per-CPU light profile components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class LightArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "qnx":
                self.add_gnat_sources(
                    "s-traceb__dummy.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-arm.adb",
                )


class LightAarch64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "lynx":
                self.add_gnat_sources(
                    "s-traceb__zfp.adb",
                    "s-tracon__zfp.ads",
                    "s-tracon__zfp-aarch64.adb",
                )
            case "qnx":
                self.add_gnat_sources(
                    "s-traceb__zfp.adb",
                    "s-tracon__zfp.ads",
                    "s-tracon__zfp-aarch64.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-aarch64.adb",
                )


class LightPpc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-ppc.adb",
                )


class LightPpc64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-ppc64.adb",
                )


class LightX86(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-x86.adb",
                )


class LightX8664(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-tracon__zfp-x86_64.adb",
                )
