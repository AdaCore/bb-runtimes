#
# Copyright (C) 2026, AdaCore
#

"""System.Parameters variants (memory profile sizing)."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ParametersSmall(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__small.ads",
                    "hie/s-parame.adb",
                )


class ParametersLarge(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__large.ads",
                    "hie/s-parame.adb",
                )


class ParametersHuge(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__huge.ads",
                    "hie/s-parame.adb",
                )


class ParametersLightSmall(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__zfp_small.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-parame__zfp_small.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "s-parame__zfp_small.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "s-parame__zfp_small.ads",
                )


class ParametersLightLarge(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__zfp.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-parame__zfp.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "s-parame__zfp.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "s-parame__zfp.ads",
                )


class ParametersLightHuge(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                )
