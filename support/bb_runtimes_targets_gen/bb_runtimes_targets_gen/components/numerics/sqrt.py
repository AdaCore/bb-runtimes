#
# Copyright (C) 2026, AdaCore
#

"""Square-root implementation variants (soft/hard, SP/DP)."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class MathSoftSP(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-lisisq__ada.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-lisisq__ada.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "s-lisisq__ada.adb",
                )


class MathSoftDP(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-lidosq__ada.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-lidosq__ada.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "s-lidosq__ada.adb",
                )


class MathHardSP(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-lisisq__fpu.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-lisisq__fpu.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "s-lisisq__fpu.adb",
                )


class MathHardDP(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-lidosq__fpu.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-lidosq__fpu.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "s-lidosq__fpu.adb",
                )
