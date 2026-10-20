#
# Copyright (C) 2026, AdaCore
#

"""Long Long Float bindings for the math library."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class MathLongLong64bit(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "a-nllefu.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "a-nllefu.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "a-nallfl__light.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "a-nllefu.ads",
                )


class MathLongLong80bit(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-lilodo__ada.ads",
                    "s-lilodo__ada.adb",
                    "s-lldosq__ada.ads",
                    "s-lldosq__fpu.adb",
                    "a-nllefu__ada.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "a-nallfl__light-long-double.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "s-lilodo__ada.ads",
                    "s-lilodo__ada.adb",
                    "s-lldosq__ada.ads",
                    "s-lldosq__fpu.adb",
                    "a-nllefu__ada.ads",
                )
