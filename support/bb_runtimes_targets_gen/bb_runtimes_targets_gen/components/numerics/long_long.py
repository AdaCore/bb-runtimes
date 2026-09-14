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
                    "libgnat/a-nllefu.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "libgnat/a-nllefu.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-nallfl__light.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "libgnat/a-nllefu.ads",
                )


class MathLongLong80bit(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/s-lilodo__ada.ads",
                    "hie/s-lilodo__ada.adb",
                    "hie/s-lldosq__ada.ads",
                    "hie/s-lldosq__fpu.adb",
                    "hie/a-nllefu__ada.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-nallfl__light-long-double.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/s-lilodo__ada.ads",
                    "hie/s-lilodo__ada.adb",
                    "hie/s-lldosq__ada.ads",
                    "hie/s-lldosq__fpu.adb",
                    "hie/a-nllefu__ada.ads",
                )
