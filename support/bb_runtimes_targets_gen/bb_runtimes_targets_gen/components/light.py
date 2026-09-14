#
# Copyright (C) 2026, AdaCore
#

"""Light profile family core components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Light(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "hie/a-chahan__light.ads",
            "hie/a-chahan__light.adb",
            "hie/a-strsup__light.ads",
            "hie/a-strsup__light.adb",
            "hie/a-tags__hie.ads",
            "hie/a-tags__hie.adb",
            "hie/i-c__light.ads",
            "hie/i-c__light.adb",
            "hie/s-assert__xi.adb",
            "hie/s-memory__light.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/a-elchha__zfp.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "hie/a-elchha__zfp.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-elchha__zfp.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "hie/a-elchha__traceback.adb",
                    "libgnat/s-traent.ads",
                    "libgnat/s-traent.adb",
                    "hie/s-traceb__cert.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/a-elchha__zfp.adb",
                )
            case "qnx":
                self.add_gnat_sources(
                    "hie/a-elchha__traceback.adb",
                    "libgnat/s-traent.ads",
                    "libgnat/s-traent.adb",
                    "hie/s-traceb__cert.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "hie/a-elchha__traceback.adb",
                    "libgnat/s-traent.ads",
                    "libgnat/s-traent.adb",
                    "hie/s-traceb__cert.ads",
                    "hie/s-traceb__zfp.adb",
                    "hie/s-tracon__zfp.ads",
                )


class LightExceptions(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/a-elchha__zfp.ads",
            "hie/a-except__zfp.ads",
            "hie/a-except__zfp.adb",
        )


class LightNonTasking(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/s-sssita.ads",
                    "hie/s-sssita.adb",
                )
            case "deos":
                self.add_gnat_sources(
                    "hie/s-thread__cert.ads",
                    "hie/s-thread__cert.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "hie/s-sssita.ads",
                    "hie/s-sssita.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/s-sssita.ads",
                    "hie/s-sssita.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "hie/s-sssita.ads",
                    "hie/s-sssita.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/a-textio__pikeos-light.adb",
                    "hie/s-sssita.ads",
                    "hie/s-sssita.adb",
                )
            case "qnx":
                self.add_gnat_sources(
                    "hie/s-parame__zfp_huge.ads",
                    "hie/s-thread__light.ads",
                    "hie/s-thread__light.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "hie/s-parame__zfp_huge.ads",
                    "hie/s-thread__light.ads",
                    "hie/s-thread__light.adb",
                )


class LightTasking(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "hie/s-sssita.ads",
            "hie/s-sssita.adb",
        )
        match platform:
            case "linux":
                self.add_gnat_sources(
                    "libgnat/s-parame.ads",
                    "hie/s-init__light.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "libgnat/s-parame.ads",
                    "libgnat/s-parame.adb",
                    "hie/s-init__light.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "libgnat/s-parame.ads",
                    "libgnat/s-parame__qnx.adb",
                    "hie/s-init__light.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "hie/s-parame__vxworks7cert.ads",
                    "libgnat/s-parame__vxworks.adb",
                    "hie/s-init__light.ads",
                )
