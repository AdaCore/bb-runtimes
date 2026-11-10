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
            "a-chahan__light.ads",
            "a-chahan__light.adb",
            "a-strsup__light.ads",
            "a-strsup__light.adb",
            "a-tags__hie.ads",
            "a-tags__hie.adb",
            "i-c__light.ads",
            "i-c__light.adb",
            "s-assert__xi.adb",
            "s-memory__light.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "a-elchha__zfp.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "a-elchha__zfp.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "a-elchha__zfp.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "a-elchha__traceback.adb",
                    "s-traent.ads",
                    "s-traent.adb",
                    "s-traceb__cert.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "a-elchha__zfp.adb",
                )
            case "qnx":
                self.add_gnat_sources(
                    "a-elchha__traceback.adb",
                    "s-traent.ads",
                    "s-traent.adb",
                    "s-traceb__cert.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "a-elchha__traceback.adb",
                    "s-traent.ads",
                    "s-traent.adb",
                    "s-traceb__cert.ads",
                    "s-traceb__zfp.adb",
                    "s-tracon__zfp.ads",
                )


class LightExceptions(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "a-elchha__zfp.ads",
            "a-except__zfp.ads",
            "a-except__zfp.adb",
        )


class LightNonTasking(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-sssita.ads",
                    "s-sssita.adb",
                )
            case "deos":
                self.add_gnat_sources(
                    "s-thread__cert.ads",
                    "s-thread__cert.adb",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-sssita.ads",
                    "s-sssita.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "s-sssita.ads",
                    "s-sssita.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "s-sssita.ads",
                    "s-sssita.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "a-textio__pikeos-light.adb",
                    "s-sssita.ads",
                    "s-sssita.adb",
                )
            case "qnx":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                    "s-thread__light.ads",
                    "s-thread__light.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-parame__zfp_huge.ads",
                    "s-thread__light.ads",
                    "s-thread__light.adb",
                )


class LightTasking(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "s-sssita.ads",
            "s-sssita.adb",
        )
        match platform:
            case "linux":
                self.add_gnat_sources(
                    "s-parame.ads",
                    "s-init__light.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "s-parame.ads",
                    "libgnat/s-parame.adb",
                    "s-init__light.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "s-parame.ads",
                    "s-parame__qnx.adb",
                    "s-init__light.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "s-parame__vxworks7cert.ads",
                    "s-parame__vxworks.adb",
                    "s-init__light.ads",
                )
