#
# Copyright (C) 2026, AdaCore
#

"""Ada.Numerics elementary functions core."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Math(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "a-nselfu.ads",
            "a-numaux__ada.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "a-ngelfu__ada.ads",
                    "a-ngelfu__ada.adb",
                    "a-nlelfu__ada.ads",
                    "a-nuelfu__ada.ads",
                    "s-gcmain__ada.ads",
                    "s-gcmain__ada.adb",
                    "s-libdou__ada.ads",
                    "s-libdou__ada.adb",
                    "s-libsin__ada.ads",
                    "s-libsin__ada.adb",
                    "s-lidosq__ada.ads",
                    "s-lisisq__ada.ads",
                    "s-libm__ada.ads",
                    "s-libm__ada.adb",
                    "s-libpre__ada.ads",
                )
            case "deos":
                self.add_gnat_sources(
                    "a-ngelfu__cert.ads",
                    "a-ngelfu__cert.adb",
                    "a-nlelfu__cert.ads",
                    "a-nuelfu__cert.ads",
                    "s-gcmain__cert.ads",
                    "s-gcmain__cert.adb",
                    "a-nllefu__cert.ads",
                    "a-nuaufl__light.ads",
                    "a-nalofl__light.ads",
                    "a-nallfl__light.ads",
                    "a-naliop__nolibm.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "a-ngelfu__ada.ads",
                    "a-ngelfu__ada.adb",
                    "a-nlelfu__ada.ads",
                    "a-nuelfu__ada.ads",
                    "s-gcmain__ada.ads",
                    "s-gcmain__ada.adb",
                    "s-libdou__ada.ads",
                    "s-libdou__ada.adb",
                    "s-libsin__ada.ads",
                    "s-libsin__ada.adb",
                    "s-lidosq__ada.ads",
                    "s-lisisq__ada.ads",
                    "s-libm__ada.ads",
                    "s-libm__ada.adb",
                    "s-libpre__ada.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "a-ngelfu__cert.ads",
                    "a-ngelfu__cert.adb",
                    "a-nlelfu__cert.ads",
                    "a-nuelfu__cert.ads",
                    "s-gcmain__cert.ads",
                    "s-gcmain__cert.adb",
                    "a-nllefu__cert.ads",
                    "a-nuaufl__light-float.ads",
                    "a-nalofl__light.ads",
                    "a-naliop.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "a-ngelfu__cert.ads",
                    "a-ngelfu__cert.adb",
                    "a-nlelfu__cert.ads",
                    "a-nuelfu__cert.ads",
                    "s-gcmain__cert.ads",
                    "s-gcmain__cert.adb",
                    "a-nllefu__cert.ads",
                    "a-nuaufl__light.ads",
                    "a-nalofl__light.ads",
                    "a-nallfl__light.ads",
                    "a-naliop.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "a-ngelfu__ada.ads",
                    "a-ngelfu__ada.adb",
                    "a-nlelfu__ada.ads",
                    "a-nuelfu__ada.ads",
                    "s-gcmain__ada.ads",
                    "s-gcmain__ada.adb",
                    "s-libdou__ada.ads",
                    "s-libdou__ada.adb",
                    "s-libsin__ada.ads",
                    "s-libsin__ada.adb",
                    "s-lidosq__ada.ads",
                    "s-lisisq__ada.ads",
                    "s-libm__ada.ads",
                    "s-libm__ada.adb",
                    "s-libpre__ada.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "a-ngelfu__cert.ads",
                    "a-ngelfu__cert.adb",
                    "a-nlelfu__cert.ads",
                    "a-nuelfu__cert.ads",
                    "s-gcmain__cert.ads",
                    "s-gcmain__cert.adb",
                    "a-nllefu__cert.ads",
                    "a-nuaufl__light.ads",
                    "a-nalofl__light.ads",
                    "a-nallfl__light.ads",
                    "a-naliop.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "a-ngelfu__cert.ads",
                    "a-ngelfu__cert.adb",
                    "a-nlelfu__cert.ads",
                    "a-nuelfu__cert.ads",
                    "s-gcmain__cert.ads",
                    "s-gcmain__cert.adb",
                    "a-nllefu__cert.ads",
                    "a-nuaufl__light.ads",
                    "a-nalofl__light.ads",
                    "a-nallfl__light.ads",
                    "a-naliop__nolibm.ads",
                )
