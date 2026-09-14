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
            "libgnat/a-nselfu.ads",
            "hie/a-numaux__ada.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/a-ngelfu__ada.ads",
                    "hie/a-ngelfu__ada.adb",
                    "hie/a-nlelfu__ada.ads",
                    "hie/a-nuelfu__ada.ads",
                    "hie/s-gcmain__ada.ads",
                    "hie/s-gcmain__ada.adb",
                    "hie/s-libdou__ada.ads",
                    "hie/s-libdou__ada.adb",
                    "hie/s-libsin__ada.ads",
                    "hie/s-libsin__ada.adb",
                    "hie/s-lidosq__ada.ads",
                    "hie/s-lisisq__ada.ads",
                    "hie/s-libm__ada.ads",
                    "hie/s-libm__ada.adb",
                    "hie/s-libpre__ada.ads",
                )
            case "deos":
                self.add_gnat_sources(
                    "hie/a-ngelfu__cert.ads",
                    "hie/a-ngelfu__cert.adb",
                    "hie/a-nlelfu__cert.ads",
                    "hie/a-nuelfu__cert.ads",
                    "hie/s-gcmain__cert.ads",
                    "hie/s-gcmain__cert.adb",
                    "hie/a-nllefu__cert.ads",
                    "hie/a-nuaufl__light.ads",
                    "hie/a-nalofl__light.ads",
                    "hie/a-nallfl__light.ads",
                    "libgnat/a-naliop__nolibm.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "hie/a-ngelfu__ada.ads",
                    "hie/a-ngelfu__ada.adb",
                    "hie/a-nlelfu__ada.ads",
                    "hie/a-nuelfu__ada.ads",
                    "hie/s-gcmain__ada.ads",
                    "hie/s-gcmain__ada.adb",
                    "hie/s-libdou__ada.ads",
                    "hie/s-libdou__ada.adb",
                    "hie/s-libsin__ada.ads",
                    "hie/s-libsin__ada.adb",
                    "hie/s-lidosq__ada.ads",
                    "hie/s-lisisq__ada.ads",
                    "hie/s-libm__ada.ads",
                    "hie/s-libm__ada.adb",
                    "hie/s-libpre__ada.ads",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-ngelfu__cert.ads",
                    "hie/a-ngelfu__cert.adb",
                    "hie/a-nlelfu__cert.ads",
                    "hie/a-nuelfu__cert.ads",
                    "hie/s-gcmain__cert.ads",
                    "hie/s-gcmain__cert.adb",
                    "hie/a-nllefu__cert.ads",
                    "hie/a-nuaufl__light-float.ads",
                    "hie/a-nalofl__light.ads",
                    "libgnat/a-naliop.ads",
                )
            case "lynx":
                self.add_gnat_sources(
                    "hie/a-ngelfu__cert.ads",
                    "hie/a-ngelfu__cert.adb",
                    "hie/a-nlelfu__cert.ads",
                    "hie/a-nuelfu__cert.ads",
                    "hie/s-gcmain__cert.ads",
                    "hie/s-gcmain__cert.adb",
                    "hie/a-nllefu__cert.ads",
                    "hie/a-nuaufl__light.ads",
                    "hie/a-nalofl__light.ads",
                    "hie/a-nallfl__light.ads",
                    "libgnat/a-naliop.ads",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/a-ngelfu__ada.ads",
                    "hie/a-ngelfu__ada.adb",
                    "hie/a-nlelfu__ada.ads",
                    "hie/a-nuelfu__ada.ads",
                    "hie/s-gcmain__ada.ads",
                    "hie/s-gcmain__ada.adb",
                    "hie/s-libdou__ada.ads",
                    "hie/s-libdou__ada.adb",
                    "hie/s-libsin__ada.ads",
                    "hie/s-libsin__ada.adb",
                    "hie/s-lidosq__ada.ads",
                    "hie/s-lisisq__ada.ads",
                    "hie/s-libm__ada.ads",
                    "hie/s-libm__ada.adb",
                    "hie/s-libpre__ada.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "hie/a-ngelfu__cert.ads",
                    "hie/a-ngelfu__cert.adb",
                    "hie/a-nlelfu__cert.ads",
                    "hie/a-nuelfu__cert.ads",
                    "hie/s-gcmain__cert.ads",
                    "hie/s-gcmain__cert.adb",
                    "hie/a-nllefu__cert.ads",
                    "hie/a-nuaufl__light.ads",
                    "hie/a-nalofl__light.ads",
                    "hie/a-nallfl__light.ads",
                    "libgnat/a-naliop.ads",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "hie/a-ngelfu__cert.ads",
                    "hie/a-ngelfu__cert.adb",
                    "hie/a-nlelfu__cert.ads",
                    "hie/a-nuelfu__cert.ads",
                    "hie/s-gcmain__cert.ads",
                    "hie/s-gcmain__cert.adb",
                    "hie/a-nllefu__cert.ads",
                    "hie/a-nuaufl__light.ads",
                    "hie/a-nalofl__light.ads",
                    "hie/a-nallfl__light.ads",
                    "libgnat/a-naliop__nolibm.ads",
                )
