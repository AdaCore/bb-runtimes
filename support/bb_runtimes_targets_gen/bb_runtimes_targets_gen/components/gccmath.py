#
# Copyright (C) 2026, AdaCore
#

"""GCC soft-float math bindings."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GccMath(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnat_sources(
                    "hie/s-gcc.ads",
                    "hie/s-gcc-ti.ads",
                    "hie/s-gcc-ti.adb",
                    "hie/s-gtasl3.ads",
                    "hie/s-gtasl3.adb",
                    "hie/s-gtasr3.ads",
                    "hie/s-gtasr3.adb",
                    "hie/s-gtlsr3.ads",
                    "hie/s-gtlsr3.adb",
                    "hie/s-gtimo3.ads",
                    "hie/s-gtimo3.adb",
                    "hie/s-gtium3.ads",
                    "hie/s-gtium3.adb",
                    "hie/s-gcdimo.ads",
                    "hie/s-gcdimo.adb",
                    "hie/s-gudmo4.ads",
                    "hie/s-gudmo4.adb",
                    "hie/s-gtdmo3.ads",
                    "hie/s-gtiud3.ads",
                    "hie/s-gtiud3.adb",
                    "hie/s-gtidi3.ads",
                    "hie/s-gtidi3.adb",
                    "hie/s-gtumo4.ads",
                )
