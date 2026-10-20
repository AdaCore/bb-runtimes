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
                    "s-gcc.ads",
                    "s-gcc-ti.ads",
                    "s-gcc-ti.adb",
                    "s-gtasl3.ads",
                    "s-gtasl3.adb",
                    "s-gtasr3.ads",
                    "s-gtasr3.adb",
                    "s-gtlsr3.ads",
                    "s-gtlsr3.adb",
                    "s-gtimo3.ads",
                    "s-gtimo3.adb",
                    "s-gtium3.ads",
                    "s-gtium3.adb",
                    "s-gcdimo.ads",
                    "s-gcdimo.adb",
                    "s-gudmo4.ads",
                    "s-gudmo4.adb",
                    "s-gtdmo3.ads",
                    "s-gtiud3.ads",
                    "s-gtiud3.adb",
                    "s-gtidi3.ads",
                    "s-gtidi3.adb",
                    "s-gtumo4.ads",
                )
