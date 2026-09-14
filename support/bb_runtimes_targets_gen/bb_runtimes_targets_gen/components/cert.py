#
# Copyright (C) 2026, AdaCore
#

"""Cert profile core components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Cert(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "hie/a-calclo__deos.adb",
            "hie/a-calend.ads",
            "hie/a-calend.adb",
            "libgnat/a-elchha.ads",
            "hie/a-elchha__deos.adb",
            "hie/a-except__cert.ads",
            "hie/a-except__cert.adb",
            "libgnat/a-exctra.ads",
            "libgnat/a-exctra.adb",
            "libgnat/calendar.ads",
            "hie/g-io__c.ads",
            "hie/s-except__cert.ads",
            "hie/a-excach__cert.adb",
            "libgnat/s-excmac__gcc.adb",
            "libgnat/s-excmac__gcc.ads",
            "hie/s-init.ads",
            "hie/s-parame__cert.adb",
            "hie/s-soflin__cert.ads",
            "hie/s-soflin__cert.adb",
            "hie/s-stalib__raven.ads",
            "hie/s-stalib__raven.adb",
            "libgnat/s-traent.ads",
            "libgnat/s-traent.adb",
            "hie/s-traceb__cert.ads",
            "libgcc/unwind-generic.h",
            "libgcc/unwind.inc",
            "libgcc/unwind-pe.h",
            "raise-gcc.c",
            "raise.h",
            "runtime.h",
            "unwind-sjlj-cert.c",
        )
        match platform:
            case "deos":
                self.add_gnat_sources(
                    "hie/s-init__deos.adb",
                    "hie/s-parame__deos.ads",
                    "hie/s-traceb__zfp.adb",
                    "hie/s-tracon__zfp.ads",
                )


class CertArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-tracon__zfp-arm.adb",
        )
