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
            "a-calclo__deos.adb",
            "hie/a-calend.ads",
            "hie/a-calend.adb",
            "a-elchha.ads",
            "a-elchha__deos.adb",
            "a-except__cert.ads",
            "a-except__cert.adb",
            "a-exctra.ads",
            "a-exctra.adb",
            "calendar.ads",
            "g-io__c.ads",
            "s-except__cert.ads",
            "a-excach__cert.adb",
            "s-excmac__gcc.adb",
            "s-excmac__gcc.ads",
            "s-init.ads",
            "s-parame__cert.adb",
            "s-soflin__cert.ads",
            "s-soflin__cert.adb",
            "s-stalib__raven.ads",
            "s-stalib__raven.adb",
            "s-traent.ads",
            "s-traent.adb",
            "s-traceb__cert.ads",
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
                    "s-init__deos.adb",
                    "s-parame__deos.ads",
                    "s-traceb__zfp.adb",
                    "s-tracon__zfp.ads",
                )


class CertArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-tracon__zfp-arm.adb",
        )
