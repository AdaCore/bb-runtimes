#
# Copyright (C) 2026, AdaCore
#

"""Exponentiation ("**") support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class ExponentInt(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-exponn.ads",
            "s-exponn.adb",
            "s-expont.ads",
            "s-expont.adb",
            "s-exponu.ads",
            "s-exponu.adb",
            "s-exnint.ads",
            "s-expint.ads",
            "s-expuns.ads",
        )


class ExponentIntLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-exnlli.ads",
            "s-explli.ads",
            "s-expllu.ads",
        )


class ExponentIntLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-exnllli.ads",
            "s-expllli.ads",
            "s-explllu.ads",
        )


class ExponentMod(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-expmod.ads",
            "s-expmod.adb",
        )


class ExponentFloat(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-exponr.ads",
            "s-exponr.adb",
            "s-exnflt.ads",
            "s-exnlfl.ads",
            "s-exnllf.ads",
        )
