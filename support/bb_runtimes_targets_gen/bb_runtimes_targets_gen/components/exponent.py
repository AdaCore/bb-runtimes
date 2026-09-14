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
            "libgnat/s-exponn.ads",
            "libgnat/s-exponn.adb",
            "libgnat/s-expont.ads",
            "libgnat/s-expont.adb",
            "libgnat/s-exponu.ads",
            "libgnat/s-exponu.adb",
            "libgnat/s-exnint.ads",
            "libgnat/s-expint.ads",
            "libgnat/s-expuns.ads",
        )


class ExponentIntLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-exnlli.ads",
            "libgnat/s-explli.ads",
            "libgnat/s-expllu.ads",
        )


class ExponentIntLLL(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-exnllli.ads",
            "libgnat/s-expllli.ads",
            "libgnat/s-explllu.ads",
        )


class ExponentMod(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-expmod.ads",
            "libgnat/s-expmod.adb",
        )


class ExponentFloat(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-exponr.ads",
            "libgnat/s-exponr.adb",
            "libgnat/s-exnflt.ads",
            "libgnat/s-exnlfl.ads",
            "libgnat/s-exnllf.ads",
        )
