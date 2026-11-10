#
# Copyright (C) 2026, AdaCore
#

"""GNARL tasking-profile layers (sfp vs full)."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlSfp(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "s-reldel.ads",
            "s-reldel.adb",
            "s-taskin__light-tasking.ads",
            "hie/s-tasque.ads",
            "hie/s-tasque.adb",
            "s-tpoben__bb.ads",
            "s-tpoben__bb.adb",
            "s-tposen__raven.ads",
            "s-tposen__raven.adb",
            "s-tpobop__light-tasking.ads",
            "s-tpobop__light-tasking.adb",
            "a-synbar.adb",
            "a-synbar.ads",
            "s-restri.ads",
            "s-restri.adb",
            "s-rident.ads",
        )


class GnarlFull(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnarl_sources(
            "s-taskin__full.ads",
            "s-tposen__xi-full.adb",
            "s-tposen__xi-full.ads",
            "s-putaim.adb",
            "s-putaim.ads",
        )
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "s-btstch.ads",
                    "s-btstch.adb",
                )


class GnarlFullExtended(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "s-reldel.ads",
            "s-reldel.adb",
            "a-synbar.adb",
            "a-synbar.ads",
            "g-boubuf.adb",
            "g-boubuf.ads",
            "g-boumai.ads",
            "g-semaph.adb",
            "g-semaph.ads",
            "hie/s-tasque.ads",
            "hie/s-tasque.adb",
            "s-tpoben__bb.ads",
            "s-tpoben__bb.adb",
            "s-tpobop__bb.ads",
            "s-tpobop__bb.adb",
        )
