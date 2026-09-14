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
            "hie/s-reldel.ads",
            "hie/s-reldel.adb",
            "hie/s-taskin__light-tasking.ads",
            "hie/s-tasque.ads",
            "hie/s-tasque.adb",
            "hie/s-tpoben__bb.ads",
            "hie/s-tpoben__bb.adb",
            "hie/s-tposen__raven.ads",
            "hie/s-tposen__raven.adb",
            "hie/s-tpobop__light-tasking.ads",
            "hie/s-tpobop__light-tasking.adb",
            "libgnarl/a-synbar.adb",
            "libgnarl/a-synbar.ads",
            "libgnat/s-restri.ads",
            "libgnat/s-restri.adb",
            "libgnat/s-rident.ads",
        )


class GnarlFull(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnarl_sources(
            "hie/s-taskin__full.ads",
            "hie/s-tposen__xi-full.adb",
            "hie/s-tposen__xi-full.ads",
            "libgnarl/s-putaim.adb",
            "libgnarl/s-putaim.ads",
        )
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "hie/s-btstch.ads",
                    "hie/s-btstch.adb",
                )


class GnarlFullExtended(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "hie/s-reldel.ads",
            "hie/s-reldel.adb",
            "libgnarl/a-synbar.adb",
            "libgnarl/a-synbar.ads",
            "libgnarl/g-boubuf.adb",
            "libgnarl/g-boubuf.ads",
            "libgnarl/g-boumai.ads",
            "libgnarl/g-semaph.adb",
            "libgnarl/g-semaph.ads",
            "hie/s-tasque.ads",
            "hie/s-tasque.adb",
            "hie/s-tpoben__bb.ads",
            "hie/s-tpoben__bb.adb",
            "hie/s-tpobop__bb.ads",
            "hie/s-tpobop__bb.adb",
        )
