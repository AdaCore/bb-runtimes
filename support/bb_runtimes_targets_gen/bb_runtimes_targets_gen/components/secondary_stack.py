#
# Copyright (C) 2026, AdaCore
#

"""Secondary stack support variants."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class SecondaryStackSymbol(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-secsta__zfp.ads",
            "hie/s-secsta__zfp.adb",
        )


class SecondaryStackSymbolCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-secsta__zfp.ads",
            "hie/s-secsta__cheri.adb",
        )


class SecondaryStackSoftlinks(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-secsta__cert.ads",
            "hie/s-secsta__cert.adb",
        )
