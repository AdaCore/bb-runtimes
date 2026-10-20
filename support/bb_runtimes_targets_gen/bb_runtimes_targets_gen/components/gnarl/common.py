#
# Copyright (C) 2026, AdaCore
#

"""GNARL core sources shared by every tasking profile."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlCommon(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "a-retide.ads",
            "a-retide__raven.adb",
            "a-sytaco__xi.ads",
            "a-sytaco__xi.adb",
            "a-taside.ads",
            "a-taside__raven.adb",
            "hie/a-taster.ads",
            "hie/a-taster.adb",
            "s-mufalo.ads",
            "s-mufalo.adb",
            "s-musplo.ads",
            "s-taprob__raven.ads",
            "s-taprob__raven.adb",
            "s-tarest.ads",
            "hie/s-tarest.adb",
            "s-tasdeb__xi.ads",
            "s-tasdeb__raven.adb",
            "s-tasinf.ads",
            "s-tasinf.adb",
            "s-tasres.ads",
            "s-tpobmu.ads",
        )


class GnarlCommonNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "hie/s-taskin.adb",
        )


class GnarlCommonCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "s-taskin__cheri.adb",
        )
