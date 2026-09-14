#
# Copyright (C) 2026, AdaCore
#

"""Static constructors/destructors support."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class CtorsDtors(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-libc.ads",
            "hie/s-libc.adb",
        )


class CtorsDtorsC(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-liinfi__c.adb",
        )


class CtorsDtorsDefault(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "hie/s-liinfi.adb",
        )
