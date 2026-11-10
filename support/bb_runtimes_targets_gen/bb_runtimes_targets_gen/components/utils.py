#
# Copyright (C) 2026, AdaCore
#

"""Shared attribute-support utilities (float core, case folding)."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class UtilsFloat(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-flocon.ads",
            "s-flocon__none.adb",
            "s-dourea.ads",
            "s-dourea.adb",
            "s-dorepr.adb",
            "s-powflt.ads",
            "s-powlfl.ads",
            "s-powllf.ads",
        )


class UtilsFloatFma(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-flocon.ads",
            "s-flocon__none.adb",
            "s-dourea.ads",
            "s-dourea.adb",
            "s-dorepr__fma.adb",
            "s-powflt.ads",
            "s-powlfl.ads",
            "s-powllf.ads",
        )


class UtilsCaseUtil(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-casuti.ads",
            "s-casuti.adb",
        )


class UtilsCaseUtilNss(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "s-cautns.ads",
            "s-cautns.adb",
        )
