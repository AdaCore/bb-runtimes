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
            "libgnat/s-flocon.ads",
            "libgnat/s-flocon__none.adb",
            "libgnat/s-dourea.ads",
            "libgnat/s-dourea.adb",
            "libgnat/s-dorepr.adb",
            "libgnat/s-powflt.ads",
            "libgnat/s-powlfl.ads",
            "libgnat/s-powllf.ads",
        )


class UtilsFloatFma(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-flocon.ads",
            "libgnat/s-flocon__none.adb",
            "libgnat/s-dourea.ads",
            "libgnat/s-dourea.adb",
            "libgnat/s-dorepr__fma.adb",
            "libgnat/s-powflt.ads",
            "libgnat/s-powlfl.ads",
            "libgnat/s-powllf.ads",
        )


class UtilsCaseUtil(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-casuti.ads",
            "libgnat/s-casuti.adb",
        )


class UtilsCaseUtilNss(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/s-cautns.ads",
            "libgnat/s-cautns.adb",
        )
