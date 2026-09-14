#
# Copyright (C) 2026, AdaCore
#

"""Per-CPU GNARL components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlAarch64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "linux":
                self.add_gnarl_sources(
                    "hie/s-oscons__linux-aarch64.ads",
                    "libgnat/s-parame__aarch64-linux.adb",
                )
            case "qnx":
                self.add_gnarl_sources(
                    "hie/s-oscons__qnx-aarch64.ads",
                )
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/s-vxwork__aarch64.ads",
                )


class GnarlArm(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/s-vxwork__arm.ads",
                )


class GnarlPpc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/s-vxwork__ppc.ads",
                )


class GnarlX86(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "linux":
                self.add_gnarl_sources(
                    "hie/s-oscons__linux-x86.ads",
                    "libgnat/s-parame.adb",
                )
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/s-vxwork__x86.ads",
                )


class GnarlX8664(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "linux":
                self.add_gnarl_sources(
                    "hie/s-oscons__linux-x86_64.ads",
                    "libgnat/s-parame.adb",
                )
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/s-vxwork__x86.ads",
                )
