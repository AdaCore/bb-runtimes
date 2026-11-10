#
# Copyright (C) 2026, AdaCore
#

"""Hardware timer width GNARL components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlTimer32(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "s-bbbosu__timer-32.ads",
                    "s-bbtime__timer-32.adb",
                )


class GnarlTimer64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "s-bbbosu__timer-64.ads",
                    "s-bbtime__timer-64.adb",
                )
