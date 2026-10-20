#
# Copyright (C) 2026, AdaCore
#

"""Spinlock implementation GNARL components."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlSpinlockGcc(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "s-musplo.adb",
                )


class GnarlSpinlockLeon(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "s-musplo__leon.adb",
                )
