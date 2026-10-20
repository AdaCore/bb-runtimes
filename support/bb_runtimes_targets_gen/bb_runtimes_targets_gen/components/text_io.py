#
# Copyright (C) 2026, AdaCore
#

"""Text_IO channel variants."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class SystemIO(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "a-textio__bb.adb",
                )


class Semihosting(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "s-semiho.ads",
                    "s-semiho.adb",
                    "s-textio__semihosting.adb",
                    "a-textio__semihosting.adb",
                )
