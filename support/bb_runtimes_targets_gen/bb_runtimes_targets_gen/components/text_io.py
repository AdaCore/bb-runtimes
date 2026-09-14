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
                    "hie/a-textio__bb.adb",
                )


class Semihosting(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/s-semiho.ads",
                    "hie/s-semiho.adb",
                    "hie/s-textio__semihosting.adb",
                    "hie/a-textio__semihosting.adb",
                )
