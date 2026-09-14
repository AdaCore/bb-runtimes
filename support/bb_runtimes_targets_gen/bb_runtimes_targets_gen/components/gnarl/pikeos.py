#
# Copyright (C) 2026, AdaCore
#

"""PikeOS GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlPikeos(SourcesAndFlagsComponent):
    """The PikeOS interrupt and OS-interface sources, per PikeOS version.

    PikeOS 4.2 and 5 install the same set: 4.2 is where p4ext appeared, and 5
    kept it.
    """

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "pikeos":
            raise IncompatibleComponent()

        match target.platform_variant:
            case "pikeos3":
                self.add_gnarl_sources(
                    "hie/s-interr__pikeos3.adb",
                    "hie/s-osinte__pikeos3.ads",
                    "hie/s-osinte__pikeos3.adb",
                )
            case "pikeos4":
                self.add_gnarl_sources(
                    "hie/s-interr__pikeos4.adb",
                    "hie/s-osinte__pikeos4.ads",
                    "hie/s-osinte__pikeos4.adb",
                )
            case "pikeos4.2" | "pikeos5":
                self.add_gnarl_sources(
                    "hie/s-interr__pikeos4.adb",
                    "hie/s-osinte__pikeos4.ads",
                    "hie/s-osinte__p4ext.adb",
                )
            case _:
                raise ValueError(f"Unknown PikeOS version: {target.platform_variant}")
