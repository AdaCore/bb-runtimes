#
# Copyright (C) 2026, AdaCore
#

"""VxWorks 7 cert GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlVx7r2Cert(SourcesAndFlagsComponent):
    """The VxWorks extension sources, per kernel-vs-RTP mode."""

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "vx7r2cert":
            raise IncompatibleComponent()

        match target.platform_variant:
            case "rtp":
                self.add_gnarl_sources(
                    "hie/s-vxwext__light-tasking-rtp.ads",
                    "hie/s-vxwext__light-tasking-rtp.adb",
                )
            case "kernel":
                self.add_gnarl_sources(
                    "hie/s-vxwext__light-tasking.ads",
                    "hie/s-vxwext__light-tasking.adb",
                    "ivec.c",
                )
            case _:
                raise ValueError(
                    f"Unknown vx7r2cert variant: {target.platform_variant}"
                )
