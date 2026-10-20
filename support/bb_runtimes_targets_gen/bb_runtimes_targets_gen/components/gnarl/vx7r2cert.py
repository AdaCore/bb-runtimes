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
    """The VxWorks GNARL sources, per kernel-vs-RTP mode."""

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "vx7r2cert":
            raise IncompatibleComponent()

        self.add_gnarl_sources(
            "libgnarl/a-reatim.ads",
            "libgnarl/a-reatim.adb",
            "s-taprop__raven-cert.ads",
            "s-taprop__vxworks7cert.adb",
            "s-tpopsp__vxworks7-cert-tls.adb",
            "s-taspri__raven-vxworks.ads",
            "s-multip__raven-default.ads",
            "s-musplo.adb",
            "s-osinte__vxworks-light-tasking.ads",
            "s-osinte__vxworks-light-tasking.adb",
            "libgnarl/s-tpobmu.adb",
            "clexco.c",
            "initflt.c",
            "sysdep-raven-cert-vxworks.c",
        )

        match target.platform_variant:
            case "rtp":
                self.add_gnarl_sources(
                    "s-vxwext__light-tasking-rtp.ads",
                    "s-vxwext__light-tasking-rtp.adb",
                )
            case "kernel":
                self.add_gnarl_sources(
                    "s-vxwext__light-tasking.ads",
                    "s-vxwext__light-tasking.adb",
                    "ivec.c",
                )
            case _:
                raise ValueError(
                    f"Unknown vx7r2cert variant: {target.platform_variant}"
                )
