#
# Copyright (C) 2026, AdaCore
#

"""Linux GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlLinux(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "linux":
            raise IncompatibleComponent()

        self.add_gnarl_sources(
            "libgnarl/a-reatim.ads",
            "libgnarl/a-reatim.adb",
            "s-multip__raven-default.ads",
            "s-taspri__posix.ads",
            "s-osinte__linux-light-tasking.ads",
            "s-osinte__linux-light-tasking.adb",
            "s-taprop__raven-cert.ads",
            "s-taprop__linux-light-tasking.adb",
            "s-tpopsp__posix-raven-cert.adb",
            "s-musplo.adb",
            "libgnarl/s-tpobmu.adb",
            "s-linux.ads",
            "s-c_time.ads",
            "s-c_time.adb",
            "s-oslock__posix.ads",
        )
