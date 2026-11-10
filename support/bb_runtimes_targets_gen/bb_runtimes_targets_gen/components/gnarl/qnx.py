#
# Copyright (C) 2026, AdaCore
#

"""QNX GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlQnx(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "qnx":
            raise IncompatibleComponent()

        self.add_gnarl_sources(
            "libgnarl/a-reatim.ads",
            "libgnarl/a-reatim.adb",
            "s-multip__raven-default.ads",
            "s-taspri__posix.ads",
            "s-osinte__qnx-light-tasking.ads",
            "s-osinte__qnx-light-tasking.adb",
            "s-oslock__qnx-light-tasking.ads",
            "s-taprop__raven-cert.ads",
            "s-taprop__qnx-light-tasking.adb",
            "s-tpopsp__posix-raven-cert.adb",
            "s-musplo.adb",
            "libgnarl/s-tpobmu.adb",
            "s-c_time.ads",
            "s-c_time.adb",
        )
