#
# Copyright (C) 2026, AdaCore
#

"""LynxOS GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlLynx(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "lynx":
            raise IncompatibleComponent()

        self.add_gnarl_sources(
            "libgnarl/a-reatim.ads",
            "libgnarl/a-reatim.adb",
            "s-multip__raven-default.ads",
            "s-osinte__lynxos178-light-tasking.ads",
            "s-osinte__lynxos178-light-tasking.adb",
            "s-taprop__raven-cert.ads",
            "s-tpopsp__posix-raven-cert.adb",
            "s-taprop__lynxos178-light-tasking.adb",
            "s-oslock__lynxos178-light-tasking.ads",
            "s-musplo.adb",
            "libgnarl/s-tpobmu.adb",
            "s-taspri__lynxos.ads",
        )
