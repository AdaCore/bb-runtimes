#
# Copyright (C) 2026, AdaCore
#

"""Bareboard GNARL component."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    IncompatibleComponent,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlBareboard(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if target.platform != "bb":
            raise IncompatibleComponent()

        self.add_gnarl_sources(
            "hie/s-inttyp.ads",
            "a-interr.ads",
            "a-interr__raven.adb",
            "hie/a-exetim.ads",
            "a-exetim.adb",
            "hie/a-extiin.ads",
            "a-extiin.adb",
            "hie/a-reatim.ads",
            "hie/a-reatim.adb",
            "hie/a-rttiev.ads",
            "hie/a-rttiev.adb",
            "s-bbexti.ads",
            "s-bbexti.adb",
            "s-bbinte.ads",
            "s-bbinte.adb",
            "s-bbprot.ads",
            "s-bbprot.adb",
            "s-bbthqu.ads",
            "s-bbthqu.adb",
            "s-bbthre.ads",
            "s-bbthre.adb",
            "s-bbtiev.ads",
            "s-bbtiev.adb",
            "s-bbtime.ads",
            "s-bcprmu.ads",
            "s-bcprmu.adb",
            "s-interr__raven.ads",
            "hie/s-interr.adb",
            "hie/s-multip.ads",
            "hie/s-multip.adb",
            "s-taprop__bb.adb",
            "hie/s-taprop.ads",
            "s-taspri.ads",
            "hie/s-tpobmu.adb",
            "s-trahoo.ads",
            "s-trahoo.adb",
            "s-osinte.ads",
        )
