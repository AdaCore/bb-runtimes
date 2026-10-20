#
# Copyright (C) 2026, AdaCore
#

"""GNARL core sources shared by every tasking profile."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class GnarlCommon(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnarl_sources(
            "a-retide.ads",
            "a-retide__raven.adb",
            "a-sytaco__xi.ads",
            "a-sytaco__xi.adb",
            "a-taside.ads",
            "a-taside__raven.adb",
            "hie/a-taster.ads",
            "hie/a-taster.adb",
            "s-mufalo.ads",
            "s-mufalo.adb",
            "s-musplo.ads",
            "s-taprob__raven.ads",
            "s-taprob__raven.adb",
            "s-tarest.ads",
            "hie/s-tarest.adb",
            "s-tasdeb__xi.ads",
            "s-tasdeb__raven.adb",
            "s-tasinf.ads",
            "s-tasinf.adb",
            "s-tasres.ads",
            "s-tpobmu.ads",
        )
        match platform:
            case "bb":
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
            case "linux":
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
            case "lynx":
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
            case "pikeos":
                self.add_gnarl_sources(
                    "hie/s-inttyp.ads",
                    "a-interr.ads",
                    "a-interr__raven.adb",
                    "hie/a-reatim.ads",
                    "hie/a-reatim.adb",
                    "a-textio__pikeos.adb",
                    "s-interr__raven.ads",
                    "s-multip__raven-default.ads",
                    "s-musplo.adb",
                    "hie/s-taprop.ads",
                    "s-taprop__pikeos.adb",
                    "s-taspri__pikeos.ads",
                    "libgnarl/s-tpobmu.adb",
                )
            case "qnx":
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
            case "vx7r2cert":
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


class GnarlCommonNoCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "hie/s-taskin.adb",
        )


class GnarlCommonCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnarl_sources(
            "s-taskin__cheri.adb",
        )
