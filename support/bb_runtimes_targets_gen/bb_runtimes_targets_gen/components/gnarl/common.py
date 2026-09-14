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
            "libgnarl/a-retide.ads",
            "hie/a-retide__raven.adb",
            "hie/a-sytaco__xi.ads",
            "hie/a-sytaco__xi.adb",
            "libgnarl/a-taside.ads",
            "hie/a-taside__raven.adb",
            "hie/a-taster.ads",
            "hie/a-taster.adb",
            "hie/s-mufalo.ads",
            "hie/s-mufalo.adb",
            "hie/s-musplo.ads",
            "hie/s-taprob__raven.ads",
            "hie/s-taprob__raven.adb",
            "libgnarl/s-tarest.ads",
            "hie/s-tarest.adb",
            "hie/s-tasdeb__xi.ads",
            "hie/s-tasdeb__raven.adb",
            "libgnarl/s-tasinf.ads",
            "libgnarl/s-tasinf.adb",
            "libgnarl/s-tasres.ads",
            "libgnarl/s-tpobmu.ads",
        )
        match platform:
            case "bb":
                self.add_gnarl_sources(
                    "hie/s-inttyp.ads",
                    "libgnarl/a-interr.ads",
                    "hie/a-interr__raven.adb",
                    "hie/a-exetim.ads",
                    "hie/a-exetim.adb",
                    "hie/a-extiin.ads",
                    "hie/a-extiin.adb",
                    "hie/a-reatim.ads",
                    "hie/a-reatim.adb",
                    "hie/a-rttiev.ads",
                    "hie/a-rttiev.adb",
                    "hie/s-bbexti.ads",
                    "hie/s-bbexti.adb",
                    "hie/s-bbinte.ads",
                    "hie/s-bbinte.adb",
                    "hie/s-bbprot.ads",
                    "hie/s-bbprot.adb",
                    "hie/s-bbthqu.ads",
                    "hie/s-bbthqu.adb",
                    "hie/s-bbthre.ads",
                    "hie/s-bbthre.adb",
                    "hie/s-bbtiev.ads",
                    "hie/s-bbtiev.adb",
                    "hie/s-bbtime.ads",
                    "hie/s-bcprmu.ads",
                    "hie/s-bcprmu.adb",
                    "hie/s-interr__raven.ads",
                    "hie/s-interr.adb",
                    "hie/s-multip.ads",
                    "hie/s-multip.adb",
                    "hie/s-taprop__bb.adb",
                    "hie/s-taprop.ads",
                    "hie/s-taspri.ads",
                    "hie/s-tpobmu.adb",
                    "hie/s-trahoo.ads",
                    "hie/s-trahoo.adb",
                    "hie/s-osinte.ads",
                )
            case "linux":
                self.add_gnarl_sources(
                    "libgnarl/a-reatim.ads",
                    "libgnarl/a-reatim.adb",
                    "hie/s-multip__raven-default.ads",
                    "libgnarl/s-taspri__posix.ads",
                    "hie/s-osinte__linux-light-tasking.ads",
                    "hie/s-osinte__linux-light-tasking.adb",
                    "hie/s-taprop__raven-cert.ads",
                    "hie/s-taprop__linux-light-tasking.adb",
                    "hie/s-tpopsp__posix-raven-cert.adb",
                    "hie/s-musplo.adb",
                    "libgnarl/s-tpobmu.adb",
                    "libgnarl/s-linux.ads",
                    "libgnat/s-c_time.ads",
                    "libgnat/s-c_time.adb",
                    "libgnat/s-oslock__posix.ads",
                )
            case "lynx":
                self.add_gnarl_sources(
                    "libgnarl/a-reatim.ads",
                    "libgnarl/a-reatim.adb",
                    "hie/s-multip__raven-default.ads",
                    "hie/s-osinte__lynxos178-light-tasking.ads",
                    "hie/s-osinte__lynxos178-light-tasking.adb",
                    "hie/s-taprop__raven-cert.ads",
                    "hie/s-tpopsp__posix-raven-cert.adb",
                    "hie/s-taprop__lynxos178-light-tasking.adb",
                    "hie/s-oslock__lynxos178-light-tasking.ads",
                    "hie/s-musplo.adb",
                    "libgnarl/s-tpobmu.adb",
                    "libgnarl/s-taspri__lynxos.ads",
                )
            case "pikeos":
                self.add_gnarl_sources(
                    "hie/s-inttyp.ads",
                    "libgnarl/a-interr.ads",
                    "hie/a-interr__raven.adb",
                    "hie/a-reatim.ads",
                    "hie/a-reatim.adb",
                    "hie/a-textio__pikeos.adb",
                    "hie/s-interr__raven.ads",
                    "hie/s-multip__raven-default.ads",
                    "hie/s-musplo.adb",
                    "hie/s-taprop.ads",
                    "hie/s-taprop__pikeos.adb",
                    "hie/s-taspri__pikeos.ads",
                    "libgnarl/s-tpobmu.adb",
                )
            case "qnx":
                self.add_gnarl_sources(
                    "libgnarl/a-reatim.ads",
                    "libgnarl/a-reatim.adb",
                    "hie/s-multip__raven-default.ads",
                    "libgnarl/s-taspri__posix.ads",
                    "hie/s-osinte__qnx-light-tasking.ads",
                    "hie/s-osinte__qnx-light-tasking.adb",
                    "hie/s-oslock__qnx-light-tasking.ads",
                    "hie/s-taprop__raven-cert.ads",
                    "hie/s-taprop__qnx-light-tasking.adb",
                    "hie/s-tpopsp__posix-raven-cert.adb",
                    "hie/s-musplo.adb",
                    "libgnarl/s-tpobmu.adb",
                    "libgnat/s-c_time.ads",
                    "libgnat/s-c_time.adb",
                )
            case "vx7r2cert":
                self.add_gnarl_sources(
                    "libgnarl/a-reatim.ads",
                    "libgnarl/a-reatim.adb",
                    "hie/s-taprop__raven-cert.ads",
                    "hie/s-taprop__vxworks7cert.adb",
                    "hie/s-tpopsp__vxworks7-cert-tls.adb",
                    "hie/s-taspri__raven-vxworks.ads",
                    "hie/s-multip__raven-default.ads",
                    "hie/s-musplo.adb",
                    "hie/s-osinte__vxworks-light-tasking.ads",
                    "hie/s-osinte__vxworks-light-tasking.adb",
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
            "hie/s-taskin__cheri.adb",
        )
