#
# Copyright (C) 2026, AdaCore
#

"""Core runtime sources shared by every profile."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)


class Common(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        platform = target.platform
        self.add_gnat_sources(
            "ada.ads",
            "a-assert.ads",
            "a-assert.adb",
            "a-charac.ads",
            "a-chlat1.ads",
            "a-numeri.ads",
            "a-nubinu.ads",
            "a-string.ads",
            "a-strbou.ads",
            "a-strbou.adb",
            "a-strfix.ads",
            "a-strfix.adb",
            "a-strmap.ads",
            "a-strmap.adb",
            "a-stmaco.ads",
            "a-strsea.ads",
            "a-strsea.adb",
            "a-unccon.ads",
            "a-uncdea.ads",
            "gnat.ads",
            "g-debuti.ads",
            "g-debuti.adb",
            "g-sestin.ads",
            "g-souinf.ads",
            "interfac__2020.ads",
            "i-cstrin.ads",
            "i-cstrin.adb",
            "i-cpoint.ads",
            "i-cpoint.adb",
            "machcode.ads",
            "s-addope.ads",
            "s-addope.adb",
            "s-atacco.ads",
            "s-atacco.adb",
            "s-arit32.ads",
            "s-arit32.adb",
            "s-assert.ads",
            "s-bitops.ads",
            "s-bitops.adb",
            "s-boarop.ads",
            "s-bytswa.ads",
            "s-carsi8.ads",
            "s-carsi8.adb",
            "s-carun8.ads",
            "s-carun8.adb",
            "s-casi16.ads",
            "s-casi16.adb",
            "s-casi32.ads",
            "s-casi32.adb",
            "s-casi64.ads",
            "s-casi64.adb",
            "s-caun16.ads",
            "s-caun16.adb",
            "s-caun32.ads",
            "s-caun32.adb",
            "s-caun64.ads",
            "s-caun64.adb",
            "s-elaall.ads",
            "s-elaall.adb",
            "s-geveop.ads",
            "s-geveop.adb",
            "s-imagei.ads",
            "s-imagei.adb",
            "s-imageu.ads",
            "s-imageu.adb",
            "s-imgboo.ads",
            "s-imgboo.adb",
            "s-imgint.ads",
            "s-imglli.ads",
            "s-imgllu.ads",
            "s-imguns.ads",
            "s-maccod.ads",
            "s-retsta.ads",
            "s-stache.ads",
            "s-stache.adb",
            "s-stoele.ads",
            "s-stoele.adb",
            "s-unstyp.ads",
            "s-vector.ads",
            "s-veboop.ads",
            "s-veboop.adb",
            "s-widint.ads",
            "s-widlli.ads",
            "s-widllu.ads",
            "s-widuns.ads",
            "s-widthi.ads",
            "s-widthi.adb",
            "s-widthu.ads",
            "s-widthu.adb",
            "text_io.ads",
            "unchconv.ads",
            "unchdeal.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put__bb.adb",
                    "s-bb.ads",
                    "s-macres.ads",
                    "s-textio.ads",
                )
            case "deos":
                self.add_gnat_sources(
                    "exit.c",
                    "g-io__c.ads",
                    "a-textio__deos.ads",
                    "a-textio__deos.adb",
                    "s-macres.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "s-textio__freertos.ads",
                    "s-textio__freertos.adb",
                    "s-macres.ads",
                    "a-textio__c.ads",
                    "a-textio__c.adb",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "a-textio__libc.adb",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put.adb",
                    "s-macres.ads",
                    "s-textio__libc.ads",
                    "s-textio__libc.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "exit.c",
                    "hie/a-textio.ads",
                    "a-textio__lynxos178.adb",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put__lynxos178.adb",
                    "s-macres.ads",
                    "s-textio__libc.ads",
                    "s-textio__lynxos178.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "exit.c",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put__bb.adb",
                    "s-macres.ads",
                    "s-parame__large.ads",
                    "hie/s-parame.adb",
                    "s-textio.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "exit.c",
                    "g-io__zfp.ads",
                    "g-io__zfp.adb",
                    "g-io-put.adb",
                    "hie/a-textio.ads",
                    "a-textio__libc.adb",
                    "s-macres.ads",
                    "s-textio__libc.ads",
                    "s-textio__libc.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "exit.c",
                    "g-io__zfp.ads",
                    "g-io__vxworks7cert.adb",
                    "s-textio__vxworks7cert.ads",
                    "s-textio__vxworks7cert.adb",
                    "hie/a-textio.ads",
                    "a-textio__vxworks7cert.adb",
                    "s-macres.ads",
                    "vx_stack_info.c",
                )


class Common32(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "i-cexten.ads",
        )


class Common64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "i-cexten__128.ads",
            "s-casi128.ads",
            "s-casi128.adb",
            "s-caun128.ads",
            "s-caun128.adb",
            "s-imgllli.ads",
            "s-imglllu.ads",
            "s-widllli.ads",
            "s-widlllu.ads",
        )


class CommonCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "i-cheri.ads",
            "i-cheri.adb",
            "i-cheri-exceptions.ads",
        )
