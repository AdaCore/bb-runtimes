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
            "libgnat/ada.ads",
            "libgnat/a-assert.ads",
            "libgnat/a-assert.adb",
            "libgnat/a-charac.ads",
            "libgnat/a-chlat1.ads",
            "libgnat/a-numeri.ads",
            "libgnat/a-nubinu.ads",
            "libgnat/a-string.ads",
            "libgnat/a-strbou.ads",
            "libgnat/a-strbou.adb",
            "libgnat/a-strfix.ads",
            "libgnat/a-strfix.adb",
            "libgnat/a-strmap.ads",
            "libgnat/a-strmap.adb",
            "libgnat/a-stmaco.ads",
            "libgnat/a-strsea.ads",
            "libgnat/a-strsea.adb",
            "libgnat/a-unccon.ads",
            "libgnat/a-uncdea.ads",
            "libgnat/gnat.ads",
            "libgnat/g-debuti.ads",
            "libgnat/g-debuti.adb",
            "libgnat/g-sestin.ads",
            "libgnat/g-souinf.ads",
            "libgnat/interfac__2020.ads",
            "libgnat/i-cstrin.ads",
            "libgnat/i-cstrin.adb",
            "libgnat/i-cpoint.ads",
            "libgnat/i-cpoint.adb",
            "libgnat/machcode.ads",
            "libgnat/s-addope.ads",
            "libgnat/s-addope.adb",
            "libgnat/s-atacco.ads",
            "libgnat/s-atacco.adb",
            "libgnat/s-arit32.ads",
            "libgnat/s-arit32.adb",
            "libgnat/s-assert.ads",
            "libgnat/s-bitops.ads",
            "libgnat/s-bitops.adb",
            "libgnat/s-boarop.ads",
            "libgnat/s-bytswa.ads",
            "libgnat/s-carsi8.ads",
            "libgnat/s-carsi8.adb",
            "libgnat/s-carun8.ads",
            "libgnat/s-carun8.adb",
            "libgnat/s-casi16.ads",
            "libgnat/s-casi16.adb",
            "libgnat/s-casi32.ads",
            "libgnat/s-casi32.adb",
            "libgnat/s-casi64.ads",
            "libgnat/s-casi64.adb",
            "libgnat/s-caun16.ads",
            "libgnat/s-caun16.adb",
            "libgnat/s-caun32.ads",
            "libgnat/s-caun32.adb",
            "libgnat/s-caun64.ads",
            "libgnat/s-caun64.adb",
            "libgnat/s-elaall.ads",
            "libgnat/s-elaall.adb",
            "libgnat/s-geveop.ads",
            "libgnat/s-geveop.adb",
            "libgnat/s-imagei.ads",
            "libgnat/s-imagei.adb",
            "libgnat/s-imageu.ads",
            "libgnat/s-imageu.adb",
            "libgnat/s-imgboo.ads",
            "libgnat/s-imgboo.adb",
            "libgnat/s-imgint.ads",
            "libgnat/s-imglli.ads",
            "libgnat/s-imgllu.ads",
            "libgnat/s-imguns.ads",
            "libgnat/s-maccod.ads",
            "libgnat/s-retsta.ads",
            "libgnat/s-stache.ads",
            "libgnat/s-stache.adb",
            "libgnat/s-stoele.ads",
            "libgnat/s-stoele.adb",
            "libgnat/s-unstyp.ads",
            "libgnat/s-vector.ads",
            "libgnat/s-veboop.ads",
            "libgnat/s-veboop.adb",
            "libgnat/s-widint.ads",
            "libgnat/s-widlli.ads",
            "libgnat/s-widllu.ads",
            "libgnat/s-widuns.ads",
            "libgnat/s-widthi.ads",
            "libgnat/s-widthi.adb",
            "libgnat/s-widthu.ads",
            "libgnat/s-widthu.adb",
            "libgnat/text_io.ads",
            "libgnat/unchconv.ads",
            "libgnat/unchdeal.ads",
        )
        match platform:
            case "bb":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put__bb.adb",
                    "hie/s-bb.ads",
                    "hie/s-macres.ads",
                    "hie/s-textio.ads",
                )
            case "deos":
                self.add_gnat_sources(
                    "exit.c",
                    "hie/g-io__c.ads",
                    "hie/a-textio__deos.ads",
                    "hie/a-textio__deos.adb",
                    "hie/s-macres.ads",
                )
            case "freertos":
                self.add_gnat_sources(
                    "hie/s-textio__freertos.ads",
                    "hie/s-textio__freertos.adb",
                    "hie/s-macres.ads",
                    "hie/a-textio__c.ads",
                    "hie/a-textio__c.adb",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put.adb",
                )
            case "linux":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "hie/a-textio__libc.adb",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put.adb",
                    "hie/s-macres.ads",
                    "hie/s-textio__libc.ads",
                    "hie/s-textio__libc.adb",
                )
            case "lynx":
                self.add_gnat_sources(
                    "exit.c",
                    "hie/a-textio.ads",
                    "hie/a-textio__lynxos178.adb",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put__lynxos178.adb",
                    "hie/s-macres.ads",
                    "hie/s-textio__libc.ads",
                    "hie/s-textio__lynxos178.adb",
                )
            case "pikeos":
                self.add_gnat_sources(
                    "hie/a-textio.ads",
                    "exit.c",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put__bb.adb",
                    "hie/s-macres.ads",
                    "hie/s-parame__large.ads",
                    "hie/s-parame.adb",
                    "hie/s-textio.ads",
                )
            case "qnx":
                self.add_gnat_sources(
                    "exit.c",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__zfp.adb",
                    "hie/g-io-put.adb",
                    "hie/a-textio.ads",
                    "hie/a-textio__libc.adb",
                    "hie/s-macres.ads",
                    "hie/s-textio__libc.ads",
                    "hie/s-textio__libc.adb",
                )
            case "vx7r2cert":
                self.add_gnat_sources(
                    "exit.c",
                    "hie/g-io__zfp.ads",
                    "hie/g-io__vxworks7cert.adb",
                    "hie/s-textio__vxworks7cert.ads",
                    "hie/s-textio__vxworks7cert.adb",
                    "hie/a-textio.ads",
                    "hie/a-textio__vxworks7cert.adb",
                    "hie/s-macres.ads",
                    "vx_stack_info.c",
                )


class Common32(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/i-cexten.ads",
        )


class Common64(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/i-cexten__128.ads",
            "libgnat/s-casi128.ads",
            "libgnat/s-casi128.adb",
            "libgnat/s-caun128.ads",
            "libgnat/s-caun128.adb",
            "libgnat/s-imgllli.ads",
            "libgnat/s-imglllu.ads",
            "libgnat/s-widllli.ads",
            "libgnat/s-widlllu.ads",
        )


class CommonCheri(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "libgnat/i-cheri.ads",
            "libgnat/i-cheri.adb",
            "libgnat/i-cheri-exceptions.ads",
        )
