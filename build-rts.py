#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

# This makes us use the local repository as a python module
import build_rts_support

from build_rts_support.config import Config
from build_rts_support.files_holder import SharedFilesHolder, readfile
from build_rts_support.bsp import BSP
from build_rts_support.target import Target, DFBBTarget

# PikeOS
from pikeos import ArmPikeOS, PpcPikeOS, X86PikeOS

# Cortex-M runtimes
from arm.stm32 import Stm32
from arm.sam import Sam
from arm.smartfusion2 import SmartFusion2
from arm.lm3s import LM3S

# Cortex-A/R runtimes
from arm.tms570 import TMS570
from arm.rpi2 import RPI2
from arm.zynq import Zynq7000

# Aarch64
from aarch64.rpi3 import RPI3

# leon
from sparc.leon import Leon2
from sparc.leon3 import Leon3

# powerpc
from powerpc.mpc8641 import MPC8641
from powerpc.mpc8349 import MPC8349e
from powerpc.p2020 import P2020
from powerpc.p5566 import P5566
from powerpc.mpc5634 import P5634

# visium
from visium import Visium

# native
from native import X86Linux, X86Windows

import getopt
import os
import sys


# Definitions of shared source files.
# Keep spec and body on one line.

class SourceDirs(SharedFilesHolder):
    def __init__(self, is_bb):
        super(SourceDirs, self).__init__()
        self._is_bb = is_bb
        self._init_zfp()
        self._init_sfp()
        self._init_full()

    def install(self, dirname, destination, installed_files):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)

        if Config.create_common:
            # Change dirs to ../rts-sources/<dirname>
            rel = os.path.join('..', Config.shared_sources, dirname)
        else:
            rel = dirname

        destdir = os.path.join(destination, rel)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

            for k, v in self.dirs[dirname].items():
                self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                                installed_files=installed_files)
        else:
            for k, v in self.dirs[dirname].items():
                installed_files.append(os.path.basename(k))

        return rel

    def _init_zfp(self):
        """Files common to all runtimes"""
        # files there no matter what
        self.add_sources('common', [
            'a-assert.ads', 'a-assert.adb',
            {'a-textio.ads': 'a-textio-zfp.ads'},
            'a-unccon.ads',
            'a-uncdea.ads',
            'ada.ads',
            {'g-io.ads': 'g-io-zfp.ads'},
            {'g-io.adb': 'g-io-zfp.adb'},
            {'g-io-put.adb': 'g-io-put-stextio.adb'},
            'g-souinf.ads',
            'gnat.ads',
            'i-cexten.ads',
            'interfac.ads',
            'machcode.ads',
            's-assert.ads',
            's-atacco.ads', 's-atacco.adb',
            's-imgboo.ads', 's-imgboo.adb',
            's-imgint.ads', 's-imgint.adb',
            's-imglli.ads', 's-imglli.adb',
            's-imgllu.ads', 's-imgllu.adb',
            's-imguns.ads', 's-imguns.adb',
            's-maccod.ads',
            's-macres.ads',
            {'s-secsta.ads': 's-secsta-zfp.ads'},
            {'s-secsta.adb': 's-secsta-zfp.adb'},
            's-stoele.ads', 's-stoele.adb',
            {'s-textio.ads': 's-textio-zfp.ads'},
            's-unstyp.ads',
            'text_io.ads',
            'unchconv.ads',
            'unchdeal.ads'])

        if self._is_bb:
            self.add_sources('common', [
                {'a-textio.adb': 'a-textio-zfp.adb'},
                's-bb.ads'])
        else:
            self.add_sources('zfp_io', {'a-textio.adb': 'a-textio-zfp.adb'})
            self.add_sources('sfp_io', {'a-textio.adb': 'a-textio-raven.adb'})

        # FPU support sources
        self.add_sources('fpu', [
            's-fatflt.ads',
            's-fatgen.ads', 's-fatgen.adb',
            's-fatlfl.ads',
            's-fatllf.ads',
            's-fatsfl.ads'])

        # Memory support sources
        self.add_sources('mem', [
            's-memcom.ads', 's-memcom.adb',
            {'s-memcop.ads': 's-memcop-zfp.ads',
             's-memcop.adb': 's-memcop-zfp.adb'},
            's-memmov.ads', 's-memmov.adb',
            's-memset.ads', 's-memset.adb'])

        # Libc implementation
        self.add_sources('libc', {
            's-c.ads': 's-c-zfp.ads',
            's-cerrno.ads': 's-cerrno-zfp.ads',
            's-cerrno.adb': 's-cerrno-zfp.adb',
            's-cmallo.ads': 's-cmallo-zfp.ads',
            's-cmallo.adb': 's-cmallo-zfp.adb',
            's-cstrle.ads': 's-cstrle-zfp.ads',
            's-cstrle.adb': 's-cstrle-zfp.adb'})

        # Newlib support
        self.add_sources('newlib', [
            'newlib-bb.c'])

        # Math support
        self.add_sources('math', [
            'a-ncelfu.ads',
            'a-ngcefu.ads', 'a-ngcefu.adb',
            'a-ngcoar.ads', 'a-ngcoar.adb',
            'a-ngcoty.ads', 'a-ngcoty.adb',
            'a-ngelfu.ads', 'a-ngelfu.adb',
            'a-ngrear.ads', 'a-ngrear.adb',
            'a-nlcefu.ads',
            'a-nlcoty.ads',
            'a-nlelfu.ads',
            'a-nllcef.ads',
            'a-nllcty.ads',
            'a-nllefu.ads',
            'a-nscefu.ads',
            'a-nscoty.ads',
            'a-nselfu.ads',
            'a-nucoty.ads',
            'a-nuelfu.ads',
            'a-numaux.ads',
            'a-numeri.ads',
            'a-nurear.ads',
            's-exnllf.ads', 's-exnllf.adb',
            's-gcmain.ads', 's-gcmain.adb',
            's-gearop.ads', 's-gearop.adb',
            's-libdou.ads', 's-libdou.adb',
            's-libm.ads', 's-libm.adb',
            's-libpre.ads',
            's-libsin.ads', 's-libsin.adb',
            's-lidosq.ads',
            's-lisisq.ads'])
        self.update_pairs('math', {
            'a-ngcoty.adb': 'a-ngcoty-ada.adb',
            'a-ngelfu.ads': 'a-ngelfu-ada.ads',
            'a-ngelfu.adb': 'a-ngelfu-ada.adb',
            'a-nlelfu.ads': 'a-nlelfu-ada.ads',
            'a-nuelfu.ads': 'a-nuelfu-ada.ads',
            'a-numaux.ads': 'a-numaux-ada.ads',
            's-gcmain.ads': 's-gcmain-ada.ads',
            's-gcmain.adb': 's-gcmain-ada.adb',
            's-libdou.ads': 's-libdou-ada.ads',
            's-libdou.adb': 's-libdou-ada.adb',
            's-libm.ads': 's-libm-ada.ads',
            's-libm.adb': 's-libm-ada.adb',
            's-libpre.ads': 's-libpre-ada.ads',
            's-libsin.ads': 's-libsin-ada.ads',
            's-libsin.adb': 's-libsin-ada.adb',
            's-lidosq.ads': 's-lidosq-ada.ads',
            's-lisisq.ads': 's-lisisq-ada.ads'})
        if self._is_bb:
            self.add_sources('math/softsp', {
                's-lisisq.adb': 's-lisisq-ada.adb'})
            self.add_sources('math/hardsp', {
                's-lisisq.adb': 's-lisisq-fpu.adb'})
            self.add_sources('math/softdp', {
                's-lidosq.adb': 's-lidosq-ada.adb'})
            self.add_sources('math/harddp', {
                's-lidosq.adb': 's-lidosq-fpu.adb'})
        else:
            # PikeOS
            self.add_sources('math', {
                's-lisisq.adb': 's-lisisq-fpu.adb',
                's-lidosq.adb': 's-lidosq-fpu.adb'})

        # Finally, the ZFP & SFP-specific libgnat files
        self.add_sources('zfp', {
            'a-elchha.ads': 'a-elchha-zfp.ads',
            'a-elchha.adb': 'a-elchha-zfp.adb',
            'a-except.ads': 'a-except-zfp.ads',
            'a-except.adb': 'a-except-zfp.adb',
            'a-tags.ads': 'a-tags-hie.ads',
            'a-tags.adb': 'a-tags-hie.adb',
            'i-c.ads': 'i-c-hie.ads',
            's-assert.adb': 's-assert-xi.adb',
            's-memory.ads': 's-memory-zfp.ads',
            's-sssita.ads': 's-sssita-xi.ads',
            's-sssita.adb': 's-sssita-xi.adb'})
        if self._is_bb:
            self.add_sources('zfp', {
                's-memory.adb': 's-memory-zfp.adb'})
        else:
            self.add_sources('zfp', {
                's-memory.adb': 's-memory-raven-min.adb'})

    def _init_sfp(self):
        """ravenscar-sfp files"""
        # libgnarl sources common to sfp/full
        self.add_sources('gnarl/common', [
            'a-interr.ads', 'a-interr.adb',
            'a-reatim.ads', 'a-reatim.adb',
            'a-retide.ads', 'a-retide.adb',
            'a-sytaco.ads', 'a-sytaco.adb',
            'a-taside.ads', 'a-taside.adb',
            'a-taster.ads', 'a-taster.adb',
            's-interr.ads',
            's-mufalo.ads', 's-mufalo.adb',
            's-multip.ads', 's-multip.adb',
            's-musplo.ads',
            's-parame.adb',
            's-taprob.ads', 's-taprob.adb',
            's-taprop.ads', 's-taprop.adb',
            's-tarest.ads', 's-tarest.adb',
            's-tasdeb.ads', 's-tasdeb.adb',
            's-tasinf.ads', 's-tasinf.adb',
            's-taskin.adb',
            's-taspri.ads',
            's-tasres.ads',
            's-tpobmu.ads', 's-tpobmu.adb'])
        self.update_pairs('gnarl/common', {
            'a-interr.adb': 'a-interr-raven.adb',
            'a-reatim.ads': 'a-reatim-xi.ads',
            'a-reatim.adb': 'a-reatim-xi.adb',
            'a-retide.adb': 'a-retide-raven.adb',
            'a-sytaco.ads': 'a-sytaco-xi.ads',
            'a-sytaco.adb': 'a-sytaco-xi.adb',
            'a-taside.adb': 'a-taside-raven.adb',
            'a-taster.ads': 'a-taster-raven.ads',
            'a-taster.adb': 'a-taster-raven.adb',
            's-interr.ads': 's-interr-raven.ads',
            's-parame.adb': 's-parame-xi.adb',
            's-taprob.adb': 's-taprob-raven.adb',
            's-taprob.ads': 's-taprob-raven.ads',
            's-taprop.ads': 's-taprop-xi.ads',
            's-tarest.adb': 's-tarest-raven.adb',
            's-tasdeb.adb': 's-tasdeb-raven.adb',
            's-tasdeb.ads': 's-tasdeb-xi.ads',
            's-taskin.adb': 's-taskin-raven.adb',
            's-taspri.ads': 's-taspri-xi.ads'})
        if self._is_bb:
            # BB case
            self.add_sources('gnarl/common', [
                'a-exetim.ads', 'a-exetim.adb',
                'a-extiin.ads', 'a-extiin.adb',
                'a-rttiev.ads', 'a-rttiev.adb',
                's-bbbosu.ads',
                's-bbexti.ads', 's-bbexti.adb',
                's-bbinte.ads',
                's-bbprot.ads', 's-bbprot.adb',
                's-bbthqu.ads', 's-bbthqu.adb',
                's-bbthre.ads', 's-bbthre.adb',
                's-bbtiev.ads', 's-bbtiev.adb',
                's-bbtime.ads',
                's-bcprmu.ads', 's-bcprmu.adb',
                's-interr.adb',
                's-osinte.ads'])
            self.update_pairs('gnarl/common', {
                'a-exetim.ads': 'a-exetim-bb.ads',
                'a-exetim.adb': 'a-exetim-bb.adb',
                'a-extiin.ads': 'a-extiin-bb.ads',
                'a-extiin.adb': 'a-extiin-bb.adb',
                'a-rttiev.ads': 'a-rttiev-bb.ads',
                'a-rttiev.adb': 'a-rttiev-bb.adb',
                's-interr.adb': 's-interr-xi.adb',
                's-multip.ads': 's-multip-bb.ads',
                's-multip.adb': 's-multip-bb.adb',
                's-osinte.ads': 's-osinte-bb.ads',
                's-taprop.adb': 's-taprop-xi.adb',
                's-tpobmu.adb': 's-tpobmu-bb.adb'})
        else:
            # PikeOS case
            self.update_pairs('gnarl/common', {
                's-multip.ads': 's-multip-raven-default.ads',
                's-multip.adb': 's-multip-raven-default.adb',
                's-taprop.adb': 's-taprop-pikeos.adb'})
            self.add_sources('gnarl/pikeos3', {
                's-interr.adb': 's-interr-pikeos.adb',
                's-osinte.ads': 's-osinte-pikeos.ads',
                's-osinte.adb': 's-osinte-pikeos.adb'})
            self.add_sources('gnarl/pikeos4', {
                's-interr.adb': 's-interr-pikeos4.adb',
                's-osinte.ads': 's-osinte-pikeos4.ads',
                's-osinte.adb': 's-osinte-pikeos4.adb'})

        # SFP-specific files
        self.add_sources('gnarl/sfp', {
            's-taskin.ads': 's-taskin-raven.ads',
            's-tposen.adb': 's-tposen-raven.adb',
            's-tposen.ads': 's-tposen-raven.ads'})

        # timer support
        self.add_sources('gnarl/timer32', 's-bbtime.adb')
        self.add_sources('gnarl/timer64', {'s-bbtime.adb': 's-bbtime-ppc.adb'})

        # spinlock support (leon workaround)
        if self._is_bb:
            self.add_sources('gnarl/spinlock/gcc', 's-musplo.adb')
            self.add_sources('gnarl/spinlock/leon',
                             {'s-musplo.adb': 's-musplo-leon.adb'})
        else:
            self.add_sources('gnarl/common', 's-musplo.adb')

        # memory profile
        if self._is_bb:
            self.add_sources('gnarl/mem-small',
                             {'s-parame.ads': 's-parame-xi-small.ads'})
            self.add_sources('gnarl/mem-large',
                             {'s-parame.ads': 's-parame-xi.ads'})
        else:
            # PikeOS:
            self.add_sources('gnarl/common',
                             {'s-parame.ads': 's-parame-xi.ads'})

    def _init_full(self):
        """ravenscar-full files"""

        # libgnat files for the full profile
        self.add_sources('full', [
            'a-chahan.ads', 'a-chahan.adb',
            'a-charac.ads',
            'a-chlat1.ads',
            'a-chlat9.ads',
            'a-cwila1.ads',
            'a-cwila9.ads',
            'a-decima.ads', 'a-decima.adb',
            'a-einuoc.ads', 'a-einuoc.adb',
            'a-elchha.ads', 'a-elchha.adb',
            'a-excach.adb',
            'a-except.ads', 'a-except.adb',
            'a-excpol.adb',
            'a-exctra.ads', 'a-exctra.adb',
            'a-exexda.adb',
            'a-exexpr.adb',
            'a-exextr.adb',
            'a-exstat.adb',
            'a-finali.ads', 'a-finali.adb',
            'a-ioexce.ads',
            'a-nudira.ads', 'a-nudira.adb',
            'a-nuflra.ads', 'a-nuflra.adb',
            'a-stmaco.ads',
            'a-storio.ads', 'a-storio.adb',
            'a-strbou.ads', 'a-strbou.adb',
            'a-stream.ads', 'a-stream.adb',
            'a-strfix.ads', 'a-strfix.adb',
            'a-string.ads',
            'a-strmap.ads', 'a-strmap.adb',
            'a-strsea.ads', 'a-strsea.adb',
            'a-strsup.ads', 'a-strsup.adb',
            'a-strunb.ads', 'a-strunb.adb',
            'a-stunau.ads', 'a-stunau.adb',
            'a-stwibo.ads', 'a-stwibo.adb',
            'a-stwifi.ads', 'a-stwifi.adb',
            'a-stwima.ads', 'a-stwima.adb',
            'a-stwise.ads', 'a-stwise.adb',
            'a-stwisu.ads', 'a-stwisu.adb',
            'a-stwiun.ads', 'a-stwiun.adb',
            'a-swmwco.ads',
            'a-tags.ads', 'a-tags.adb',
            'a-undesu.ads', 'a-undesu.adb',
            'g-arrspl.ads', 'g-arrspl.adb',
            'g-bubsor.ads', 'g-bubsor.adb',
            'g-busora.ads', 'g-busora.adb',
            'g-busorg.ads', 'g-busorg.adb',
            'g-bytswa.ads', 'g-bytswa.adb',
            'g-casuti.ads', 'g-casuti.adb',
            'g-comver.ads', 'g-comver.adb',
            'g-crc32.ads', 'g-crc32.adb',
            'g-debuti.ads', 'g-debuti.adb',
            'g-except.ads',
            'g-heasor.ads', 'g-heasor.adb',
            'g-hesora.ads', 'g-hesora.adb',
            'g-hesorg.ads', 'g-hesorg.adb',
            'g-htable.ads', 'g-htable.adb',
            'g-md5.ads', 'g-md5.adb',
            'g-moreex.ads', 'g-moreex.adb',
            'g-regexp.ads',
            'g-sechas.ads', 'g-sechas.adb',
            'g-sehamd.ads', 'g-sehamd.adb',
            'g-sehash.ads', 'g-sehash.adb',
            'g-sha1.ads', 'g-sha1.adb',
            'g-sha224.ads',
            'g-sha256.ads',
            'g-sha384.ads',
            'g-sha512.ads',
            'g-shsh32.ads', 'g-shsh32.adb',
            'g-shsh64.ads', 'g-shsh64.adb',
            'g-shshco.ads', 'g-shshco.adb',
            'g-string.ads',
            'g-strspl.ads',
            'g-table.ads', 'g-table.adb',
            'g-tasloc.ads',
            'g-wistsp.ads',
            'i-c.ads', 'i-c.adb',
            'i-cobol.ads', 'i-cobol.adb',
            'i-cpoint.ads', 'i-cpoint.adb',
            'i-cstrin.ads', 'i-cstrin.adb',
            'i-fortra.ads', 'i-fortra.adb',
            'i-pacdec.ads', 'i-pacdec.adb',
            'ioexcept.ads',
            'raise-gcc.c',
            'raise.h',
            's-addima.ads', 's-addima.adb',
            's-addope.ads', 's-addope.adb',
            's-arit64.ads', 's-arit64.adb',
            's-assert.adb',
            's-bitops.ads', 's-bitops.adb',
            's-boarop.ads',
            's-bytswa.ads',
            's-carsi8.ads', 's-carsi8.adb',
            's-carun8.ads', 's-carun8.adb',
            's-casi16.ads', 's-casi16.adb',
            's-casi32.ads', 's-casi32.adb',
            's-casi64.ads', 's-casi64.adb',
            's-casuti.ads', 's-casuti.adb',
            's-caun16.ads', 's-caun16.adb',
            's-caun32.ads', 's-caun32.adb',
            's-caun64.ads', 's-caun64.adb',
            's-chepoo.ads',
            's-crc32.ads', 's-crc32.adb',
            's-excdeb.ads', 's-excdeb.adb',
            's-except.ads', 's-except.adb',
            's-exctab.ads', 's-exctab.adb',
            's-exnint.ads', 's-exnint.adb',
            's-exnlli.ads', 's-exnlli.adb',
            's-expint.ads', 's-expint.adb',
            's-explli.ads', 's-explli.adb',
            's-expllu.ads', 's-expllu.adb',
            's-expmod.ads', 's-expmod.adb',
            's-expuns.ads', 's-expuns.adb',
            's-finmas.ads', 's-finmas.adb',
            's-finroo.ads', 's-finroo.adb',
            's-flocon.ads', 's-flocon.adb',
            's-fore.ads', 's-fore.adb',
            's-geveop.ads', 's-geveop.adb',
            's-htable.ads', 's-htable.adb',
            's-imenne.ads', 's-imenne.adb',
            's-imgbiu.ads', 's-imgbiu.adb',
            's-imgcha.adb', 's-imgcha.ads',
            's-imgdec.adb', 's-imgdec.ads',
            's-imgenu.ads', 's-imgenu.adb',
            's-imgllb.ads', 's-imgllb.adb',
            's-imglld.ads', 's-imglld.adb',
            's-imgllw.ads', 's-imgllw.adb',
            's-imgrea.ads', 's-imgrea.adb',
            's-imgwch.ads', 's-imgwch.adb',
            's-imgwiu.ads', 's-imgwiu.adb',
            's-init.ads', 's-init.adb',
            's-io.ads', 's-io.adb',
            's-mantis.ads', 's-mantis.adb',
            's-mastop.ads', 's-mastop.adb',
            's-memory.ads', 's-memory.adb',
            's-pack03.ads', 's-pack03.adb',
            's-pack05.ads', 's-pack05.adb',
            's-pack06.ads', 's-pack06.adb',
            's-pack07.ads', 's-pack07.adb',
            's-pack09.ads', 's-pack09.adb',
            's-pack10.ads', 's-pack10.adb',
            's-pack11.ads', 's-pack11.adb',
            's-pack12.ads', 's-pack12.adb',
            's-pack13.ads', 's-pack13.adb',
            's-pack14.ads', 's-pack14.adb',
            's-pack15.ads', 's-pack15.adb',
            's-pack17.ads', 's-pack17.adb',
            's-pack18.ads', 's-pack18.adb',
            's-pack19.ads', 's-pack19.adb',
            's-pack20.ads', 's-pack20.adb',
            's-pack21.ads', 's-pack21.adb',
            's-pack22.ads', 's-pack22.adb',
            's-pack23.ads', 's-pack23.adb',
            's-pack24.ads', 's-pack24.adb',
            's-pack25.ads', 's-pack25.adb',
            's-pack26.ads', 's-pack26.adb',
            's-pack27.ads', 's-pack27.adb',
            's-pack28.ads', 's-pack28.adb',
            's-pack29.ads', 's-pack29.adb',
            's-pack30.ads', 's-pack30.adb',
            's-pack31.ads', 's-pack31.adb',
            's-pack33.ads', 's-pack33.adb',
            's-pack34.ads', 's-pack34.adb',
            's-pack35.ads', 's-pack35.adb',
            's-pack36.ads', 's-pack36.adb',
            's-pack37.ads', 's-pack37.adb',
            's-pack38.ads', 's-pack38.adb',
            's-pack39.ads', 's-pack39.adb',
            's-pack40.ads', 's-pack40.adb',
            's-pack41.ads', 's-pack41.adb',
            's-pack42.ads', 's-pack42.adb',
            's-pack43.ads', 's-pack43.adb',
            's-pack44.ads', 's-pack44.adb',
            's-pack45.ads', 's-pack45.adb',
            's-pack46.ads', 's-pack46.adb',
            's-pack47.ads', 's-pack47.adb',
            's-pack48.ads', 's-pack48.adb',
            's-pack49.ads', 's-pack49.adb',
            's-pack50.ads', 's-pack50.adb',
            's-pack51.ads', 's-pack51.adb',
            's-pack52.ads', 's-pack52.adb',
            's-pack53.ads', 's-pack53.adb',
            's-pack54.ads', 's-pack54.adb',
            's-pack55.ads', 's-pack55.adb',
            's-pack56.ads', 's-pack56.adb',
            's-pack57.ads', 's-pack57.adb',
            's-pack58.ads', 's-pack58.adb',
            's-pack59.ads', 's-pack59.adb',
            's-pack60.ads', 's-pack60.adb',
            's-pack61.ads', 's-pack61.adb',
            's-pack62.ads', 's-pack62.adb',
            's-pack63.ads', 's-pack63.adb',
            's-pooglo.ads', 's-pooglo.adb',
            's-pooloc.ads', 's-pooloc.adb',
            's-poosiz.ads', 's-poosiz.adb',
            's-powtab.ads',
            's-rannum.ads', 's-rannum.adb',
            's-ransee.ads', 's-ransee.adb',
            's-regexp.ads', 's-regexp.adb',
            's-restri.ads', 's-restri.adb',
            's-rident.ads',
            's-scaval.ads', 's-scaval.adb',
            's-soflin.ads', 's-soflin.adb',
            's-sopco3.ads', 's-sopco3.adb',
            's-sopco4.ads', 's-sopco4.adb',
            's-sopco5.ads', 's-sopco5.adb',
            's-spsufi.ads', 's-spsufi.adb',
            's-stalib.ads', 's-stalib.adb',
            's-stopoo.ads', 's-stopoo.adb',
            's-stposu.ads', 's-stposu.adb',
            's-stratt.ads', 's-stratt.adb',
            's-strhas.ads', 's-strhas.adb',
            's-string.ads', 's-string.adb',
            's-tasloc.ads', 's-tasloc.adb',
            's-traceb.ads',
            's-traent.ads', 's-traent.adb',
            's-trasym.ads', 's-trasym.adb',
            's-valboo.ads', 's-valboo.adb',
            's-valcha.ads', 's-valcha.adb',
            's-valdec.ads', 's-valdec.adb',
            's-valenu.ads', 's-valenu.adb',
            's-valint.ads', 's-valint.adb',
            's-vallld.ads', 's-vallld.adb',
            's-vallli.ads', 's-vallli.adb',
            's-valllu.ads', 's-valllu.adb',
            's-valrea.ads', 's-valrea.adb',
            's-valuns.ads', 's-valuns.adb',
            's-valuti.ads', 's-valuti.adb',
            's-valwch.ads', 's-valwch.adb',
            's-veboop.ads', 's-veboop.adb',
            's-vector.ads', 's-vercon.adb',
            's-vercon.ads',
            's-wchcnv.ads', 's-wchcnv.adb',
            's-wchcon.ads', 's-wchcon.adb',
            's-wchjis.ads', 's-wchjis.adb',
            's-wchstw.ads', 's-wchstw.adb',
            's-wchwts.ads', 's-wchwts.adb',
            's-widboo.ads', 's-widboo.adb',
            's-widcha.ads', 's-widcha.adb',
            's-widenu.ads', 's-widenu.adb',
            's-widlli.ads', 's-widlli.adb',
            's-widllu.ads', 's-widllu.adb',
            's-widwch.ads', 's-widwch.adb',
            's-wwdcha.ads', 's-wwdcha.adb',
            's-wwdenu.ads', 's-wwdenu.adb',
            's-wwdwch.ads', 's-wwdwch.adb',
            'src/tconfig.h',
            'src/tsystem.h',
            'libgcc/unwind-pe.h'])
        self.update_pairs('full', {
            'a-elchha.adb': 'a-elchha-xi.adb',
            'a-excach.adb': 'a-excach-cert.adb',
            'a-except.adb': 'a-except-2005.adb',
            'a-except.ads': 'a-except-2005.ads',
            'a-exexpr.adb': 'a-exexpr-gcc.adb',
            's-flocon.adb': 's-flocon-none.adb',
            's-io.adb': 's-io-xi.adb',
            's-ransee.adb': 's-ransee-xi.adb',
            's-soflin.adb': 's-soflin-xi.adb',
            's-soflin.ads': 's-soflin-xi.ads',
            's-traceb.ads': 's-traceb-cert.ads'})

        if self._is_bb:
            self.add_sources('full', 'adaint-xi.c')
            self.update_pairs('full', {
                's-memory.adb': 's-memory-xi.adb'})
        else:
            # PikeOS
            self.update_pairs('full', {
                's-memory.ads': 's-memory-pikeos.ads',
                's-memory.adb': 's-memory-pikeos.adb',
                's-init.adb': 's-init-pikeos-ravenscar.adb'})

        # Zero-cost-exception support
        self.add_sources('full/zcx-arm', {
            's-excmac.ads': 's-excmac-arm.ads',
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})
        self.add_sources('full/zcx-dw2', [
            {'s-excmac.ads': 's-excmac-gcc.ads'},
            'libgcc/unwind-dw2-fde.h'])
        if self._is_bb:
            self.add_sources('full/zcx-dw2', 'src/unwind-dw2-fde-bb.c')

        self.add_sources('full/zcx-ppc', {
            's-traceb.adb': 's-traceb-xi-ppc.adb'})
        self.add_sources('full/zcx-leon', {
            's-traceb.adb': 's-traceb-xi-sparc.adb'})
        self.add_sources('full/zcx-x86', {
            's-traceb.adb': 's-traceb-vx653-sim.adb'})

        # Containers
        self.add_sources('containers', [
            'a-btgbso.adb', 'a-btgbso.ads',
            'a-cbdlli.adb', 'a-cbdlli.ads',
            'a-cbhama.adb', 'a-cbhama.ads',
            'a-cbhase.adb', 'a-cbhase.ads',
            'a-cbmutr.adb', 'a-cbmutr.ads',
            'a-cborma.adb', 'a-cborma.ads',
            'a-cborse.adb', 'a-cborse.ads',
            'a-cdlili.adb', 'a-cdlili.ads',
            'a-cfdlli.adb', 'a-cfdlli.ads',
            'a-cfhama.adb', 'a-cfhama.ads',
            'a-cfhase.adb', 'a-cfhase.ads',
            'a-cfinve.adb', 'a-cfinve.ads',
            'a-cforma.adb', 'a-cforma.ads',
            'a-cforse.adb', 'a-cforse.ads',
            'a-cgaaso.adb', 'a-cgaaso.ads',
            'a-cgarso.adb', 'a-cgarso.ads',
            'a-cgcaso.adb', 'a-cgcaso.ads',
            'a-chtgbk.adb', 'a-chtgbk.ads',
            'a-chtgbo.adb', 'a-chtgbo.ads',
            'a-chtgke.adb', 'a-chtgke.ads',
            'a-chtgop.adb', 'a-chtgop.ads',
            'a-cidlli.adb', 'a-cidlli.ads',
            'a-cihama.adb', 'a-cihama.ads',
            'a-cihase.adb', 'a-cihase.ads',
            'a-cimutr.adb', 'a-cimutr.ads',
            'a-ciorma.adb', 'a-ciorma.ads',
            'a-ciormu.adb', 'a-ciormu.ads',
            'a-ciorse.adb', 'a-ciorse.ads',
            'a-coboho.adb', 'a-coboho.ads',
            'a-cobove.adb', 'a-cobove.ads',
            'a-cofove.adb', 'a-cofove.ads',
            'a-cogeso.adb', 'a-cogeso.ads',
            'a-cohama.adb', 'a-cohama.ads',
            'a-cohase.adb', 'a-cohase.ads',
            'a-cohata.ads',
            'a-coinho.adb', 'a-coinho.ads',
            'a-coinve.adb', 'a-coinve.ads',
            'a-comutr.adb', 'a-comutr.ads',
            'a-conhel.adb', 'a-conhel.ads',
            'a-contai.ads',
            'a-convec.adb', 'a-convec.ads',
            'a-coorma.adb', 'a-coorma.ads',
            'a-coormu.adb', 'a-coormu.ads',
            'a-coorse.adb', 'a-coorse.ads',
            'a-coprnu.adb', 'a-coprnu.ads',
            'a-crbltr.ads',
            'a-crbtgk.adb', 'a-crbtgk.ads',
            'a-crbtgo.adb', 'a-crbtgo.ads',
            'a-crdlli.adb', 'a-crdlli.ads',
            'a-csquin.ads',
            'a-rbtgbk.adb', 'a-rbtgbk.ads',
            'a-rbtgbo.adb', 'a-rbtgbo.ads',
            'a-rbtgso.adb', 'a-rbtgso.ads',
            'a-iteint.ads',
            's-atocou.adb', 's-atocou.ads'])
        self.update_pairs('containers', {
            'a-coinho.adb': 'a-coinho-shared.adb',
            'a-coinho.ads': 'a-coinho-shared.ads',
            's-atocou.adb': 's-atocou-builtin.adb'})

        # GNARL files for the full runtime
        self.add_sources('gnarl/full', {
            's-taskin.ads': 's-taskin-xi-full.ads',
            's-tposen.adb': 's-tposen-xi-full.adb',
            's-tposen.ads': 's-tposen-xi-full.ads'})

        if self._is_bb:
            self.add_sources('gnarl/full', [
                's-btstch.ads', 's-btstch.adb'])

        # Ravenscar extended: relative delays
        self.add_sources('gnarl/full/extended', {
            's-reldel.ads': 's-reldel-xi.ads',
            's-reldel.adb': 's-reldel-xi.adb'})

        # Tasking extensions: multiple entries
        self.add_sources('gnarl/full/extended', [
            'a-synbar.adb', 'a-synbar.ads',
            'g-boubuf.adb', 'g-boubuf.ads',
            'g-boumai.ads',
            'g-semaph.adb', 'g-semaph.ads'])
        self.add_sources('gnarl/full/extended', {
            's-tpoben.ads': 's-tpoben-raven-full.ads',
            's-tpoben.adb': 's-tpoben-raven-full.adb',
            's-tasque.ads': 's-tasque-raven-full.ads',
            's-tasque.adb': 's-tasque-raven-full.adb',
            's-tpobop.ads': 's-tpobop-raven-full.ads',
            's-tpobop.adb': 's-tpobop-raven-full.adb'})

    def zfp_dirs(self, config, mem_routines, math_lib):
        """Returns the list of directories contained in a base ZFP runtime"""
        dirs = []
        dirs.append('common')
        dirs.append('zfp')

        if config.has_fpu:
            dirs.append('fpu')

        if mem_routines:
            dirs.append('mem')

        if config.is_pikeos:
            dirs.append('zfp_io')

        if math_lib:
            dirs.append('math')
            if self._is_bb:
                if config.has_single_precision_fpu:
                    dirs.append('math/hardsp')
                else:
                    dirs.append('math/softsp')
                if config.has_double_precision_fpu:
                    dirs.append('math/harddp')
                else:
                    dirs.append('math/softdp')
        return dirs

    def sfp_dirs(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base SFP runtime"""
        dirs = self.zfp_dirs(config, mem_routines, math_lib)

        if config.is_pikeos:
            dirs.remove('zfp_io')
            dirs.append('sfp_io')

        dirs.append('gnarl/common')
        dirs.append('gnarl/sfp')

        if not config.is_pikeos:
            if config.has_timer_64:
                dirs.append('gnarl/timer64')
            else:
                dirs.append('gnarl/timer32')
            if config.target.startswith('leon'):
                dirs.append('gnarl/spinlock/leon')
            else:
                dirs.append('gnarl/spinlock/gcc')
            if small_mem:
                dirs.append('gnarl/mem-small')
            else:
                dirs.append('gnarl/mem-large')

        return dirs

    def full_dirs(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base full runtime"""
        dirs = self.sfp_dirs(config, mem_routines, math_lib, small_mem)
        # remove zfp and sfp-specific files
        dirs.remove('zfp')
        dirs.remove('gnarl/sfp')
        dirs.append('full')

        cpu = config.target.split('-')[0]

        if cpu in ('arm', 'aarch64'):
            dirs.append('full/zcx-arm')
        elif cpu in ('powerpc', 'ppc'):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-ppc')
        elif cpu in ('leon', 'leon3'):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-leon')
        elif cpu in ('x86'):
            dirs.append('full/zcx-dw2')
            dirs.append('full/zcx-x86')
        else:
            print "Unexpected cpu %s" % cpu
            print "  not in 'arm', 'aarch64', 'powerpc', 'leon', 'leon3'"
            sys.exit(2)

        if not config.is_pikeos:
            if config.has_newlib:
                dirs.append('newlib')
            else:
                dirs.append('libc')

        dirs.append('containers')
        dirs.append('gnarl/full')
        dirs.append('gnarl/full/extended')

        return dirs


def build_configs(target, runtime):
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'ppc-pikeos':
        t = PpcPikeOS()
    elif target == 'x86-pikeos':
        t = X86PikeOS()
    elif target == 'zynq7000':
        t = Zynq7000()
    elif target == 'rpi2':
        t = RPI2()
    elif target == 'rpi3':
        t = RPI3()
    elif target.startswith('sam'):
        t = Sam(target)
    elif target.startswith('smartfusion2'):
        t = SmartFusion2()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target == 'openmv2':
        t = Stm32(target)
    elif target == 'tms570':
        t = TMS570()
    elif target == 'lm3s':
        t = LM3S()
    elif target == 'leon2' or target == 'leon':
        t = Leon2()
    elif target == 'leon3':
        t = Leon3()
    elif target == 'mpc8641':
        t = MPC8641()
    elif target == '8349e':
        t = MPC8349e()
    elif target == 'p2020':
        t = P2020()
    elif target == 'p5566':
        t = P5566()
    elif target == 'mpc5634':
        t = P5634()
    elif target == 'mcm':
        t = Visium()
    elif target == 'x86-linux':
        t = X86Linux()
    elif target == 'x86-windows':
        t = X86Windows()
    else:
        print 'Error: undefined target %s' % target
        sys.exit(2)

    if runtime == 'zfp':
        t.init_as_zfp()
    elif runtime == 'ravenscar-sfp':
        t.init_as_sfp()
    elif runtime == 'ravenscar-full':
        t.init_as_full()
    else:
        print 'Error: unknown runtime %s' % runtime
        sys.exit(2)

    return t


def usage():
    print "usage: build-rts.py OPTIONS runtime/arch"
    print "Options are:"
    print " -v --verbose     be verbose"
    print " --output=DIR     output directory"
    print " --gcc-dir=DIR    gcc source directory"
    print " --gnat-dir=DIR   gnat source directory"
    print " --cross-dir=DIR  cross source directory"
    print " --link           create symbolic links"
    print " --create-common  create a common runtime directory"


def main():
    # global link, gccdir, gnatdir, crossdir, verbose, create_common

    Config.install = Config.objdir

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "hvl",
            ["help", "verbose",
             "output=",
             "gcc-dir=", "gnat-dir=", "cross-dir=",
             "create-common",
             "link"])
    except getopt.GetoptError, e:
        print "error: " + str(e)
        print "Try --help"
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-v", "--verbose"):
            Config.verbose = True
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt in ("-l", "--link"):
            Config.link = True
        elif opt == "--output":
            Config.install = arg
        elif opt == "--gcc-dir":
            Config.gccdir = arg
        elif opt == "--gnat-dir":
            Config.gnatdir = arg
        elif opt == "--cross-dir":
            Config.crossdir = arg
        elif opt == "--create-common":
            Config.create_common = True
        else:
            sys.abort()

    if len(args) != 1:
        print "error: missing configuration"
        print "Try --help"
        sys.exit(2)

    (rts, arch) = args[0].split('/')

    Config.rts_srcs = SourceDirs('pikeos' not in arch)

    target = build_configs(arch, rts)
    target.install(Config.install)


if __name__ == '__main__':
    main()
