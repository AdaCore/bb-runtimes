#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

import copy
import getopt
import sys
import os
import shutil
import string
import re

# Sources directories
gnatdir = "../gnat"
gccdir = "../gcc"
crossdir = "../cross/bare_board/libbareboard"

# Output directory
objdir = "install"

# Display actions
verbose = False

link = False
config = ""


def fullpath(filename):
    """Returns the full path of filename.

    If filename is a relative path, then returns the path relative to the
    script's directory.
    """
    if os.path.isabs(filename):
        return filename
    else:
        return os.path.abspath(
            os.path.join(os.path.dirname(__file__), filename))


def readfile(filename):
    """Reads the content of filename, relative to the bb-runtimes directory"""
    fp = open(fullpath(filename), 'r')
    res = fp.read()
    fp.close()
    return res


class TargetConfiguration(object):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def is_bareboard(self):
        raise Exception("not implemented")

    @property
    def has_fpu(self):
        return self.has_single_precision_fpu or self.has_double_precision_fpu

    @property
    def has_single_precision_fpu(self):
        return self.has_double_precision_fpu

    @property
    def has_double_precision_fpu(self):
        raise Exception("not implemented")


class BaseRuntime(object):
    """List of files for a runtime.
    Each list attribute represent a subdirectory.
    The pairs dictionnary handle target specific files.
    The config_files dictionnary represent configuration (.gpr and .xml)
    files"""
    def __init__(self):
        self.bsp = []
        self.arch = []
        self.common = []
        self.gnarl_bsp = []
        self.gnarl_arch = []
        self.gnarl_common = []
        self.math = []
        self.pairs = {}
        self.config_files = {}


# Definitions of sources files.
# Keep spec and body on one line.

class BaseZFP(BaseRuntime):
    def __init__(self, config, mem_routines, math_lib):
        """Files for a zfp runtime

        :param config: a TargetConfiguration object
        :param mem_routines: whether we include memory management routines
        :param math_lib: whether we include the math library
        """
        super(BaseZFP, self).__init__()
        self.arch += [
            'system.ads',
            's-macres.adb']
        self.bsp += [
            # Implementation of s-textio is board-specific
            's-textio.adb']

        self.common += [
            'ada.ads',
            'a-elchha.ads', 'a-elchha.adb',
            'a-except.adb', 'a-except.ads',
            'a-tags.adb', 'a-tags.ads',
            'a-assert.ads', 'a-assert.adb',
            'a-textio.adb', 'a-textio.ads',
            'a-unccon.ads',
            'a-uncdea.ads',
            'text_io.ads',

            'unchconv.ads',
            'unchdeal.ads',
            'machcode.ads',

            'gnat.ads',
            'g-io.adb', 'g-io.ads',
            'g-io-put.adb',
            'g-souinf.ads',

            'interfac.ads',
            'i-c.ads', 'i-cexten.ads',
            'i-bit_types.ads',

            's-atacco.adb', 's-atacco.ads',
            's-assert.adb', 's-assert.ads',
            # 'Image support
            's-imgint.ads', 's-imgint.adb',
            's-imglli.ads', 's-imglli.adb',
            's-imgboo.ads', 's-imgboo.adb',
            's-imguns.ads', 's-imguns.adb',
            's-imgllu.ads', 's-imgllu.adb',
            's-maccod.ads',
            's-macres.ads',
            's-memory.ads', 's-memory.adb',
            's-secsta.adb', 's-secsta.ads',
            's-sssita.ads', 's-sssita.adb',
            's-textio.ads',
            's-stoele.adb', 's-stoele.ads',
            's-unstyp.ads']

        self.pairs.update({
            'system.ads': None,  # Must be overriden
            'a-tags.adb': 'a-tags-hie.adb', 'a-tags.ads': 'a-tags-hie.ads',
            'a-elchha.ads': 'a-elchha-zfp.ads',
            'a-elchha.adb': 'a-elchha-zfp.adb',
            'a-except.adb': 'a-except-zfp.adb',
            'a-except.ads': 'a-except-zfp.ads',
            'a-textio.adb': 'a-textio-zfp.adb',
            'a-textio.ads': 'a-textio-zfp.ads',
            'g-io.adb': 'g-io-zfp.adb',
            'g-io.ads': 'g-io-zfp.ads',
            'g-io-put.adb': 'g-io-put-stextio.adb',
            'i-c.ads': 'i-c-hie.ads',
            's-assert.adb': 's-assert-xi.adb',
            's-macres.adb': None,  # Must be overriden
            's-memory.ads': 's-memory-zfp.ads',
            's-memory.adb': 's-memory-zfp.adb',
            's-secsta.adb': 's-secsta-zfp.adb',
            's-secsta.ads': 's-secsta-zfp.ads',
            's-sssita.ads': 's-sssita-xi.ads',
            's-sssita.adb': 's-sssita-xi.adb',
            's-textio.adb': None,  # Must be overriden
            's-textio.ads': 's-textio-zfp.ads'})

        self.config_files.update(
            {'target_options.gpr': readfile('src/target_options.gpr')})

        if config.has_fpu:
            self.common += [
                's-fatflt.ads',
                's-fatlfl.ads',
                's-fatllf.ads',
                's-fatsfl.ads',
                's-fatgen.adb', 's-fatgen.ads']

        if mem_routines:
            self.common += [
                's-memcop.ads', 's-memcop.adb',
                's-memmov.ads', 's-memmov.adb',
                's-memset.ads', 's-memset.adb',
                's-memcom.ads', 's-memcom.adb']
            self.pairs.update(
                {'s-memcop.ads': 's-memcop-zfp.ads',
                 's-memcop.adb': 's-memcop-zfp.adb'})

        if math_lib:
            self.math += [
                'a-ncelfu.ads',
                'a-ngcefu.adb', 'a-ngcefu.ads',
                'a-ngcoar.adb', 'a-ngcoar.ads',
                'a-ngcoty.adb', 'a-ngcoty.ads',
                'a-ngelfu.adb', 'a-ngelfu.ads',
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
                's-exnllf.adb', 's-exnllf.ads',
                's-gearop.adb', 's-gearop.ads',
                # libm files:
                's-gcmain.ads', 's-gcmain.adb',
                's-libm.ads', 's-libm.adb',
                's-libsin.adb', 's-libsin.ads',
                's-libdou.adb', 's-libdou.ads',
                's-libpre.ads',
                's-lisisq.ads', 's-lisisq.adb',
                's-lidosq.ads', 's-lidosq.adb']
            self.pairs.update({
                'a-ngcoty.adb': 'a-ngcoty-ada.adb',
                'a-ngelfu.adb': 'a-ngelfu-ada.adb',
                'a-ngelfu.ads': 'a-ngelfu-ada.ads',
                'a-nlelfu.ads': 'a-nlelfu-ada.ads',
                'a-nuelfu.ads': 'a-nuelfu-ada.ads',
                'a-numaux.ads': 'a-numaux-ada.ads',
                's-gcmain.ads': 's-gcmain-ada.ads',
                's-gcmain.adb': 's-gcmain-ada.adb',
                's-libm.ads': 's-libm-ada.ads',
                's-libm.adb': 's-libm-ada.adb',
                's-libsin.adb': 's-libsin-ada.adb',
                's-libsin.ads': 's-libsin-ada.ads',
                's-libdou.adb': 's-libdou-ada.adb',
                's-libdou.ads': 's-libdou-ada.ads',
                's-libpre.ads': 's-libpre-ada.ads',
                's-lisisq.ads': 's-lisisq-ada.ads',
                's-lidosq.ads': 's-lidosq-ada.ads'})
            if config.has_fpu:
                self.pairs.update({
                    's-lisisq.adb': 's-lisisq-fpu.adb'})
            else:
                self.pairs.update({
                    's-lisisq.adb': 's-lisisq-ada.adb'})
            if config.has_double_precision_fpu:
                self.pairs.update({
                    's-lidosq.adb': 's-lidosq-fpu.adb'})
            else:
                self.pairs.update({
                    's-lidosq.adb': 's-lidosq-ada.adb'})

    @property
    def merge_libgnarl(self):
        return True


class BaseRavenscarSFP(BaseZFP):
    "Ravenscar SFP runtimes"

    def __init__(self, config, mem_routines, math_lib):
        """Files for a Ravenscar SFP runtime

        :param config: a TargetConfiguration object
        :param mem_routines: whether we include memory management routines
        :param math_lib: whether we include the math library
        """
        super(BaseRavenscarSFP, self).__init__(config, mem_routines, math_lib)

        self.common += [
            's-parame.adb', 's-parame.ads']
        self.gnarl_bsp += [
            'a-intnam.ads']
        self.gnarl_common += [
            'a-interr.adb', 'a-interr.ads',
            'a-reatim.adb', 'a-reatim.ads',
            'a-retide.adb', 'a-retide.ads',
            'a-sytaco.adb', 'a-sytaco.ads',
            'a-taside.adb', 'a-taside.ads',
            's-interr.adb', 's-interr.ads',
            's-multip.ads', 's-multip.adb',
            's-mufalo.ads', 's-mufalo.adb',
            's-musplo.ads', 's-musplo.adb',
            's-osinte.ads',
            's-taprob.adb', 's-taprob.ads',
            's-taprop.adb', 's-taprop.ads',
            's-tarest.adb', 's-tarest.ads',
            's-tasdeb.adb', 's-tasdeb.ads',
            's-tasinf.adb', 's-tasinf.ads',
            's-taskin.adb', 's-taskin.ads',
            's-taspri.ads',
            's-tasres.ads',
            'a-taster.adb', 'a-taster.ads',
            's-tposen.adb', 's-tposen.ads',
            's-tpobmu.adb', 's-tpobmu.ads']
        self.pairs.update({
            'a-intnam.ads': None,
            'a-interr.adb': 'a-interr-raven.adb',
            'a-reatim.ads': 'a-reatim-xi.ads',
            'a-reatim.adb': 'a-reatim-xi.adb',
            'a-retide.adb': 'a-retide-raven.adb',
            'a-sytaco.ads': 'a-sytaco-xi.ads',
            'a-sytaco.adb': 'a-sytaco-xi.adb',
            'a-taside.adb': 'a-taside-raven.adb',
            's-taspri.ads': 's-taspri-xi.ads',
            'a-taster.adb': 'a-taster-raven.adb',
            'a-taster.ads': 'a-taster-raven.ads',
            's-interr.ads': 's-interr-raven.ads',
            's-interr.adb': 's-interr-xi.adb',
            's-osinte.ads': 's-osinte-bb.ads',
            's-parame.adb': 's-parame-xi.adb',
            's-parame.ads': 's-parame-xi.ads',
            's-macres.adb': 's-macres-native.adb',
            's-multip.ads': 's-multip-raven-default.ads',
            's-multip.adb': 's-multip-raven-default.adb',
            's-taprob.adb': 's-taprob-raven.adb',
            's-taprob.ads': 's-taprob-raven.ads',
            's-taprop.ads': 's-taprop-xi.ads',
            's-taprop.adb': 's-taprop-xi.adb',
            's-tarest.adb': 's-tarest-raven.adb',
            's-tasdeb.ads': 's-tasdeb-xi.ads',
            's-tasdeb.adb': 's-tasdeb-raven.adb',
            's-taskin.adb': 's-taskin-raven.adb',
            's-taskin.ads': 's-taskin-raven.ads',
            's-tposen.adb': 's-tposen-raven.adb',
            's-tposen.ads': 's-tposen-raven.ads'})

        self.config_files.update(
            {'ravenscar_build.gpr': readfile('src/ravenscar_build.gpr')})

        if config.is_bareboard:
            self.gnarl_arch += [
                's-bbpara.ads']
            self.gnarl_common += [
                'a-exetim.ads', 'a-exetim.adb',
                'a-extiin.ads', 'a-extiin.adb',
                'a-rttiev.ads', 'a-rttiev.adb',
                's-bb.ads',
                's-bbbosu.ads', 's-bbbosu.adb',
                's-bbcppr.ads', 's-bbcppr.adb',
                's-bbexti.ads', 's-bbexti.adb',
                's-bbinte.ads', 's-bbinte.adb',
                's-bbprot.ads', 's-bbprot.adb',
                's-bbthqu.ads', 's-bbthqu.adb',
                's-bbthre.ads', 's-bbthre.adb',
                's-bbtiev.ads', 's-bbtiev.adb',
                's-bbtime.ads', 's-bbtime.adb',
                's-bcprmu.ads', 's-bcprmu.adb']
            self.pairs.update({
                'a-exetim.ads': 'a-exetim-bb.ads',
                'a-exetim.adb': 'a-exetim-bb.adb',
                'a-extiin.ads': 'a-extiin-bb.ads',
                'a-extiin.adb': 'a-extiin-bb.adb',
                'a-rttiev.ads': 'a-rttiev-bb.ads',
                'a-rttiev.adb': 'a-rttiev-bb.adb',
                's-multip.ads': 's-multip-bb.ads',
                's-multip.adb': 's-multip-raven-default.adb',
                's-tpobmu.adb': 's-tpobmu-bb.adb',
                's-bbpara.ads': None})

    @property
    def merge_libgnarl(self):
        return False


class BaseRavenscarFull(BaseRavenscarSFP):
    def __init__(self, config, mem_routines, libc_files, arm_zcx):
        super(BaseRavenscarFull, self).__init__(
            config, mem_routines, math_lib=True)

        # Use standard version of the following files:
        del self.pairs['a-tags.ads']
        del self.pairs['a-tags.adb']
        del self.pairs['a-elchha.ads']
        del self.pairs['s-assert.adb']
        del self.pairs['s-memory.ads']

        # Disable single task secondary stack for ravenscar-full
        self.common.remove('s-sssita.ads')
        self.common.remove('s-sssita.adb')

        if config.is_bareboard:
            self.gnarl_common += [
                's-btstch.ads', 's-btstch.adb']
            self.common += [
                'adaint-xi.c']

        if libc_files:
            self.common += [
                's-c.ads',
                's-cerrno.ads', 's-cerrno.adb',
                's-cmallo.ads', 's-cmallo.adb',
                's-cstrle.ads', 's-cstrle.adb']
            self.pairs.update({
                's-c.ads': 's-c-zfp.ads',
                's-cerrno.ads': 's-cerrno-zfp.ads',
                's-cerrno.adb': 's-cerrno-zfp.adb',
                's-cmallo.ads': 's-cmallo-zfp.ads',
                's-cmallo.adb': 's-cmallo-zfp.adb',
                's-cstrle.ads': 's-cstrle-zfp.ads',
                's-cstrle.adb': 's-cstrle-zfp.adb'})

        # Add containers
        self.common += [
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
            's-atocou.adb', 's-atocou.ads']

        # Files common to the Full Ravenscar runtimes
        self.common += [
            'a-chahan.adb', 'a-chahan.ads',
            'a-charac.ads',
            'a-chlat1.ads',
            'a-chlat9.ads',
            'a-cwila1.ads',
            'a-cwila9.ads',
            'a-decima.adb', 'a-decima.ads',
            'a-einuoc.adb', 'a-einuoc.ads',
            'a-exctra.adb', 'a-exctra.ads',
            'a-finali.adb', 'a-finali.ads',
            'a-ioexce.ads',
            'a-nudira.adb', 'a-nudira.ads',
            'a-nuflra.adb', 'a-nuflra.ads',
            'a-stmaco.ads',
            'a-storio.adb', 'a-storio.ads',
            'a-strbou.adb', 'a-strbou.ads',
            'a-stream.adb', 'a-stream.ads',
            'a-strfix.adb', 'a-strfix.ads',
            'a-string.ads',
            'a-strmap.adb', 'a-strmap.ads',
            'a-strsea.adb', 'a-strsea.ads',
            'a-strsup.adb', 'a-strsup.ads',
            'a-strunb.adb', 'a-strunb.ads',
            'a-stunau.adb', 'a-stunau.ads',
            'a-stwibo.adb', 'a-stwibo.ads',
            'a-stwifi.adb', 'a-stwifi.ads',
            'a-stwima.adb', 'a-stwima.ads',
            'a-stwise.adb', 'a-stwise.ads',
            'a-stwisu.adb', 'a-stwisu.ads',
            'a-stwiun.adb', 'a-stwiun.ads',
            'a-swmwco.ads',
            'a-undesu.adb', 'a-undesu.ads',

            'g-arrspl.adb', 'g-arrspl.ads',
            'g-bubsor.adb', 'g-bubsor.ads',
            'g-busora.adb', 'g-busora.ads',
            'g-busorg.adb', 'g-busorg.ads',
            'g-bytswa.adb', 'g-bytswa.ads',
            'g-casuti.adb', 'g-casuti.ads',
            'g-comver.adb', 'g-comver.ads',
            'g-crc32.adb',  'g-crc32.ads',
            'g-debuti.adb', 'g-debuti.ads',
            'g-except.ads',
            'g-heasor.adb', 'g-heasor.ads',
            'g-hesora.adb', 'g-hesora.ads',
            'g-hesorg.adb', 'g-hesorg.ads',
            'g-htable.adb', 'g-htable.ads',
            'g-md5.adb',    'g-md5.ads',
            'g-moreex.adb', 'g-moreex.ads',
            'g-regexp.ads',
            'g-sechas.adb', 'g-sechas.ads',
            'g-sehamd.adb', 'g-sehamd.ads',
            'g-sehash.adb', 'g-sehash.ads',
            'g-sha1.adb',   'g-sha1.ads',
            'g-sha224.ads',
            'g-sha256.ads',
            'g-sha384.ads',
            'g-sha512.ads',
            'g-shsh32.adb', 'g-shsh32.ads',
            'g-shsh64.adb', 'g-shsh64.ads',
            'g-shshco.adb', 'g-shshco.ads',
            'g-string.ads',
            'g-strspl.ads',
            'g-table.adb',  'g-table.ads',
            'g-tasloc.ads',
            'g-wistsp.ads',

            'i-cobol.adb',  'i-cobol.ads',
            'i-fortra.adb', 'i-fortra.ads',
            'i-pacdec.adb', 'i-pacdec.ads',

            'ioexcept.ads',

            's-addima.adb', 's-addima.ads',
            's-addope.adb', 's-addope.ads',
            's-arit64.adb', 's-arit64.ads',
            's-bitops.adb', 's-bitops.ads',
            's-boarop.ads',
            's-bytswa.ads',
            's-carsi8.adb', 's-carsi8.ads',
            's-carun8.adb', 's-carun8.ads',
            's-casi16.adb', 's-casi16.ads',
            's-casi32.adb', 's-casi32.ads',
            's-casi64.adb', 's-casi64.ads',
            's-casuti.adb', 's-casuti.ads',
            's-caun16.adb', 's-caun16.ads',
            's-caun32.adb', 's-caun32.ads',
            's-caun64.adb', 's-caun64.ads',
            's-chepoo.ads',
            's-crc32.adb',  's-crc32.ads',
            's-except.adb', 's-except.ads',
            's-excdeb.adb', 's-excdeb.ads',
            's-exctab.adb', 's-exctab.ads',
            's-exnint.adb', 's-exnint.ads',
            's-exnlli.adb', 's-exnlli.ads',
            's-expint.adb', 's-expint.ads',
            's-explli.adb', 's-explli.ads',
            's-expllu.adb', 's-expllu.ads',
            's-expmod.adb', 's-expmod.ads',
            's-expuns.adb', 's-expuns.ads',
            's-finmas.adb', 's-finmas.ads',
            's-finroo.ads', 's-finroo.adb',
            's-flocon.adb', 's-flocon.ads',
            's-fore.adb',   's-fore.ads',
            's-geveop.adb', 's-geveop.ads',
            's-htable.adb', 's-htable.ads',
            's-imenne.adb', 's-imenne.ads',
            's-imgbiu.adb', 's-imgbiu.ads',
            's-imgcha.adb', 's-imgcha.ads',
            's-imgdec.adb', 's-imgdec.ads',
            's-imgenu.adb', 's-imgenu.ads',
            's-imgllb.adb', 's-imgllb.ads',
            's-imglld.adb', 's-imglld.ads',
            's-imgllw.adb', 's-imgllw.ads',
            's-imgrea.adb', 's-imgrea.ads',
            's-imgwch.adb', 's-imgwch.ads',
            's-imgwiu.adb', 's-imgwiu.ads',
            's-init.adb',   's-init.ads',
            's-io.adb',     's-io.ads',
            's-mantis.adb', 's-mantis.ads',
            's-mastop.adb', 's-mastop.ads',
            's-pack03.adb', 's-pack03.ads',
            's-pack05.adb', 's-pack05.ads',
            's-pack06.adb', 's-pack06.ads',
            's-pack07.adb', 's-pack07.ads',
            's-pack09.adb', 's-pack09.ads',
            's-pack10.adb', 's-pack10.ads',
            's-pack11.adb', 's-pack11.ads',
            's-pack12.adb', 's-pack12.ads',
            's-pack13.adb', 's-pack13.ads',
            's-pack14.adb', 's-pack14.ads',
            's-pack15.adb', 's-pack15.ads',
            's-pack17.adb', 's-pack17.ads',
            's-pack18.adb', 's-pack18.ads',
            's-pack19.adb', 's-pack19.ads',
            's-pack20.adb', 's-pack20.ads',
            's-pack21.adb', 's-pack21.ads',
            's-pack22.adb', 's-pack22.ads',
            's-pack23.adb', 's-pack23.ads',
            's-pack24.adb', 's-pack24.ads',
            's-pack25.adb', 's-pack25.ads',
            's-pack26.adb', 's-pack26.ads',
            's-pack27.adb', 's-pack27.ads',
            's-pack28.adb', 's-pack28.ads',
            's-pack29.adb', 's-pack29.ads',
            's-pack30.adb', 's-pack30.ads',
            's-pack31.adb', 's-pack31.ads',
            's-pack33.adb', 's-pack33.ads',
            's-pack34.adb', 's-pack34.ads',
            's-pack35.adb', 's-pack35.ads',
            's-pack36.adb', 's-pack36.ads',
            's-pack37.adb', 's-pack37.ads',
            's-pack38.adb', 's-pack38.ads',
            's-pack39.adb', 's-pack39.ads',
            's-pack40.adb', 's-pack40.ads',
            's-pack41.adb', 's-pack41.ads',
            's-pack42.adb', 's-pack42.ads',
            's-pack43.adb', 's-pack43.ads',
            's-pack44.adb', 's-pack44.ads',
            's-pack45.adb', 's-pack45.ads',
            's-pack46.adb', 's-pack46.ads',
            's-pack47.adb', 's-pack47.ads',
            's-pack48.adb', 's-pack48.ads',
            's-pack49.adb', 's-pack49.ads',
            's-pack50.adb', 's-pack50.ads',
            's-pack51.adb', 's-pack51.ads',
            's-pack52.adb', 's-pack52.ads',
            's-pack53.adb', 's-pack53.ads',
            's-pack54.adb', 's-pack54.ads',
            's-pack55.adb', 's-pack55.ads',
            's-pack56.adb', 's-pack56.ads',
            's-pack57.adb', 's-pack57.ads',
            's-pack58.adb', 's-pack58.ads',
            's-pack59.adb', 's-pack59.ads',
            's-pack60.adb', 's-pack60.ads',
            's-pack61.adb', 's-pack61.ads',
            's-pack62.adb', 's-pack62.ads',
            's-pack63.adb', 's-pack63.ads',
            's-pooglo.adb', 's-pooglo.ads',
            's-pooloc.adb', 's-pooloc.ads',
            's-poosiz.adb', 's-poosiz.ads',
            's-powtab.ads',
            's-rannum.adb', 's-rannum.ads',
            's-ransee.adb', 's-ransee.ads',
            's-regexp.adb', 's-regexp.ads',
            's-restri.adb', 's-restri.ads',
            's-rident.ads',
            's-scaval.adb', 's-scaval.ads',
            's-soflin.adb', 's-soflin.ads',
            's-sopco3.adb', 's-sopco3.ads',
            's-sopco4.adb', 's-sopco4.ads',
            's-sopco5.adb', 's-sopco5.ads',
            's-spsufi.adb', 's-spsufi.ads',
            's-stalib.adb', 's-stalib.ads',
            's-stopoo.adb', 's-stopoo.ads',
            's-stposu.adb', 's-stposu.ads',
            's-stratt.adb', 's-stratt.ads',
            's-strhas.adb', 's-strhas.ads',
            's-string.adb', 's-string.ads',
            's-tasloc.adb', 's-tasloc.ads',
            's-traceb.adb', 's-traceb.ads',
            's-traent.adb', 's-traent.ads',
            's-trasym.adb', 's-trasym.ads',
            's-valboo.adb', 's-valboo.ads',
            's-valcha.adb', 's-valcha.ads',
            's-valdec.adb', 's-valdec.ads',
            's-valenu.adb', 's-valenu.ads',
            's-valint.adb', 's-valint.ads',
            's-vallld.adb', 's-vallld.ads',
            's-vallli.adb', 's-vallli.ads',
            's-valllu.adb', 's-valllu.ads',
            's-valrea.adb', 's-valrea.ads',
            's-valuns.adb', 's-valuns.ads',
            's-valuti.adb', 's-valuti.ads',
            's-valwch.adb', 's-valwch.ads',
            's-veboop.adb', 's-veboop.ads',
            's-vector.ads',
            's-vercon.adb', 's-vercon.ads',
            's-wchcnv.adb', 's-wchcnv.ads',
            's-wchcon.adb', 's-wchcon.ads',
            's-wchjis.adb', 's-wchjis.ads',
            's-wchstw.adb', 's-wchstw.ads',
            's-wchwts.adb', 's-wchwts.ads',
            's-widboo.adb', 's-widboo.ads',
            's-widcha.adb', 's-widcha.ads',
            's-widenu.adb', 's-widenu.ads',
            's-widlli.adb', 's-widlli.ads',
            's-widllu.adb', 's-widllu.ads',
            's-widwch.adb', 's-widwch.ads',
            's-wwdcha.adb', 's-wwdcha.ads',
            's-wwdenu.adb', 's-wwdenu.ads',
            's-wwdwch.adb', 's-wwdwch.ads',
            'a-excach.adb',
            'a-excpol.adb',
            'a-exexda.adb',
            'a-exextr.adb',
            'a-exexpr.adb',
            'a-exstat.adb',
            's-excmac.ads',
            'raise-gcc.c',
            'raise.h',
            'src/tconfig.h',
            'src/tsystem.h',
            'libgcc/unwind-pe.h']
        if not arm_zcx:
            self.common += [
                'libgcc/unwind-dw2-fde.h']
            if config.is_bareboard:
                self.common += [
                    'src/unwind-dw2-fde-bb.c']

        self.pairs.update(
            {'a-elchha.adb': 'a-elchha-xi.adb',
             'a-excach.adb': 'a-excach-cert.adb',
             'a-except.adb': 'a-except-2005.adb',
             'a-except.ads': 'a-except-2005.ads',
             'a-exexpr.adb': 'a-exexpr-gcc.adb',
             's-io.adb': 's-io-xi.adb',
             's-memory.adb': 's-memory-xi.adb',
             's-ransee.adb': 's-ransee-xi.adb',
             's-soflin.adb': 's-soflin-xi.adb',
             's-soflin.ads': 's-soflin-xi.ads',
             's-taskin.ads': 's-taskin-xi-full.ads',
             's-tposen.adb': 's-tposen-xi-full.adb',
             's-tposen.ads': 's-tposen-xi-full.ads',
             's-traceb.ads': 's-traceb-cert.ads',
             's-atocou.adb': 's-atocou-builtin.adb',
             'a-coinho.ads': 'a-coinho-shared.ads',
             'a-coinho.adb': 'a-coinho-shared.adb'})

        if arm_zcx:
            self.pairs.update({'s-excmac.ads': 's-excmac-arm.ads'})
        else:
            self.pairs.update({'s-excmac.ads': 's-excmac-gcc.ads'})

    @property
    def merge_libgnarl(self):
        return False


class Target(TargetConfiguration):
    """Handles the creation of runtimes for a particular target"""

    def __init__(self, mem_routines, libc_files, arm_zcx):
        """Initialize the target
        :param mem_routines: True for adding memory functions (memcpy..)
        :param libc_files: True for adding libc functions (malloc...)
        :param arm_zcx: True to use arm unwind mechanism

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        self._mem_routines = mem_routines
        self._libc_files = libc_files
        self._arm_zcx = arm_zcx
        self.bsp = None
        self.arch = None
        self.common = None
        self.gnarl_bsp = None
        self.gnarl_arch = None
        self.gnarl_common = None
        self.math = None
        self.pairs = None
        self.config_files = None
        self.installed_files = []
        self.build_flags = {'target': 'unknown',
                            'source_dirs': None,
                            'common_flags': ['-fcallgraph-info=su,da',
                                             '-ffunction-sections',
                                             '-fdata-sections'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc']}

    def amend_zfp(self):
        pass

    def amend_ravenscar_sfp(self):
        self.amend_zfp()

    def amend_ravenscar_full(self):
        self.amend_ravenscar_sfp()

    def __init_from_base(self, base_runtime):
        self.bsp = copy.deepcopy(base_runtime.bsp)
        self.arch = copy.deepcopy(base_runtime.arch)
        self.common = copy.deepcopy(base_runtime.common)
        self.gnarl_bsp = copy.deepcopy(base_runtime.gnarl_bsp)
        self.gnarl_arch = copy.deepcopy(base_runtime.gnarl_arch)
        self.gnarl_common = copy.deepcopy(base_runtime.gnarl_common)
        self.math = copy.deepcopy(base_runtime.math)
        self.pairs = copy.deepcopy(base_runtime.pairs)
        self.config_files = copy.deepcopy(base_runtime.config_files)
        self._merge_libgnarl = base_runtime.merge_libgnarl

    def init_as_zfp(self):
        self.__init_from_base(
            BaseZFP(self, self._mem_routines, math_lib=False))
        self.amend_zfp()

    def init_as_sfp(self):
        self.__init_from_base(
            BaseRavenscarSFP(self, self._mem_routines, math_lib=False))
        self.amend_ravenscar_sfp()

    def init_as_full(self):
        self.__init_from_base(
            BaseRavenscarFull(
                self, self._mem_routines, self._libc_files, self._arm_zcx))
        self.amend_ravenscar_full()

    def _copy(self, src, dst):
        "Copy (or symlink) src to dst"

        if not os.path.isfile(src):
            print "runtime file " + src + " does not exists"
            sys.exit(4)
        if os.path.isfile(dst):
            print "runtime file " + dst + " already exists"
            sys.exit(5)
        if os.path.basename(dst) in self.installed_files:
            print "runtime file " + dst + " installed multiple times"
            sys.exit(6)

        self.installed_files.append(os.path.basename(dst))

        if verbose:
            print "copy " + src + " to " + dst
        if link:
            try:
                os.symlink(os.path.abspath(src), dst)
            except os.error, e:
                print "symlink error for " + src
                print "msg: " + str(e)
                sys.exit(2)
        else:
            shutil.copy(src, dst)

    def _copy_pair(self, srcfile, destdir):
        "Copy after substitution with pairs"

        dst = os.path.join(destdir, os.path.basename(srcfile))

        # Find the pair filename
        if srcfile in self.pairs:
            pair = self.pairs[srcfile]
            if not pair:
                print "Error: undefined pair for %s" % srcfile
                sys.exit(2)
            srcfile = pair

        # Find the sourcedir
        if '/' not in srcfile:
            # Files without path elements are in gnat
            self._copy(os.path.join(gnatdir, srcfile), dst)
        else:
            for d in [os.path.dirname(__file__), gccdir, crossdir]:
                src = os.path.join(d, srcfile)
                if os.path.exists(src):
                    self._copy(src, dst)
                    return
            print "Cannot find source dir for %s" % srcfile
            sys.exit(2)

    def install(self, destination):
        assert self.common is not None, "Uninitialized Target object"

        # Build target directories
        destination = fullpath(destination)
        os.mkdir(destination)
        all_src_dirs = ['bsp', 'arch', 'common', 'math',
                        'gnarl_bsp', 'gnarl_common', 'gnarl_arch']
        self.installed_files = []
        src_dirs = []
        gnarl_dirs = []
        gnat_dirs = []
        for d in all_src_dirs:
            l = getattr(self, d)
            dirname = None
            if l:
                if self._merge_libgnarl and d.startswith('gnarl_'):
                    dirname = d.replace('gnarl_', '')
                else:
                    dirname = string.replace(d, '_', '-')

                if dirname not in src_dirs:
                    src_dirs.append(dirname)
                    if dirname.startswith('gnarl'):
                        gnarl_dirs.append(dirname)
                    else:
                        gnat_dirs.append(dirname)
                subdir = os.path.join(destination, dirname)

                if not os.path.exists(subdir):
                    os.mkdir(subdir)

                for f in l:
                    self._copy_pair(f, subdir)

        for d in ['obj', 'adalib']:
            os.mkdir(os.path.join(destination, d))

        # Generate ada_source_path
        with open(os.path.join(destination, 'ada_source_path'), 'w') as fp:
            for d in src_dirs:
                fp.write(d + '\n')

        # Generate ada_object_path
        with open(os.path.join(destination, 'ada_object_path'), 'w') as fp:
            fp.write('adalib\n')

        # Write config files
        for name, content in self.config_files.iteritems():
            fp = open(os.path.join(destination, name), 'w')
            fp.write(content)
            fp.close()

        # Add the project files
        cnt = readfile(fullpath('src/new_runtime_build.gpr'))
        # Set source_dirs
        self.build_flags['source_dirs'] = '", "'.join(gnat_dirs)
        # Flags array are converted to a string
        for f in ['common_flags', 'asm_flags', 'c_flags']:
            self.build_flags[f] = '",\n        "'.join(self.build_flags[f])
        # Format
        cnt = cnt.format(**self.build_flags)
        # Write
        fp = open(os.path.join(destination, 'runtime_build.gpr'), 'w')
        fp.write(cnt)
        fp.close()

        if len(gnarl_dirs) > 0:
            fp = open(fullpath('src/ravenscar_build.gpr'), 'r')
            cnt = ''
            for line in fp:
                if line.strip().startswith('for Source_Dirs'):
                    cnt += '  for Source_Dirs use ("%s");\n' % (
                        '", "'.join(gnarl_dirs))
                else:
                    cnt += line
            fp.close()
            fp = open(os.path.join(destination, 'ravenscar_build.gpr'), 'w')
            fp.write(cnt)
            fp.close()


class PikeOS(Target):
    @property
    def is_bareboard(self):
        return False

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    def __init__(self, arm_zcx):
        super(PikeOS, self).__init__(
            mem_routines=True,
            libc_files=False,
            arm_zcx=arm_zcx)

    def init_as_full(self):
        super(PikeOS, self).init_as_full()
        self._merge_libgnarl = False

    def amend_zfp(self):
        super(PikeOS, self).amend_zfp()
        self.arch += ['arm/pikeos/memory.ld']
        self.pairs.update({
            's-textio.adb': 's-textio-pikeos.adb',
            's-macres.adb': 's-macres-native.adb',
            's-memory.ads': 's-memory-zfp.ads',
            's-memory.adb': 's-memory-raven-min.adb'})
        self.arch += ['pikeos-cert-app.c']
        # Children should add a pair for system.ads and read runtime.xml

    def amend_ravenscar_sfp(self):
        super(PikeOS, self).amend_ravenscar_sfp()
        self.arch += [
            'adaint-pikeos.c']
        self.gnarl_common += [
            's-osinte.adb']
        self.pairs.update({
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-taprop.adb': 's-taprop-pikeos.adb',
            's-init.adb': 's-init-pikeos-ravenscar.adb',
            'a-textio.adb': 'a-textio-raven.adb',  # Replaced
            's-flocon.adb': 's-flocon-none.adb'})

    def amend_ravenscar_full(self):
        super(PikeOS, self).amend_ravenscar_full()
        self.pairs.update({
            's-memory.ads': 's-memory-pikeos.ads',
            's-memory.adb': 's-memory-pikeos.adb'})
        # Register ZCX frames (for pikeos-cert-app.c)
        self.build_flags['c_flags'] += ['-DUSE_ZCX']


class PikeOS3(PikeOS):
    def amend_zfp(self):
        super(PikeOS3, self).amend_zfp()
        # Don't use function/data sections, not supported by linker script
        self.build_flags['common_flags'] = \
            filter(lambda x: x not in ['-ffunction-sections',
                                       '-fdata-sections'],
                   self.build_flags['common_flags'])

    def amend_ravenscar_sfp(self):
        super(PikeOS3, self).amend_ravenscar_sfp()
        self.pairs.update({
            's-interr.adb': 's-interr-pikeos.adb',
            's-osinte.ads': 's-osinte-pikeos.ads',
            's-osinte.adb': 's-osinte-pikeos.adb'})


class PikeOS4(PikeOS):
    def amend_ravenscar_sfp(self):
        super(PikeOS4, self).amend_ravenscar_sfp()
        self.pairs.update({
            's-interr.adb': 's-interr-pikeos4.adb',
            's-osinte.ads': 's-osinte-pikeos4.ads',
            's-osinte.adb': 's-osinte-pikeos4.adb'})


class ArmPikeOS(PikeOS4):
    def __init__(self):
        super(ArmPikeOS, self).__init__(arm_zcx=True)
        self.build_flags['target'] = 'arm-pikeos'

    def amend_zfp(self):
        super(ArmPikeOS, self).amend_zfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-arm.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(ArmPikeOS, self).amend_ravenscar_sfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-arm-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(ArmPikeOS, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-pikeos-arm-ravenscar-full.ads'})
        self.pairs.update({
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})


class PpcPikeOS(PikeOS3):
    def __init__(self):
        super(PpcPikeOS, self).__init__(arm_zcx=False)
        self.build_flags['target'] = 'ppc-pikeos'

    def amend_zfp(self):
        super(PpcPikeOS, self).amend_zfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-ppc.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/pikeos3/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(PpcPikeOS, self).amend_ravenscar_sfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-ppc-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(PpcPikeOS, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-pikeos-ppc-ravenscar-full.ads'})
        self.pairs.update({
            's-traceb.adb': 's-traceb-xi-ppc.adb'})


class X86PikeOS(PikeOS3):
    def __init__(self):
        super(X86PikeOS, self).__init__(arm_zcx=False)
        self.build_flags['target'] = 'x86-pikeos'

    def amend_zfp(self):
        super(X86PikeOS, self).amend_zfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-x86.ads'})
        # FIXME: use an i586 specific runtime.xml ?
        self.config_files.update(
            {'runtime.xml': readfile('arm/pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(X86PikeOS, self).amend_ravenscar_sfp()
        self.pairs.update({
            'system.ads': 'system-pikeos-x86-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(X86PikeOS, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-pikeos-x86-ravenscar-full.ads'})
        self.pairs.update({
            's-traceb.adb': 's-traceb-vx653-sim.adb'})


class ArmBBTarget(Target):
    @property
    def is_bareboard(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return False

    def __init__(self):
        super(ArmBBTarget, self).__init__(
            mem_routines=True,
            libc_files=True,
            arm_zcx=True)
        self.build_flags['target'] = 'arm-eabi'

    def amend_zfp(self):
        super(ArmBBTarget, self).amend_zfp()
        self.pairs.update({
            'system.ads': 'system-xi-arm.ads',
            's-macres.adb': 's-macres-cortexm3.adb'})

    def amend_ravenscar_sfp(self):
        super(ArmBBTarget, self).amend_ravenscar_sfp()
        self.pairs.update({
            'system.ads': 'system-xi-cortexm4-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-armv7m.adb',
            's-bbbosu.adb': 's-bbbosu-armv7m.adb',
            's-parame.ads': 's-parame-xi-small.ads',
            's-flocon.adb': 's-flocon-none.adb'})

    def amend_ravenscar_full(self):
        super(ArmBBTarget, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-xi-cortexm4-full.ads',
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})


class Stm32(ArmBBTarget):
    _to_mcu = {
        'stm32f4': 'stm32f40x',
        'stm32f429disco': 'stm32f429x',
        'stm32f469disco': 'stm32f469x',
        'stm32f7disco': 'stm32f7x'}

    def __init__(self, board):
        super(Stm32, self).__init__()

        assert board in Stm32._to_mcu, 'Unknown STM32 board: %s' % board
        self.mcu = Stm32._to_mcu[board]

    def amend_zfp(self):
        super(Stm32, self).amend_zfp()
        self.common += [
            's-bb.ads']

        if 's-bbpara.ads' not in self.gnarl_arch:
            self.gnarl_arch.append('s-bbpara.ads')

        self.bsp += [
            's-stm32.ads',
            's-stm32.adb',
            'arm/stm32/common-RAM.ld',
            'arm/stm32/common-ROM.ld',
            'arm/stm32/start-rom.S',
            'arm/stm32/start-ram.S',
            'arm/stm32/start-common.S',
            'arm/stm32/setup_pll.adb',
            'arm/stm32/%s/memory-map.ld' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.ads' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.adb' % self.mcu,
            'arm/stm32/%s/s-bbbopa.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-flash.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-gpio.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-pwr.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-rcc.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-syscfg.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-usart.ads' % self.mcu]

        self.pairs.update({
            's-bbpara.ads': 's-bbpara-stm32f4.ads'})

        if self.mcu == 'stm32f40x':
            self.pairs.update({
                's-stm32.adb': 's-stm32-f40x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.mcu == 'stm32f429x':
            self.pairs.update({
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.mcu == 'stm32f469x':
            self.pairs.update({
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f469.adb'})
        elif self.mcu == 'stm32f7x':
            self.pairs.update({
                's-stm32.adb': 's-stm32-f7x.adb',
                's-textio.adb': 's-textio-stm32f7.adb'})

        self.config_files.update(
            {'runtime.xml': readfile('arm/stm32/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(Stm32, self).amend_ravenscar_sfp()
        self.gnarl_common.remove('s-bb.ads')

        self.bsp += [
            'arm/stm32/%s/svd/handler.S' % self.mcu]
        self.pairs.update({
            'a-intnam.ads': 'arm/stm32/%s/svd/a-intnam.ads' % self.mcu})


class Sam(ArmBBTarget):
    def __init__(self, board):
        super(Sam, self).__init__()

        assert board in ('sam4s', 'samg55'), 'Unknown SAM board: %s' % board
        self.board = board

    @property
    def has_single_precision_fpu(self):
        return self.board != 'sam4s'

    def amend_zfp(self):
        super(Sam, self).amend_zfp()
        self.bsp += [
            's-sam4s.ads',
            'arm/sam/common-SAMBA.ld',
            'arm/sam/common-ROM.ld',
            'arm/sam/start-rom.S',
            'arm/sam/start-ram.S',
            'arm/sam/setup_pll.ads',
            'arm/sam/%s/board_config.ads' % self.board,
            'arm/sam/%s/setup_pll.adb' % self.board,
            'arm/sam/%s/memory-map.ld' % self.board,
            'arm/sam/%s/svd/i-sam.ads' % self.board,
            'arm/sam/%s/svd/i-sam-efc.ads' % self.board,
            'arm/sam/%s/svd/i-sam-pmc.ads' % self.board,
            'arm/sam/%s/svd/i-sam-sysc.ads' % self.board]

        self.pairs.update({
            's-textio.adb': 's-textio-sam4s.adb'})

        if self.has_fpu:
            fp = 'hardfloat'
        else:
            fp = 'softfloat'
        self.config_files.update(
            {'runtime.xml': readfile('arm/sam/%s/runtime.xml' % fp)})

    def amend_ravenscar_sfp(self):
        super(Sam, self).amend_ravenscar_sfp()
        self.bsp += [
            'arm/sam/%s/svd/handler.S' % self.board,
            'arm/sam/%s/s-bbbopa.ads' % self.board,
            'arm/sam/%s/s-bbmcpa.ads' % self.board]

        self.pairs.update({
            'a-intnam.ads': 'arm/sam/%s/svd/a-intnam.ads' % self.board,
            's-bbpara.ads': 's-bbpara-sam4s.ads'})


class DFBBTarget(Target):
    "BB target with single and double FPU"
    @property
    def is_bareboard(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True


class Zynq(DFBBTarget):
    def __init__(self):
        super(Zynq, self).__init__(
            mem_routines=True,
            libc_files=True,
            arm_zcx=True)
        self.build_flags['target'] = 'arm-eabi'

    def amend_zfp(self):
        super(Zynq, self).amend_zfp()
        self.bsp += [
            'arm/zynq/ram.ld',
            'arm/zynq/start-ram.S']
        self.pairs.update(
            {'system.ads': 'system-xi-arm.ads',
             's-textio.adb': 's-textio-zynq.adb',
             's-macres.adb': 's-macres-none.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('arm/zynq/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(Zynq, self).amend_ravenscar_sfp()

        self.pairs.update({
            'system.ads': 'system-xi-cortexa-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-arm.adb',
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbbosu.adb': 's-bbbosu-cortexa9.adb',
            's-bbpara.ads': 's-bbpara-cortexa9.ads'})


class SparcBBTarget(DFBBTarget):
    def __init__(self):
        super(SparcBBTarget, self).__init__(
            mem_routines=True,
            libc_files=False,
            arm_zcx=False)

    def amend_zfp(self):
        super(SparcBBTarget, self).amend_zfp()
        self.common += [
            'sparc.h']
        self.pairs.update({
            'system.ads': 'system-xi-sparc.ads',
            's-macres.adb': 's-macres-leon.adb',
            'sparc.h': 'sparc-bb.h'})
        # Add s-bb and s-bbbopa (needed by zfp for uart address)
        self.common.append('s-bb.ads')
        self.bsp.append('s-bbbopa.ads')

        # Was not present on erc32:
        self.build_flags['c_flags'] += ['-DLEON']

    def amend_ravenscar_sfp(self):
        super(SparcBBTarget, self).amend_ravenscar_sfp()
        self.gnarl_common.remove('s-bb.ads')
        self.gnarl_common += [
            'context_switch.S',
            'trap_handler.S',
            'interrupt_masking.S',
            'floating_point.S',
            's-bcpith.adb',
            # Were not present in erc32:
            's-bbcaco.ads', 's-bbcaco.adb']
        self.pairs.update({
            'system.ads': 'system-xi-sparc-ravenscar.ads',
            's-bbcppr.adb': 's-bbcppr-sparc.adb',
            's-bcpith.adb': 's-bcpith-bb-sparc.adb',
            'context_switch.S': 'context_switch-bb-sparc.S',
            'trap_handler.S': 'trap_handler-bb-sparc.S',
            'interrupt_masking.S': 'interrupt_masking-bb-sparc.S',
            'floating_point.S': 'floating_point-bb-sparc.S',
            's-bbcaco.adb': 's-bbcaco-leon.adb',
            's-musplo.adb': 's-musplo-leon.adb'})

    def amend_ravenscar_full(self):
        super(SparcBBTarget, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-xi-sparc-full.ads',
            's-traceb.adb': 's-traceb-xi-sparc.adb'})
        self.common += ['newlib-bb.c']
        # Use leon-zcx.specs to link with -lc.
        self.config_files.update(
            {'link-zcx.spec':
             readfile(os.path.join(crossdir, 'leon-elf/leon-zcx.specs'))})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nostartfiles",',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",')


class Leon2(SparcBBTarget):
    def __init__(self):
        super(Leon2, self).__init__()
        self.build_flags['target'] = 'leon-elf'

    def amend_zfp(self):
        super(Leon2, self).amend_zfp()
        self.arch += [
            'leon-elf/leon.ld',
            'leon-elf/crt0.S']
        self.bsp += [
            'sparc/leon/hw_init.S']
        self.pairs.update({
            's-textio.adb': 's-textio-leon.adb',
            's-bbbopa.ads': 's-bbbopa-leon.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('sparc/leon/runtime.xml')})
        self.build_flags['c_flags'] += ['-DLEON2']

    def amend_ravenscar_sfp(self):
        super(Leon2, self).amend_ravenscar_sfp()
        self.gnarl_common += [
            's-bbsule.ads']
        self.pairs.update({
            's-bbbosu.adb': 's-bbbosu-leon.adb',
            's-multip.adb': 's-multip-raven-default.adb',
            's-bbpara.ads': 's-bbpara-leon.ads',
            'a-intnam.ads': 'a-intnam-xi-leon.ads'})


class Leon3(SparcBBTarget):
    def __init__(self):
        super(Leon3, self).__init__()
        self.build_flags['target'] = 'leon3-elf'

    def amend_zfp(self):
        super(Leon3, self).amend_zfp()
        self.arch += [
            'leon3-elf/leon.ld',
            'leon-elf/crt0.S']
        self.bsp += [
            'sparc/leon/hw_init.S']
        self.pairs.update({
            's-textio.adb': 's-textio-leon3.adb',
            's-bbbopa.ads': 's-bbbopa-leon3.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('sparc/leon3/runtime.xml')})
        self.build_flags['c_flags'] += ['-DLEON3']

    def amend_ravenscar_sfp(self):
        super(Leon3, self).amend_ravenscar_sfp()
        self.gnarl_common += [
            's-bbsle3.ads']
        self.pairs.update({
            's-bbbosu.adb': 's-bbbosu-leon3.adb',
            's-multip.adb': 's-multip-leon3.adb',
            's-bbpara.ads': 's-bbpara-leon.ads',
            's-bcprmu.adb': 's-bcprmu-leon3.adb',
            'a-intnam.ads': 'a-intnam-xi-leon3.ads'})


class PPC6XXBBTarget(DFBBTarget):
    def __init__(self):
        super(PPC6XXBBTarget, self).__init__(
            mem_routines=True,
            libc_files=True,
            arm_zcx=False)
        self.build_flags['target'] = 'powerpc-elf'

    def amend_zfp(self):
        super(PPC6XXBBTarget, self).amend_zfp()
        self.pairs.update({
            'system.ads': 'system-xi-ppc.ads',
            's-lidosq.adb': 's-lidosq-ada.adb',
            's-lisisq.adb': 's-lisisq-ada.adb'})

    def amend_ravenscar_sfp(self):
        super(PPC6XXBBTarget, self).amend_ravenscar_sfp()
        self.gnarl_common += [
            'powerpc/6xx/context_switch.S',
            'powerpc/6xx/handler.S',
            's-bbcpsp.ads', 's-bbcpsp.adb']
        self.pairs.update({
            'system.ads': 'system-xi-ppc-sfp.ads',
            's-bbcppr.adb': 's-bbcppr-ppc.adb',
            's-bbcppr.ads': 's-bbcppr-ppc.ads',
            's-bbbosu.ads': 's-bbbosu-ppc.ads',
            's-bbinte.adb': 's-bbinte-ppc.adb',
            's-bbtime.adb': 's-bbtime-ppc.adb',
            's-bbcpsp.ads': 's-bbcpsp-6xx.ads',
            's-bbcpsp.adb': 's-bbcpsp-6xx.adb',
            's-flocon.adb': 's-flocon-none.adb'})

    def amend_ravenscar_full(self):
        super(PPC6XXBBTarget, self).amend_ravenscar_full()
        self.pairs.update({
            'system.ads': 'system-xi-ppc-full.ads',
            's-traceb.adb': 's-traceb-xi-ppc.adb'})


class MPC8641(PPC6XXBBTarget):
    def amend_zfp(self):
        super(MPC8641, self).amend_zfp()
        self.arch += [
            'powerpc/8641d/qemu-rom.ld',
            'powerpc/8641d/ram.ld']
        self.bsp += [
            'powerpc/8641d/start-rom.S',
            'powerpc/8641d/setup.S']
        # Add s-bb and s-bbbopa (needed by zfp for uart address)
        self.common.append('s-bb.ads')
        self.bsp.append('s-bbbopa.ads')
        self.pairs.update({
            's-macres.adb': 's-macres-p2020.adb',
            's-bbbopa.ads': 's-bbbopa-8641d.ads',
            's-textio.adb': 's-textio-p2020.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/8641d/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(MPC8641, self).amend_ravenscar_sfp()
        self.gnarl_common.remove('s-bb.ads')
        self.pairs.update({
            's-multip.adb': 's-multip-8641d.adb',
            's-bbbosu.adb': 's-bbbosu-ppc-openpic.adb',
            's-bbpara.ads': 's-bbpara-8641d.ads',
            'a-intnam.ads': 'a-intnam-xi-ppc-openpic.ads',
            's-bcprmu.adb': 's-bcprmu-8641d.adb'})

    def amend_ravenscar_full(self):
        super(MPC8641, self).amend_ravenscar_full()
        self.config_files.update(
            {'link-zcx.spec':
             readfile('powerpc/prep/link-zcx.spec')})
        self.config_files['runtime.xml'] = \
            self.config_files['runtime.xml'].replace(
                '"-nolibc"',
                '"-nolibc",\n' +
                '         "-lgnat", "-lgcc", "-lgnat",\n' +
                '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')


class Visium(DFBBTarget):
    def __init__(self):
        super(Visium, self).__init__(
            mem_routines=False,
            libc_files=False,
            arm_zcx=False)
        self.build_flags['target'] = 'visium-elf'

    def amend_zfp(self):
        super(Visium, self).amend_zfp()
        self.pairs.update(
            {'system.ads': 'system-xi-visium.ads',
             's-textio.adb': 's-textio-stdio.adb',
             's-macres.adb': 's-macres-native.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('visium/mcm/runtime.xml'),
             'target_options.gpr': readfile('visium/mcm/target_options.gpr')})


class X86Native(DFBBTarget):
    def __init__(self):
        super(X86Native, self).__init__(
            mem_routines=False,
            libc_files=False,
            arm_zcx=False)

    def amend_zfp(self):
        super(X86Native, self).amend_zfp()
        self.pairs.update(
            {'system.ads': 'system-xi-x86.ads',
             's-textio.adb': 's-textio-stdio.adb',
             's-macres.adb': 's-macres-native.adb'})
        self.config_files.update(
            {'runtime.xml': readfile('native/runtime.xml')})


class X86Linux(X86Native):
    def __init__(self):
        super(X86Linux, self).__init__()
        self.build_flags['target'] = 'x86-linux'


class X86Windows(X86Native):
    def __init__(self):
        super(X86Linux, self).__init__()
        self.build_flags['target'] = 'x86-windows'


def build_configs(target, runtime):
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'ppc-pikeos':
        t = PpcPikeOS()
    elif target == 'x86-pikeos':
        t = X86PikeOS()
    elif target == 'zynq':
        t = Zynq()
    elif target.startswith('stm32'):
        t = Stm32(target)
    elif target.startswith('sam'):
        t = Sam(target)
    elif target == 'leon2' or target == 'leon':
        t = Leon2()
    elif target == 'leon3':
        t = Leon3()
    elif target == '8641d':
        t = MPC8641()
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
    print "usage: build-rts.py OPTIONS arch/runtime"
    print "Options are:"
    print " -v --verbose     be verbose"
    print " --output=DIR     output directory"
    print " --gccdir=DIR     gcc source directory"
    print " --gnatdir=DIR    gnat source directory"
    print " --crossdir=DIR   cross source directory"


def main():
    global link, gccdir, gnatdir, crossdir, verbose

    install = objdir

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "hvl",
            ["help", "verbose",
             "output=",
             "gcc-dir=", "gnat-dir=", "cross-dir=",
             "link"])
    except getopt.GetoptError, e:
        print "error: " + str(e)
        print "Try --help"
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-v", "--verbose"):
            verbose = True
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt in ("-l", "--link"):
            link = True
        elif opt == "--output":
            install = arg
        elif opt == "--gcc-dir":
            gccdir = arg
        elif opt == "--gnat-dir":
            gnatdir = arg
        elif opt == "--cross-dir":
            crossdir = arg
        else:
            sys.abort()

    if len(args) != 1:
        print "error: missing configuration"
        print "Try --help"
        sys.exit(2)

    (rts, arch) = args[0].split('/')

    target = build_configs(arch, rts)
    target.install(install)


if __name__ == '__main__':
    main()
