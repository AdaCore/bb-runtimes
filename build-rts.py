#! /usr/bin/env python
#
# Copyright (C) 2016, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.4 (yes, it's very old but that's the system
# python on oldest host).

import getopt
import sys
import os
import shutil
import string

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


class Files:
    """List of files for a runtime.
    Each list attribute represent a subdirectory.
    The pairs dictionnary handle target specific files.
    The config_files dictionnary represent configuration (.gpr and .xml)
    files"""
    def __init__(self):
        self.arch = []
        self.common = []
        self.gnarl_common = []
        self.gnarl_arch = []
        self.math = []
        self.svd = []
        self.pairs = {}
        self.config_files = {}

# Definitions of sources files.
# Keep spec and body on one line.


def files_zfp_nofp(f):
    "Files for a zfp runtime (without floating point support)"

    f.arch += ["system.ads"]
    f.common += \
        ["ada.ads",
         "a-except.adb", "a-except.ads",
         "a-tags.adb", "a-tags.ads",
         "a-assert.ads", "a-assert.adb",
         "a-textio.adb", "a-textio.ads",
         "a-unccon.ads",
         "a-uncdea.ads",

         "unchconv.ads",
         "unchdeal.ads",
         "machcode.ads",

         "gnat.ads",
         "g-io.adb", "g-io.ads",
         "g-io-put.adb",
         "g-souinf.ads",

         "interfac.ads",
         "i-c.ads", "i-cexten.ads",
         "i-bit_types.ads",

         "s-atacco.adb", "s-atacco.ads",
         "s-assert.adb", "s-assert.ads",
         "s-maccod.ads",
         "s-secsta.adb", "s-secsta.ads",
         "s-textio.adb", "s-textio.ads",
         "s-stoele.adb", "s-stoele.ads",
         "s-unstyp.ads"]

    f.pairs.update(
        {"system.ads": None,  # Must be overriden
         "a-tags.adb": "a-tags-hie.adb", "a-tags.ads": "a-tags-hie.ads",
         "a-except.adb": "a-except-zfp.adb",
         "a-except.ads": "a-except-zfp.ads",
         "a-textio.adb": "a-textio-zfp.adb",
         "a-textio.ads": "a-textio-zfp.ads",
         "g-io.adb": "g-io-zfp.adb",
         "g-io.ads": "g-io-zfp.ads",
         "g-io-put.adb": "g-io-put-stextio.adb",
         "i-c.ads": "i-c-hie.ads",
         "s-assert.adb": "s-assert-xi.adb",
         "s-secsta.adb": "s-secsta-zfp.adb",
         "s-secsta.ads": "s-secsta-zfp.ads",
         "s-textio.adb": None,
         "s-textio.ads": "s-textio-zfp.ads"})
    f.config_files.update(
      {"runtime_build.gpr": readfile("src/runtime_build.gpr"),
       "target_options.gpr": readfile("src/target_options.gpr")})


# ZFP with floating points.
def files_zfp(f):
    files_zfp_nofp(f)
    f.common += \
        ["s-fatflt.ads",
         "s-fatlfl.ads",
         "s-fatllf.ads",
         "s-fatsfl.ads",
         "s-fatgen.adb", "s-fatgen.ads"]


def files_mem(f):
    """Add support for memory routines called directly by backend.
    Required on targets without libc"""

    f.common += \
        ["s-memcop.ads", "s-memcop.adb",
         "s-memmov.ads", "s-memmov.adb",
         "s-memset.ads", "s-memset.adb",
         "s-memcom.ads", "s-memcom.adb"]
    f.pairs.update(
        {"s-memcop.ads": "s-memcop-zfp.ads",
         "s-memcop.adb": "s-memcop-zfp.adb"})


def files_ravenscar_sfp(f):
    "Ravenscar sfp"

    files_zfp(f)
    f.common += \
        ["s-parame.adb", "s-parame.ads",
         "s-sssita.adb", "s-sssita.ads"]
    f.gnarl_arch += \
        ["a-intnam.ads"]
    f.gnarl_common += \
        ["a-interr.adb", "a-interr.ads",
         "a-reatim.adb", "a-reatim.ads",
         "a-retide.adb", "a-retide.ads",
         "a-sytaco.adb", "a-sytaco.ads",
         "a-taside.adb", "a-taside.ads",
         "s-interr.adb", "s-interr.ads",
         "s-multip.ads", "s-multip.adb",
         "s-mufalo.ads", "s-mufalo.adb",
         "s-musplo.ads", "s-musplo.adb",
         "s-osinte.ads", "s-osinte.adb",
         "s-taprob.adb", "s-taprob.ads",
         "s-taprop.adb", "s-taprop.ads",
         "s-tarest.adb", "s-tarest.ads",
         "s-tasdeb.adb", "s-tasdeb.ads",
         "s-tasinf.adb", "s-tasinf.ads",
         "s-taskin.adb", "s-taskin.ads",
         "s-taspri.ads",
         "s-tasres.ads",
         "a-taster.adb", "a-taster.ads",
         "s-tposen.adb", "s-tposen.ads",
         "s-tpobmu.adb", "s-tpobmu.ads"]
    f.pairs.update(
        {"a-intnam.ads": None,
         "a-interr.adb": "a-interr-raven.adb",
         "a-reatim.ads": "a-reatim-xi.ads", "a-reatim.adb": "a-reatim-xi.adb",
         "a-retide.adb": "a-retide-raven.adb",
         "a-sytaco.ads": "a-sytaco-xi.ads", "a-sytaco.adb": "a-sytaco-xi.adb",
         "a-taside.adb": "a-taside-raven.adb",
         "s-taspri.ads": "s-taspri-xi.ads",
         "a-taster.adb": "a-taster-raven.adb",
         "a-taster.ads": "a-taster-raven.ads",
         "s-interr.ads": "s-interr-raven.ads",
         "s-parame.adb": "s-parame-xi.adb",
         "s-parame.ads": "s-parame-xi.ads",
         "s-multip.ads": "s-multip-raven-default.ads",
         "s-multip.adb": "s-multip-raven-default.adb",
         "s-sssita.adb": "s-sssita-xi.adb",
         "s-sssita.ads": "s-sssita-xi.ads",
         "s-taprob.adb": "s-taprob-raven.adb",
         "s-taprob.ads": "s-taprob-raven.ads",
         "s-taprop.ads": "s-taprop-xi.ads",
         "s-tarest.adb": "s-tarest-raven.adb",
         "s-tasdeb.ads": "s-tasdeb-xi.ads",
         "s-tasdeb.adb": "s-tasdeb-raven.adb",
         "s-taskin.adb": "s-taskin-raven.adb",
         "s-taskin.ads": "s-taskin-raven.ads",
         "s-tposen.adb": "s-tposen-raven.adb",
         "s-tposen.ads": "s-tposen-raven.ads"})
    f.config_files.update(
      {"ravenscar_build.gpr": readfile("src/ravenscar_build.gpr")})


def files_math(f):
    f.math += \
        ["a-ncelfu.ads",
         "a-ngcefu.adb", "a-ngcefu.ads",
         "a-ngcoar.adb", "a-ngcoar.ads",
         "a-ngcoty.adb", "a-ngcoty.ads",
         "a-ngelfu.adb", "a-ngelfu.ads",
         "a-ngrear.ads", "a-ngrear.adb",
         "a-nlcefu.ads",
         "a-nlcoty.ads",
         "a-nlelfu.ads",
         "a-nllcef.ads",
         "a-nllcty.ads",
         "a-nllefu.ads",
         "a-nscefu.ads",
         "a-nscoty.ads",
         "a-nselfu.ads",
         "a-nucoty.ads",
         "a-nuelfu.ads",
         "a-numaux.ads",
         "a-numeri.ads",
         "s-exnllf.adb", "s-exnllf.ads",
         "s-gearop.adb", "s-gearop.ads"]


def files_containers(f):
    f.common += \
        ["a-btgbso.adb", "a-btgbso.ads",
         "a-cbdlli.adb", "a-cbdlli.ads",
         "a-cbhama.adb", "a-cbhama.ads",
         "a-cbhase.adb", "a-cbhase.ads",
         "a-cbmutr.adb", "a-cbmutr.ads",
         "a-cborma.adb", "a-cborma.ads",
         "a-cborse.adb", "a-cborse.ads",
         "a-cdlili.adb", "a-cdlili.ads",
         "a-cfdlli.adb", "a-cfdlli.ads",
         "a-cfhama.adb", "a-cfhama.ads",
         "a-cfhase.adb", "a-cfhase.ads",
         "a-cfinve.adb", "a-cfinve.ads",
         "a-cforma.adb", "a-cforma.ads",
         "a-cforse.adb", "a-cforse.ads",
         "a-cgaaso.adb", "a-cgaaso.ads",
         "a-cgarso.adb", "a-cgarso.ads",
         "a-cgcaso.adb", "a-cgcaso.ads",
         "a-chtgbk.adb", "a-chtgbk.ads",
         "a-chtgbo.adb", "a-chtgbo.ads",
         "a-chtgke.adb", "a-chtgke.ads",
         "a-chtgop.adb", "a-chtgop.ads",
         "a-cidlli.adb", "a-cidlli.ads",
         "a-cihama.adb", "a-cihama.ads",
         "a-cihase.adb", "a-cihase.ads",
         "a-cimutr.adb", "a-cimutr.ads",
         "a-ciorma.adb", "a-ciorma.ads",
         "a-ciormu.adb", "a-ciormu.ads",
         "a-ciorse.adb", "a-ciorse.ads",
         "a-coboho.adb", "a-coboho.ads",
         "a-cobove.adb", "a-cobove.ads",
         "a-cofove.adb", "a-cofove.ads",
         "a-cogeso.adb", "a-cogeso.ads",
         "a-cohama.adb", "a-cohama.ads",
         "a-cohase.adb", "a-cohase.ads",
         "a-cohata.ads",
         "a-coinho.adb", "a-coinho.ads",
         "a-coinve.adb", "a-coinve.ads",
         "a-comutr.adb", "a-comutr.ads",
         "a-conhel.adb", "a-conhel.ads",
         "a-contai.ads",
         "a-convec.adb", "a-convec.ads",
         "a-coorma.adb", "a-coorma.ads",
         "a-coormu.adb", "a-coormu.ads",
         "a-coorse.adb", "a-coorse.ads",
         "a-coprnu.adb", "a-coprnu.ads",
         "a-crbltr.ads",
         "a-crbtgk.adb", "a-crbtgk.ads",
         "a-crbtgo.adb", "a-crbtgo.ads",
         "a-crdlli.adb", "a-crdlli.ads",
         "a-csquin.ads",
         "a-rbtgbk.adb", "a-rbtgbk.ads",
         "a-rbtgbo.adb", "a-rbtgbo.ads",
         "a-rbtgso.adb", "a-rbtgso.ads",
         "a-iteint.ads",
         "s-atocou.adb", "s-atocou.ads"]


def files_ravenscar_full(f):
    files_ravenscar_sfp(f)
    files_math(f)
    files_containers(f)
    f.common += \
        ["a-chahan.adb", "a-chahan.ads",
         "a-charac.ads",
         "a-chlat1.ads",
         "a-chlat9.ads",
         "a-cwila1.ads",
         "a-cwila9.ads",
         "a-decima.adb", "a-decima.ads",
         "a-einuoc.adb", "a-einuoc.ads",
         "a-elchha.adb", "a-elchha.ads",
         "a-exctra.adb", "a-exctra.ads",
         "a-finali.adb", "a-finali.ads",
         "a-ioexce.ads",
         "a-nudira.adb", "a-nudira.ads",
         "a-nuflra.adb", "a-nuflra.ads",
         "a-stmaco.ads",
         "a-storio.adb", "a-storio.ads",
         "a-strbou.adb", "a-strbou.ads",
         "a-stream.adb", "a-stream.ads",
         "a-strfix.adb", "a-strfix.ads",
         "a-string.ads",
         "a-strmap.adb", "a-strmap.ads",
         "a-strsea.adb", "a-strsea.ads",
         "a-strsup.adb", "a-strsup.ads",
         "a-strunb.adb", "a-strunb.ads",
         "a-stunau.adb", "a-stunau.ads",
         "a-stwibo.adb", "a-stwibo.ads",
         "a-stwifi.adb", "a-stwifi.ads",
         "a-stwima.adb", "a-stwima.ads",
         "a-stwise.adb", "a-stwise.ads",
         "a-stwisu.adb", "a-stwisu.ads",
         "a-stwiun.adb", "a-stwiun.ads",
         "a-swmwco.ads",
         "a-undesu.adb", "a-undesu.ads",

         "g-arrspl.adb", "g-arrspl.ads",
         "g-bubsor.adb", "g-bubsor.ads",
         "g-busora.adb", "g-busora.ads",
         "g-busorg.adb", "g-busorg.ads",
         "g-bytswa.adb", "g-bytswa.ads",
         "g-casuti.adb", "g-casuti.ads",
         "g-comver.adb", "g-comver.ads",
         "g-crc32.adb",  "g-crc32.ads",
         "g-debuti.adb", "g-debuti.ads",
         "g-except.ads",
         "g-heasor.adb", "g-heasor.ads",
         "g-hesora.adb", "g-hesora.ads",
         "g-hesorg.adb", "g-hesorg.ads",
         "g-htable.adb", "g-htable.ads",
         "g-md5.adb",    "g-md5.ads",
         "g-moreex.adb", "g-moreex.ads",
         "g-regexp.ads",
         "g-sechas.adb", "g-sechas.ads",
         "g-sehamd.adb", "g-sehamd.ads",
         "g-sehash.adb", "g-sehash.ads",
         "g-sha1.adb",   "g-sha1.ads",
         "g-sha224.ads",
         "g-sha256.ads",
         "g-sha384.ads",
         "g-sha512.ads",
         "g-shsh32.adb", "g-shsh32.ads",
         "g-shsh64.adb", "g-shsh64.ads",
         "g-shshco.adb", "g-shshco.ads",
         "g-string.ads",
         "g-strspl.ads",
         "g-table.adb",  "g-table.ads",
         "g-tasloc.ads",
         "g-wistsp.ads",

         "i-cobol.adb",  "i-cobol.ads",
         "i-fortra.adb", "i-fortra.ads",
         "i-pacdec.adb", "i-pacdec.ads",

         "ioexcept.ads",

         "s-addima.adb", "s-addima.ads",
         "s-addope.adb", "s-addope.ads",
         "s-arit64.adb", "s-arit64.ads",
         "s-bitops.adb", "s-bitops.ads",
         "s-boarop.ads",
         "s-bytswa.ads",
         "s-carsi8.adb", "s-carsi8.ads",
         "s-carun8.adb", "s-carun8.ads",
         "s-casi16.adb", "s-casi16.ads",
         "s-casi32.adb", "s-casi32.ads",
         "s-casi64.adb", "s-casi64.ads",
         "s-casuti.adb", "s-casuti.ads",
         "s-caun16.adb", "s-caun16.ads",
         "s-caun32.adb", "s-caun32.ads",
         "s-caun64.adb", "s-caun64.ads",
         "s-chepoo.ads",
         "s-crc32.adb",  "s-crc32.ads",
         "s-except.adb", "s-except.ads",
         "s-excdeb.adb", "s-excdeb.ads",
         "s-exctab.adb", "s-exctab.ads",
         "s-exnint.adb", "s-exnint.ads",
         "s-exnlli.adb", "s-exnlli.ads",
         "s-expint.adb", "s-expint.ads",
         "s-explli.adb", "s-explli.ads",
         "s-expllu.adb", "s-expllu.ads",
         "s-expmod.adb", "s-expmod.ads",
         "s-expuns.adb", "s-expuns.ads",
         "s-finmas.adb", "s-finmas.ads",
         "s-finroo.ads", "s-finroo.adb",
         "s-flocon.adb", "s-flocon.ads",
         "s-fore.adb",   "s-fore.ads",
         "s-geveop.adb", "s-geveop.ads",
         "s-htable.adb", "s-htable.ads",
         "s-imenne.adb", "s-imenne.ads",
         "s-imgbiu.adb", "s-imgbiu.ads",
         "s-imgboo.adb", "s-imgboo.ads",
         "s-imgcha.adb", "s-imgcha.ads",
         "s-imgdec.adb", "s-imgdec.ads",
         "s-imgenu.adb", "s-imgenu.ads",
         "s-imgint.adb", "s-imgint.ads",
         "s-imgllb.adb", "s-imgllb.ads",
         "s-imglld.adb", "s-imglld.ads",
         "s-imglli.adb", "s-imglli.ads",
         "s-imgllu.adb", "s-imgllu.ads",
         "s-imgllw.adb", "s-imgllw.ads",
         "s-imgrea.adb", "s-imgrea.ads",
         "s-imguns.adb", "s-imguns.ads",
         "s-imgwch.adb", "s-imgwch.ads",
         "s-imgwiu.adb", "s-imgwiu.ads",
         "s-init.adb",   "s-init.ads",
         "s-io.adb",     "s-io.ads",
         "s-mantis.adb", "s-mantis.ads",
         "s-mastop.adb", "s-mastop.ads",
         "s-memory.adb", "s-memory.ads",
         "s-pack03.adb", "s-pack03.ads",
         "s-pack05.adb", "s-pack05.ads",
         "s-pack06.adb", "s-pack06.ads",
         "s-pack07.adb", "s-pack07.ads",
         "s-pack09.adb", "s-pack09.ads",
         "s-pack10.adb", "s-pack10.ads",
         "s-pack11.adb", "s-pack11.ads",
         "s-pack12.adb", "s-pack12.ads",
         "s-pack13.adb", "s-pack13.ads",
         "s-pack14.adb", "s-pack14.ads",
         "s-pack15.adb", "s-pack15.ads",
         "s-pack17.adb", "s-pack17.ads",
         "s-pack18.adb", "s-pack18.ads",
         "s-pack19.adb", "s-pack19.ads",
         "s-pack20.adb", "s-pack20.ads",
         "s-pack21.adb", "s-pack21.ads",
         "s-pack22.adb", "s-pack22.ads",
         "s-pack23.adb", "s-pack23.ads",
         "s-pack24.adb", "s-pack24.ads",
         "s-pack25.adb", "s-pack25.ads",
         "s-pack26.adb", "s-pack26.ads",
         "s-pack27.adb", "s-pack27.ads",
         "s-pack28.adb", "s-pack28.ads",
         "s-pack29.adb", "s-pack29.ads",
         "s-pack30.adb", "s-pack30.ads",
         "s-pack31.adb", "s-pack31.ads",
         "s-pack33.adb", "s-pack33.ads",
         "s-pack34.adb", "s-pack34.ads",
         "s-pack35.adb", "s-pack35.ads",
         "s-pack36.adb", "s-pack36.ads",
         "s-pack37.adb", "s-pack37.ads",
         "s-pack38.adb", "s-pack38.ads",
         "s-pack39.adb", "s-pack39.ads",
         "s-pack40.adb", "s-pack40.ads",
         "s-pack41.adb", "s-pack41.ads",
         "s-pack42.adb", "s-pack42.ads",
         "s-pack43.adb", "s-pack43.ads",
         "s-pack44.adb", "s-pack44.ads",
         "s-pack45.adb", "s-pack45.ads",
         "s-pack46.adb", "s-pack46.ads",
         "s-pack47.adb", "s-pack47.ads",
         "s-pack48.adb", "s-pack48.ads",
         "s-pack49.adb", "s-pack49.ads",
         "s-pack50.adb", "s-pack50.ads",
         "s-pack51.adb", "s-pack51.ads",
         "s-pack52.adb", "s-pack52.ads",
         "s-pack53.adb", "s-pack53.ads",
         "s-pack54.adb", "s-pack54.ads",
         "s-pack55.adb", "s-pack55.ads",
         "s-pack56.adb", "s-pack56.ads",
         "s-pack57.adb", "s-pack57.ads",
         "s-pack58.adb", "s-pack58.ads",
         "s-pack59.adb", "s-pack59.ads",
         "s-pack60.adb", "s-pack60.ads",
         "s-pack61.adb", "s-pack61.ads",
         "s-pack62.adb", "s-pack62.ads",
         "s-pack63.adb", "s-pack63.ads",
         "s-pooglo.adb", "s-pooglo.ads",
         "s-pooloc.adb", "s-pooloc.ads",
         "s-poosiz.adb", "s-poosiz.ads",
         "s-powtab.ads",
         "s-rannum.adb", "s-rannum.ads",
         "s-ransee.adb", "s-ransee.ads",
         "s-regexp.adb", "s-regexp.ads",
         "s-restri.adb", "s-restri.ads",
         "s-rident.ads",
         "s-scaval.adb", "s-scaval.ads",
         "s-soflin.adb", "s-soflin.ads",
         "s-sopco3.adb", "s-sopco3.ads",
         "s-sopco4.adb", "s-sopco4.ads",
         "s-sopco5.adb", "s-sopco5.ads",
         "s-spsufi.adb", "s-spsufi.ads",
         "s-stalib.adb", "s-stalib.ads",
         "s-stopoo.adb", "s-stopoo.ads",
         "s-stposu.adb", "s-stposu.ads",
         "s-stratt.adb", "s-stratt.ads",
         "s-strhas.adb", "s-strhas.ads",
         "s-string.adb", "s-string.ads",
         "s-tasloc.adb", "s-tasloc.ads",
         "s-traceb.adb", "s-traceb.ads",
         "s-traent.adb", "s-traent.ads",
         "s-trasym.adb", "s-trasym.ads",
         "s-valboo.adb", "s-valboo.ads",
         "s-valcha.adb", "s-valcha.ads",
         "s-valdec.adb", "s-valdec.ads",
         "s-valenu.adb", "s-valenu.ads",
         "s-valint.adb", "s-valint.ads",
         "s-vallld.adb", "s-vallld.ads",
         "s-vallli.adb", "s-vallli.ads",
         "s-valllu.adb", "s-valllu.ads",
         "s-valrea.adb", "s-valrea.ads",
         "s-valuns.adb", "s-valuns.ads",
         "s-valuti.adb", "s-valuti.ads",
         "s-valwch.adb", "s-valwch.ads",
         "s-veboop.adb", "s-veboop.ads",
         "s-vector.ads",
         "s-vercon.adb", "s-vercon.ads",
         "s-wchcnv.adb", "s-wchcnv.ads",
         "s-wchcon.adb", "s-wchcon.ads",
         "s-wchjis.adb", "s-wchjis.ads",
         "s-wchstw.adb", "s-wchstw.ads",
         "s-wchwts.adb", "s-wchwts.ads",
         "s-widboo.adb", "s-widboo.ads",
         "s-widcha.adb", "s-widcha.ads",
         "s-widenu.adb", "s-widenu.ads",
         "s-widlli.adb", "s-widlli.ads",
         "s-widllu.adb", "s-widllu.ads",
         "s-widwch.adb", "s-widwch.ads",
         "s-wwdcha.adb", "s-wwdcha.ads",
         "s-wwdenu.adb", "s-wwdenu.ads",
         "s-wwdwch.adb", "s-wwdwch.ads",
         "text_io.ads",
         "a-excach.adb",
         "a-excpol.adb",
         "a-exexda.adb",
         "a-exextr.adb",
         "a-exexpr.adb",
         "a-exstat.adb",
         "s-excmac.ads",
         "raise-gcc.c",
         "raise.h",
         "src/tconfig.h",
         "src/tsystem.h",
         "libgcc/unwind-pe.h"]

    f.pairs.update(
        {"a-elchha.adb": "a-elchha-xi.adb",
         "a-excach.adb": "a-excach-cert.adb",
         "a-except.adb": "a-except-2005.adb",
         "a-except.ads": "a-except-2005.ads",
         "s-io.adb": "s-io-xi.adb",
         "s-ransee.adb": "s-ransee-xi.adb",
         "s-soflin.adb": "s-soflin-xi.adb", "s-soflin.ads": "s-soflin-xi.ads",
         "s-sssita.adb": "s-sssita-xi.adb", "s-sssita.ads": "s-sssita-xi.ads",
         "s-taskin.ads": "s-taskin-xi-full.ads",
         "s-tposen.adb": "s-tposen-xi-full.adb",
         "s-tposen.ads": "s-tposen-xi-full.ads",
         "s-atocou.adb": "s-atocou-builtin.adb",
         "a-coinho.ads": "a-coinho-shared.ads",
         "a-coinho.adb": "a-coinho-shared.adb"})


cwd = os.getcwd()


def copy(src, dst):
    "Copy (or symlink) src to dst"

    if not os.path.exists(src):
        print "runtime file " + src + " does not exists"
        sys.exit(4)
    if verbose:
        print "copy " + src + " to " + dst
    if link:
        try:
            os.symlink(os.path.join(cwd, src), dst)
        except os.error, e:
            print "symlink error for " + src
            print "msg: " + str(e)
            sys.exit(2)
    else:
        shutil.copy(src, dst)


def copy_pair(srcfile, destdir, pairs):
    "Copy after substitution with pairs"

    # Find the pair filename
    if srcfile in pairs:
        n = pairs[srcfile]
        if not n:
            print "Error: undefined pair for " + srcfile
            sys.exit(2)
    else:
        n = srcfile

    dst = objdir + '/' + destdir + '/' + srcfile

    # Find the sourcedir
    if '/' not in n:
        # Files without path elements are in gnat
        copy(gnatdir + '/' + n, objdir + '/' + destdir + '/' + srcfile)
    else:
        b = os.path.basename(n)
        for d in ['.', gccdir, crossdir]:
            src = d + '/' + n
            if os.path.exists(src):
                copy(src, objdir + '/' + destdir + '/' + b)
                return
        print "Cannot find source dir for " + n
        sys.exit(2)


def readfile(filename):
    f = open(filename)
    res = f.read()
    f.close()
    return res


def files_ravenscar_pikeos(f):
    f.gnarl_common += ["adaint-pikeos.c"]
    f.pairs.update(
      {"a-intnam.ads": "a-intnam-dummy.ads",
       "s-taprop.adb": "s-taprop-pikeos.adb",
       "s-textio.adb": "s-textio-pikeos.adb",
       "s-init.adb": "s-init-pikeos-ravenscar.adb",
       "s-memory.ads": "s-memory-pikeos.ads",
       "s-memory.adb": "s-memory-pikeos.adb",
       "a-textio.adb": "a-textio-raven.adb",
       "s-traceb.adb": "s-traceb-xi-armeabi.adb",
       "s-traceb.ads": "s-traceb-cert.ads",
       "s-interr.adb": "s-interr-pikeos4.adb",
       "s-osinte.ads": "s-osinte-pikeos4.ads",
       "s-osinte.adb": "s-osinte-pikeos4.adb"})
    f.pairs.update({"s-flocon.adb": "s-flocon-none.adb"})
    f.arch += ["pikeos-cert-app.c"]


def build_ravenscar_full_arm_pikeos(f):
    files_ravenscar_full(f)
    files_ravenscar_pikeos(f)
    f.pairs.update(
        {"system.ads": "system-pikeos-arm-ravenscar-full.ads"})
    f.pairs.update({"a-exexpr.adb": "a-exexpr-gcc.adb",
                    "s-excmac.ads": "s-excmac-arm.ads"})
    f.config_files.update(
      {"runtime.xml": readfile("arm/pikeos/runtime.xml")})

build_configs = \
  {"ravenscar-full/arm-pikeos": build_ravenscar_full_arm_pikeos}


def usage():
    print "usage: build-rts.py OPTIONS"
    print "Options are:"
    print " -v --verbose     be verbose"
    print " --output=DIR     output directory"
    print " --gccdir=DIR     gcc source directory"
    print " --gnatdir=DIR    gnat source directory"
    print " --crossdir=DIR   cross source directory"


def main():
    global link, objdir, gccdir, gnatdir, crossdir, verbose
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hvl",
                                   ["help", "verbose",
                                    "output=",
                                    "gccdir=", "gnatdir=", "crossdir=",
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
            objdir = arg
        elif opt == "--gccdir":
            gccdir = arg
        elif opt == "--gnatdir":
            gnatdir = arg
        elif opt == "--crossdir":
            crossdir = arg
        else:
            sys.abort()

    if len(args) != 1:
        print "error: missing configuration"
        sys.exit(2)
    config = args[0]
    (rts, arch) = config.split('/')

    if verbose:
        print "runtime: " + rts
        print "architecture: " + arch

    if config not in build_configs:
        print "error: unknown config " + config
        sys.exit(1)
    fn = build_configs[config]

    # List of files.
    files = Files()
    fn(files)

    # Build target directories
    os.mkdir(objdir)
    all_src_dirs = ["arch", "common", "svd", "math",
                    "gnarl_common", "gnarl_arch"]
    src_dirs = []
    for d in all_src_dirs:
        l = getattr(files, d)
        if l:
            dirname = string.replace(d, '_', '-')
            src_dirs += [dirname]
            os.mkdir(objdir + "/" + dirname)
            for f in l:
                copy_pair(f, dirname, files.pairs)

    for d in ["obj", "adalib"]:
        os.mkdir(objdir + "/" + d)

    # Generate ada_source_path
    f = open(objdir + "/ada_source_path", 'w')
    for d in src_dirs:
        f.write(d + "\n")
    f.close()

    # Write config files
    for name, content in files.config_files.iteritems():
        f = open(objdir + "/" + name, 'w')
        f.write(content)
        f.close()


main()
