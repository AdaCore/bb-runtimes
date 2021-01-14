#
# Copyright (C) 2016-2018, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).


#
# Copyright (C) 2016-2018, AdaCore
#
# This file holds the source list and scenario variables of the runtimes

######################
# Scenario Variables #
######################

# Scenario variables used to configure the runtime sources, together with their
# acceptable values.

# default value is always the first value of the list. So for example for
# optional features enabled via a "no" or "yes" value, always set 'no' as the
# first option to disable the feature by default (zfp and ravenscar-sfp cases).

all_scenarios = {
    # Main profile
    'RTS_Profile': ['zfp', 'ravenscar-sfp', 'ravenscar-full'],
    # CPU architecture
    'CPU_Family': ['arm', 'aarch64', 'leon', 'powerpc', 'x86', 'x86_64',
                   'riscv32', 'riscv64'],
    # Word size of target
    'Target_Word_Size': ['32', '64'],
    # FPU presence
    'Has_FPU': ['no', 'yes'],
    # FMA presence
    'Has_FMA': ['no', 'yes'],
    # Whether we rely on libc being available
    'Has_libc': ['no', 'yes'],
    # Whether an implementation of compare and swap is available
    'Has_Compare_And_Swap': ['yes', 'no'],
    # RAM profile
    'Memory_Profile': ['small', 'large', 'huge'],
    # 32-bit or 64-bit timers available on the hardware
    'Timer': ['n/a', 'timer32', 'timer64'],
    # Choose between Serial I/O or semihosting (Cortex-M specific)
    'Text_IO': ['serial', 'semihosting'],
    # How does the runtime integrate C support
    'Add_C_Integration': ['no', 'ada_clib', 'newlib'],
    # Whether we use certifiable runtime components
    'Certifiable_Packages': ['no', 'yes'],
    # PikeOS-specific
    'Pikeos_Version': ['pikeos3', 'pikeos4', 'pikeos4.2', 'pikeos5'],
    # Whether to add the math library in the runtime
    'Add_Math_Lib': [
        'no', 'softfloat', 'hardfloat',
        'hardfloat_dp', 'hardfloat_sp'],
    # Add complex type to the math library. N/A if Add_Math_Lib is 'no'
    'Add_Complex_Type_Support': ['no', 'yes'],
    # Add support for 64-bit arithmetic to detect 64-bit overflow
    'Add_Arith64': ['no', 'yes'],
    # Add support for 128-bit arithmetic to detect 128-bit overflow
    'Add_Arith128': ['no', 'yes'],
    # 'Image:
    'Add_Image_Enum': ['no', 'yes'],
    'Add_Image_Int': ['no', 'yes'],
    'Add_Image_LL_Int': ['no', 'yes'],
    'Add_Image_LLL_Int': ['no', 'yes'],
    'Add_Image_Based_Int': ['no', 'yes'],
    'Add_Image_LL_Based_Int': ['no', 'yes'],
    'Add_Image_LLL_Based_Int': ['no', 'yes'],
    'Add_Image_Decimal': ['no', 'yes'],
    'Add_Image_LL_Decimal': ['no', 'yes'],
    'Add_Image_LLL_Decimal': ['no', 'yes'],
    'Add_Image_Fixed': ['no', 'yes'],
    'Add_Image_LL_Fixed': ['no', 'yes'],
    'Add_Image_LLL_Fixed': ['no', 'yes'],
    'Add_Image_Float': ['no', 'yes'],
    'Add_Image_Char': ['no', 'yes'],
    'Add_Image_Wide_Char': ['no', 'yes'],
    # 'Value:
    'Add_Value_Bool': ['no', 'yes'],
    'Add_Value_Enum': ['no', 'yes'],
    'Add_Value_Int': ['no', 'yes'],
    'Add_Value_LL_Int': ['no', 'yes'],
    'Add_Value_LLL_Int': ['no', 'yes'],
    'Add_Value_Decimal': ['no', 'yes'],
    'Add_Value_LL_Decimal': ['no', 'yes'],
    'Add_Value_LLL_Decimal': ['no', 'yes'],
    'Add_Value_Fixed': ['no', 'yes'],
    'Add_Value_LL_Fixed': ['no', 'yes'],
    'Add_Value_LLL_Fixed': ['no', 'yes'],
    'Add_Value_Float': ['no', 'yes'],
    'Add_Value_Char': ['no', 'yes'],
    'Add_Value_Wide_Char': ['no', 'yes'],
    # Exponentiation:
    'Add_Exponent_Float': ['no', 'yes'],
    'Add_Exponent_Int': ['no', 'yes'],
    'Add_Exponent_LL_Int': ['no', 'yes'],
    'Add_Exponent_LLL_Int': ['no', 'yes'],
    'Add_Exponent_Modular': ['no', 'yes'],
    # Streams:
    'Add_Streams': ['no', 'yes'],
    # Runtime support for packed arrays
    'Add_Pack': ['no', 'yes'],
    # Runtime support for packed arrays on 64-bit platforms
    'Add_Pack64': ['no', 'yes'],
    # Various support packages
    'Add_Case_Util': ['no', 'yes'],
    'Add_Float_Util': ['no', 'yes'],
    'Add_Image_Util': ['no', 'yes'],
    'Add_IO_Exceptions': ['no', 'yes'],
    'Add_Value_Utils': ['no', 'yes'],
}

# Sources

# List of source files for the runtime
#
# This list is a dictionary with keys being the RTS source folder name, and the
# value a dictionary with the following items:
# * 'conditions': see below
# * 'srcs': a list of source files to be placed in the folder
# * 'bb_srcs': sources only applicable to bare metal targets
# * 'pikeos_srcs': sources only applicable to pikeos targets
#
# Semantic of conditions:
# * the 'conditions' value is always a list.
# * each condition evaluates to a boolean.
# * if several conditions are defined for a folder, then a logical and is used
# * a condition takes the forms:
#     Scenario_Var_Name:accepted_values
#   with accepted_values being:
#     a) a simple value (e.g. RTS_Profile:zfp): evaluated to True if
#        RTS_Profile is set to "zfp"
#     b) a coma-separated list of values (e.g. RTS_Profile:zfp,ravenscar-sfp):
#        evaluated to True if RTS_Profile is "zfp" or "ravenscar-sfp"
#     c) a negated value, preceded with an exclamation point (e.g.
#        RTS_Profile:!zfp): evaluated to True if RTS_Profile is not "zfp".
# If no condition is defined, then the folder is always used.
sources = {
    # LIBGNAT

    'common': {
        'srcs': [
            'libgnat/a-assert.ads', 'libgnat/a-assert.adb',
            'libgnat/a-unccon.ads',
            'libgnat/a-uncdea.ads',
            'libgnat/ada.ads',
            'hie/g-io__zfp.ads', 'hie/g-io__zfp.adb',
            'hie/g-io-put__bb.adb',
            'libgnat/g-souinf.ads',
            'libgnat/gnat.ads',
            'libgnat/interfac__2020.ads',
            'libgnat/machcode.ads',
            'libgnat/s-assert.ads',
            'libgnat/s-atacco.ads', 'libgnat/s-atacco.adb',
            'libgnat/s-imgboo.ads', 'libgnat/s-imgboo.adb',
            'libgnat/s-imagei.ads', 'libgnat/s-imagei.adb',
            'libgnat/s-imageu.ads', 'libgnat/s-imageu.adb',
            'libgnat/s-imgint.ads',
            'libgnat/s-imglli.ads',
            'libgnat/s-imgllu.ads',
            'libgnat/s-imguns.ads',
            'libgnat/s-maccod.ads',
            'hie/s-macres.ads',
            'hie/s-secsta__zfp.ads', 'hie/s-secsta__zfp.adb',
            'libgnat/s-stoele.ads', 'libgnat/s-stoele.adb',
            'hie/s-textio.ads',
            'libgnat/s-unstyp.ads',
            'libgnat/unchconv.ads',
            'libgnat/unchdeal.ads'],
        'bb_srcs': [
            'hie/a-textio.ads',
            'hie/s-bb.ads',
            'libgnat/text_io.ads'],
        'pikeos_srcs': [
            'hie/s-parame__large.ads', 'hie/s-parame.adb']
    },

    'common/32': {
        'conditions': ['Target_Word_Size:32'],
        'srcs': ['libgnat/i-cexten.ads']
    },

    'common/64': {
        'conditions': ['Target_Word_Size:64'],
        'srcs': [
            'libgnat/i-cexten__128.ads',
            'libgnat/s-imgllli.ads', 'libgnat/s-imglllu.ads']
    },

    'zfp': {
        'conditions': ['RTS_Profile:zfp,ravenscar-sfp'],
        'srcs': [
            'hie/a-elchha__zfp.ads', 'hie/a-elchha__zfp.adb',
            'hie/s-sssita.ads', 'hie/s-sssita.adb',
            'hie/a-except__zfp.ads', 'hie/a-except__zfp.adb',
            'hie/a-tags__hie.ads', 'hie/a-tags__hie.adb',
            'hie/i-c__hie.ads',
            'hie/s-assert__xi.adb',
            'hie/s-memory__zfp.ads']
    },

    'full': {
        'conditions': ['RTS_Profile:ravenscar-full'],
        'srcs': [
            'libgnat/a-chacon.ads', 'libgnat/a-chacon.adb',
            'libgnat/a-chahan.ads', 'libgnat/a-chahan.adb',
            'libgnat/a-charac.ads',
            'libgnat/a-chlat1.ads',
            'libgnat/a-chlat9.ads',
            'libgnat/a-cwila1.ads',
            'libgnat/a-cwila9.ads',
            'libgnat/a-chzla1.ads',
            'libgnat/a-chzla9.ads',
            'libgnat/a-decima.adb',
            'libgnat/a-einuoc.ads', 'libgnat/a-einuoc.adb',
            'libgnat/a-elchha.ads', 'hie/a-elchha__full.adb',
            'hie/a-excach__cert.adb',
            'libgnat/a-except.ads', 'libgnat/a-except.adb',
            'libgnat/a-exctra.ads', 'libgnat/a-exctra.adb',
            'libgnat/a-exexda.adb',
            'libgnat/a-exexpr.adb',
            'libgnat/a-exextr.adb',
            'libgnat/a-exstat.adb',
            'libgnat/a-finali.ads', 'libgnat/a-finali.adb',
            'libgnat/a-nlcoar.ads',
            'libgnat/a-nllcar.ads',
            'libgnat/a-nllrar.ads',
            'libgnat/a-nlrear.ads',
            'libgnat/a-nucoar.ads',
            'libgnat/a-nudira.ads', 'libgnat/a-nudira.adb',
            'libgnat/a-nuflra.ads', 'libgnat/a-nuflra.adb',
            'libgnat/a-sbecin.ads', 'libgnat/a-sbecin.adb',
            'libgnat/a-sbhcin.ads', 'libgnat/a-sbhcin.adb',
            'libgnat/a-sblcin.ads', 'libgnat/a-sblcin.adb',
            'libgnat/a-secain.ads', 'libgnat/a-secain.adb',
            'libgnat/a-sfecin.ads',
            'libgnat/a-sfhcin.ads',
            'libgnat/a-sflcin.ads',
            'libgnat/a-shcain.ads', 'libgnat/a-shcain.adb',
            'libgnat/a-slcain.ads', 'libgnat/a-slcain.adb',
            'libgnat/a-stboha.ads', 'libgnat/a-stboha.adb',
            'libgnat/a-stfiha.ads',
            'libgnat/a-strhas.adb', 'libgnat/a-strhas.ads',
            'libgnat/a-stmaco.ads',
            'libgnat/a-stobbu.ads', 'libgnat/a-stobbu.adb',
            'libgnat/a-storio.ads', 'libgnat/a-storio.adb',
            'libgnat/a-stoubu.ads', 'libgnat/a-stoubu.adb',
            'libgnat/a-stouut.ads', 'libgnat/a-stouut.adb',
            'libgnat/a-strbou.ads', 'libgnat/a-strbou.adb',
            'libgnat/a-strfix.ads', 'libgnat/a-strfix.adb',
            'libgnat/a-string.ads',
            'libgnat/a-strmap.ads', 'libgnat/a-strmap.adb',
            'libgnat/a-strsea.ads', 'libgnat/a-strsea.adb',
            'libgnat/a-strsup.ads', 'libgnat/a-strsup.adb',
            'libgnat/a-strunb.ads', 'libgnat/a-strunb.adb',
            'libgnat/a-stteou.ads',
            'libgnat/a-stunau.ads', 'libgnat/a-stunau.adb',
            'libgnat/a-stunha.ads', 'libgnat/a-stunha.adb',
            'libgnat/a-stuten.ads', 'libgnat/a-stuten.adb',
            'libgnat/a-stwibo.ads', 'libgnat/a-stwibo.adb',
            'libgnat/a-stwifi.ads', 'libgnat/a-stwifi.adb',
            'libgnat/a-stwiha.ads', 'libgnat/a-stwiha.adb',
            'libgnat/a-stwima.ads', 'libgnat/a-stwima.adb',
            'libgnat/a-stwise.ads', 'libgnat/a-stwise.adb',
            'libgnat/a-stwisu.ads', 'libgnat/a-stwisu.adb',
            'libgnat/a-stwiun.ads', 'libgnat/a-stwiun.adb',
            'libgnat/a-stzbou.ads', 'libgnat/a-stzbou.adb',
            'libgnat/a-stzfix.ads', 'libgnat/a-stzfix.adb',
            'libgnat/a-stzhas.ads', 'libgnat/a-stzhas.adb',
            'libgnat/a-stzmap.ads', 'libgnat/a-stzmap.adb',
            'libgnat/a-stzsea.ads', 'libgnat/a-stzsea.adb',
            'libgnat/a-stzsup.ads', 'libgnat/a-stzsup.adb',
            'libgnat/a-stzunb.ads', 'libgnat/a-stzunb.adb',
            'libgnat/a-suecin.ads', 'libgnat/a-suecin.adb',
            'libgnat/a-suenco.ads', 'libgnat/a-suenco.adb',
            'libgnat/a-suenst.ads', 'libgnat/a-suenst.adb',
            'libgnat/a-suewst.ads', 'libgnat/a-suewst.adb',
            'libgnat/a-suezst.ads', 'libgnat/a-suezst.adb',
            'libgnat/a-suhcin.ads', 'libgnat/a-suhcin.adb',
            'libgnat/a-sulcin.ads', 'libgnat/a-sulcin.adb',
            'libgnat/a-swbwha.ads', 'libgnat/a-swbwha.adb',
            'libgnat/a-swfwha.ads',
            'libgnat/a-swunau.ads', 'libgnat/a-swunau.adb',
            'libgnat/a-swuwha.ads', 'libgnat/a-swuwha.adb',
            'libgnat/a-swmwco.ads',
            'libgnat/a-szbzha.ads', 'libgnat/a-szbzha.adb',
            'libgnat/a-szfzha.ads',
            'libgnat/a-szmzco.ads',
            'libgnat/a-szunau.ads', 'libgnat/a-szunau.adb',
            'libgnat/a-szuzha.ads', 'libgnat/a-szuzha.adb',
            'libgnat/a-tags.ads',   'libgnat/a-tags.adb',
            'libgnat/a-undesu.ads', 'libgnat/a-undesu.adb',
            'libgnat/a-widcha.ads',
            'libgnat/a-wichha.ads', 'libgnat/a-wichha.adb',
            'libgnat/a-wichun.ads', 'libgnat/a-wichun.adb',
            'libgnat/a-zchara.ads',
            'libgnat/a-zchhan.ads', 'libgnat/a-zchhan.adb',
            'libgnat/a-zchuni.ads', 'libgnat/a-zchuni.adb',
            'libgnat/g-arrspl.ads', 'libgnat/g-arrspl.adb',
            'libgnat/g-bubsor.ads', 'libgnat/g-bubsor.adb',
            'libgnat/g-busora.ads', 'libgnat/g-busora.adb',
            'libgnat/g-busorg.ads', 'libgnat/g-busorg.adb',
            'libgnat/g-bytswa.ads', 'libgnat/g-bytswa.adb',
            'libgnat/g-casuti.ads', 'libgnat/g-casuti.adb',
            'libgnat/g-comver.ads', 'libgnat/g-comver.adb',
            'libgnat/g-crc32.ads', 'libgnat/g-crc32.adb',
            'libgnat/g-debuti.ads', 'libgnat/g-debuti.adb',
            'libgnat/g-dyntab.ads', 'libgnat/g-dyntab.adb',
            'libgnat/g-except.ads',
            'libgnat/g-heasor.ads', 'libgnat/g-heasor.adb',
            'libgnat/g-hesora.ads', 'libgnat/g-hesora.adb',
            'libgnat/g-hesorg.ads', 'libgnat/g-hesorg.adb',
            'libgnat/g-htable.ads', 'libgnat/g-htable.adb',
            'libgnat/g-md5.ads', 'libgnat/g-md5.adb',
            'libgnat/g-moreex.ads', 'libgnat/g-moreex.adb',
            'libgnat/g-regexp.ads',
            'libgnat/g-sechas.ads', 'libgnat/g-sechas.adb',
            'libgnat/g-sehamd.ads', 'libgnat/g-sehamd.adb',
            'libgnat/g-sehash.ads', 'libgnat/g-sehash.adb',
            'libgnat/g-sha1.ads', 'libgnat/g-sha1.adb',
            'libgnat/g-sha224.ads',
            'libgnat/g-sha256.ads',
            'libgnat/g-sha384.ads',
            'libgnat/g-sha512.ads',
            'libgnat/g-shsh32.ads', 'libgnat/g-shsh32.adb',
            'libgnat/g-shsh64.ads', 'libgnat/g-shsh64.adb',
            'libgnat/g-shshco.ads', 'libgnat/g-shshco.adb',
            'libgnat/g-string.ads',
            'libgnat/g-strspl.ads',
            'libgnat/g-table.ads', 'libgnat/g-table.adb',
            'libgnat/g-tasloc.ads',
            'libgnat/g-wistsp.ads',
            'libgnat/i-c.ads', 'libgnat/i-c.adb',
            'libgnat/i-cobol.ads', 'libgnat/i-cobol.adb',
            'libgnat/i-cpoint.ads', 'libgnat/i-cpoint.adb',
            'libgnat/i-cstrin.ads', 'libgnat/i-cstrin.adb',
            'libgnat/i-fortra.ads', 'libgnat/i-fortra.adb',
            'libgnat/i-pacdec.ads', 'libgnat/i-pacdec.adb',
            'libgnat/ioexcept.ads',
            'raise-gcc.c',
            'raise.h',
            'libgnat/s-addima.ads', 'libgnat/s-addima.adb',
            'libgnat/s-addope.ads', 'libgnat/s-addope.adb',
            'libgnat/s-assert.adb',
            'libgnat/s-bitops.ads', 'libgnat/s-bitops.adb',
            'libgnat/s-boarop.ads',
            'libgnat/s-bytswa.ads',
            'libgnat/s-carsi8.ads', 'libgnat/s-carsi8.adb',
            'libgnat/s-carun8.ads', 'libgnat/s-carun8.adb',
            'libgnat/s-casi16.ads', 'libgnat/s-casi16.adb',
            'libgnat/s-casi32.ads', 'libgnat/s-casi32.adb',
            'libgnat/s-casi64.ads', 'libgnat/s-casi64.adb',
            'libgnat/s-caun16.ads', 'libgnat/s-caun16.adb',
            'libgnat/s-caun32.ads', 'libgnat/s-caun32.adb',
            'libgnat/s-caun64.ads', 'libgnat/s-caun64.adb',
            'libgnat/s-chepoo.ads',
            'libgnat/s-crc32.ads', 'libgnat/s-crc32.adb',
            'libgnat/s-excdeb.ads', 'libgnat/s-excdeb.adb',
            'libgnat/s-except.ads', 'libgnat/s-except.adb',
            'libgnat/s-exctab.ads', 'libgnat/s-exctab.adb',
            'libgnat/s-finmas.ads', 'libgnat/s-finmas.adb',
            'libgnat/s-finroo.ads', 'libgnat/s-finroo.adb',
            'libgnat/s-fore_d.ads', 'libgnat/s-fore_d.adb',
            'libgnat/s-fore_f.ads', 'libgnat/s-fore_f.adb',
            'libgnat/s-fode32.ads', 'libgnat/s-fode64.ads',
            'libgnat/s-fofi32.ads', 'libgnat/s-fofi64.ads',
            'libgnat/s-forrea.ads', 'libgnat/s-forrea.adb',
            'libgnat/s-geveop.ads', 'libgnat/s-geveop.adb',
            'libgnat/s-htable.ads', 'libgnat/s-htable.adb',
            'hie/s-init.ads',
            'libgnat/s-io.ads', 'hie/s-io.adb',
            'libgnat/s-mantis.ads', 'libgnat/s-mantis.adb',
            'libgnat/s-mastop.ads', 'libgnat/s-mastop.adb',
            'libgnat/s-memory.ads', 'hie/s-memory__xi.adb',
            'libgnat/s-pooglo.ads', 'libgnat/s-pooglo.adb',
            'libgnat/s-pooloc.ads', 'libgnat/s-pooloc.adb',
            'libgnat/s-poosiz.ads', 'libgnat/s-poosiz.adb',
            'libgnat/s-putima.ads', 'libgnat/s-putima.adb',
            'libgnat/s-rannum.ads', 'libgnat/s-rannum.adb',
            'libgnat/s-ransee.ads', 'hie/s-ransee.adb',
            'libgnat/s-regexp.ads', 'libgnat/s-regexp.adb',
            'libgnat/s-restri.ads', 'libgnat/s-restri.adb',
            'libgnat/s-rident.ads',
            'hie/s-soflin.ads', 'hie/s-soflin.adb',
            'libgnat/s-sopco3.ads', 'libgnat/s-sopco3.adb',
            'libgnat/s-sopco4.ads', 'libgnat/s-sopco4.adb',
            'libgnat/s-sopco5.ads', 'libgnat/s-sopco5.adb',
            'libgnat/s-spsufi.ads', 'libgnat/s-spsufi.adb',
            'libgnat/s-stalib.ads', 'libgnat/s-stalib.adb',
            'libgnat/s-stopoo.ads', 'libgnat/s-stopoo.adb',
            'libgnat/s-stposu.ads', 'libgnat/s-stposu.adb',
            'libgnat/s-strhas.ads', 'libgnat/s-strhas.adb',
            'libgnat/s-string.ads', 'libgnat/s-string.adb',
            'libgnat/s-tasloc.ads', 'libgnat/s-tasloc.adb',
            'hie/s-traceb__cert.ads',
            'libgnat/s-traent.ads', 'libgnat/s-traent.adb',
            'libgnat/s-trasym.ads', 'libgnat/s-trasym.adb',
            'libgnat/s-utf_32.ads', 'libgnat/s-utf_32.adb',
            'libgnat/s-veboop.ads', 'libgnat/s-veboop.adb',
            'libgnat/s-vector.ads', 'libgnat/s-vercon.adb',
            'libgnat/s-vercon.ads',
            'libgnat/s-wchcnv.ads', 'libgnat/s-wchcnv.adb',
            'libgnat/s-wchcon.ads', 'libgnat/s-wchcon.adb',
            'libgnat/s-wchjis.ads', 'libgnat/s-wchjis.adb',
            'libgnat/s-wchstw.ads', 'libgnat/s-wchstw.adb',
            'libgnat/s-wchwts.ads', 'libgnat/s-wchwts.adb',
            'libgnat/s-widboo.ads', 'libgnat/s-widboo.adb',
            'libgnat/s-widcha.ads', 'libgnat/s-widcha.adb',
            'libgnat/s-widenu.ads', 'libgnat/s-widenu.adb',
            'libgnat/s-widint.ads',
            'libgnat/s-widlli.ads',
            'libgnat/s-widllu.ads',
            'libgnat/s-widthi.ads', 'libgnat/s-widthi.adb',
            'libgnat/s-widthu.ads', 'libgnat/s-widthu.adb',
            'libgnat/s-widwch.ads', 'libgnat/s-widwch.adb',
            'libgnat/s-wwdcha.ads', 'libgnat/s-wwdcha.adb',
            'libgnat/s-wwdenu.ads', 'libgnat/s-wwdenu.adb',
            'libgnat/s-wwdwch.ads', 'libgnat/s-wwdwch.adb',
            'hie/tconfig.h',
            'hie/tsystem.h',
            'libgcc/unwind-pe.h'],
        'bb_srcs': [
            'hie/adaint-xi.c',
            'hie/a-calcon.ads', 'hie/a-calcon.adb',
            'hie/a-calclo__bb.adb',
            'hie/a-calend.ads', 'hie/a-calend.adb',
            'hie/s-init__bb.adb'],
        'pikeos_srcs': [
            'hie/s-init__pikeos.adb']
    },

    'full/32': {
        'conditions': ['RTS_Profile:ravenscar-full', 'Target_Word_Size:32'],
        'srcs': [
            'libgnat/a-decima.ads',
            'libgnat/s-scaval.ads', 'libgnat/s-scaval.adb']
    },

    'full/64': {
        'conditions': ['RTS_Profile:ravenscar-full', 'Target_Word_Size:64'],
        'srcs': [
            'libgnat/a-decima__128.ads',
            'libgnat/s-scaval__128.ads', 'libgnat/s-scaval__128.adb',
            'libgnat/s-fode128.ads', 'libgnat/s-fofi128.ads']
    },

    # Memory operations:
    'alloc/c': {
        'conditions': ['Has_libc:yes', 'RTS_Profile:!ravenscar-full'],
        'bb_srcs': ['hie/s-memory__libc.adb']
    },
    'alloc/no-tasking': {
        'conditions': ['Has_libc:no', 'RTS_Profile:zfp'],
        'srcs': ['hie/s-memory__zfp.adb']
    },
    'alloc/no-cas': {
        'conditions': ['Has_libc:no',
                       'RTS_Profile:ravenscar-sfp',
                       'Has_Compare_And_Swap:no'],
        'srcs': ['hie/s-memory__zfp.adb']
    },
    'alloc/tasking': {
        'conditions': ['Has_libc:no',
                       'RTS_Profile:ravenscar-sfp',
                       'Has_Compare_And_Swap:yes',
                       'Certifiable_Packages:no'],
        'srcs': ['hie/s-memory__raven.adb']
    },
    'alloc/tasking-noc': {
        'conditions': ['Has_libc:no',
                       'RTS_Profile:ravenscar-sfp',
                       'Has_Compare_And_Swap:yes',
                       'Certifiable_Packages:yes'],
        'srcs': ['hie/s-memory__raven_noc.adb']
    },
    'mem': {
        'conditions': ['Has_libc:no'],
        'srcs': [
            'hie/s-memtyp.ads',
            'hie/s-memcom.ads', 'hie/s-memcom.adb',
            'hie/s-memcop.ads', 'hie/s-memcop.adb',
            'hie/s-memmov.ads', 'hie/s-memmov.adb',
            'hie/s-memset.ads', 'hie/s-memset.adb']
    },

    # Integration with C
    'libc': {
        'conditions': ['Add_C_Integration:ada_clib'],
        'srcs': [
            'hie/s-c.ads',
            'hie/s-cerrno.ads', 'hie/s-cerrno.adb',
            'hie/s-cmallo.ads', 'hie/s-cmallo.adb',
            'hie/s-cstrle.ads', 'hie/s-cstrle.adb']
    },
    'newlib': {
        'conditions': ['Add_C_Integration:newlib'],
        'srcs': [
            'hie/newlib-bb.c']
    },

    # libgcc replacement
    'libgcc': {
        'conditions': ['Certifiable_Packages:yes'],
        'srcs': [
            'hie/s-gcc.ads',
            'hie/s-gcc-di.ads', 'hie/s-gcc-di.adb',
            'hie/s-gcdidi.ads', 'hie/s-gcdidi.adb']
    },

    'libgcc/arm': {
        'conditions': ['Certifiable_Packages:yes', 'CPU_Family:arm'],
        'srcs': [
            'hie/s-gcdifl__arm.ads', 'hie/s-gcdifl__arm.adb',
            'hie/s-gcdish__arm.ads', 'hie/s-gcdish.adb']
    },

    'libgcc/other_archs': {
        'conditions': ['Certifiable_Packages:yes', 'CPU_Family:!arm'],
        'srcs': [
            'hie/s-gcdifl.ads', 'hie/s-gcdifl.adb',
            'hie/s-gcdish.ads', 'hie/s-gcdish.adb']
    },

    # Text_IO
    'system_io': {
        'conditions': ['Text_IO:serial'],
        'bb_srcs': ['hie/a-textio__bb.adb'],
    },
    'semihosting': {
        'conditions': ['Text_IO:semihosting'],
        'bb_srcs': [
            'hie/s-semiho.ads', 'hie/s-semiho.adb',
            'hie/s-textio__semihosting.adb',
            'hie/a-textio__semihosting.adb']
    },
    # 'Image & 'Value utilities
    ''
    # 'Image support
    'image/enum': {
        'conditions': ['Add_Image_Enum:yes'],
        'srcs': [
            'libgnat/s-imagen.ads', 'libgnat/s-imagen.adb',
            'libgnat/s-imen16.ads',
            'libgnat/s-imen32.ads',
            'libgnat/s-imenu8.ads']
    },
    'image/decimal': {
        'conditions': ['Add_Image_Decimal:yes'],
        'srcs': [
            'libgnat/s-imaged.ads', 'libgnat/s-imaged.adb',
            'libgnat/s-imde32.ads'],
        'requires': ['Add_Image_Util:yes']
    },
    'image/decimal_ll': {
        'conditions': ['Add_Image_LL_Decimal:yes'],
        'srcs': [
            'libgnat/s-imde64.ads'],
        'requires': ['Add_Image_Decimal:yes']
    },
    'image/decimal_lll': {
        'conditions': ['Add_Image_LLL_Decimal:yes'],
        'srcs': [
            'libgnat/s-imde128.ads'],
        'requires': ['Add_Image_Decimal:yes']
    },
    'image/fixed': {
        'conditions': ['Add_Image_Fixed:yes'],
        'srcs': [
            'libgnat/s-arit32.ads', 'libgnat/s-arit32.adb',
            'libgnat/s-imagef.ads', 'libgnat/s-imagef.adb',
            'libgnat/s-imfi32.ads'],
        'requires': ['Add_Image_Util:yes']
    },
    'image/fixed_ll': {
        'conditions': ['Add_Image_LL_Fixed:yes'],
        'srcs': [
            'libgnat/s-imfi64.ads'],
        'requires': ['Add_Image_Fixed:yes', 'Add_Arith64:yes']
    },
    'image/fixed_lll': {
        'conditions': ['Add_Image_LLL_Fixed:yes'],
        'srcs': [
            'libgnat/s-imfi128.ads'],
        'requires': ['Add_Image_Fixed:yes', 'Add_Arith128:yes']
    },
    'image/float': {
        'conditions': ['Add_Image_Float:yes', 'Has_FPU:yes'],
        'srcs': [
            'libgnat/s-imager.ads', 'libgnat/s-imager.adb',
            'libgnat/s-imgrea.ads', 'libgnat/s-imgflt.ads',
            'libgnat/s-imglfl.ads', 'libgnat/s-imgllf.ads'],
        'requires': ['Add_Image_Util:yes', 'Add_Float_Util:yes'],
    },
    'image/int': {
        'conditions': ['Add_Image_Int:yes'],
        'srcs': [
            'libgnat/s-imagew.ads', 'libgnat/s-imagew.adb',
            'libgnat/s-imgwiu.ads']
    },
    'image/int_ll': {
        'conditions': ['Add_Image_LL_Int:yes'],
        'srcs': [
            'libgnat/s-imgllw.ads'],
        'requires': ['Add_Image_Int:yes']
    },
    'image/int_lll': {
        'conditions': ['Add_Image_LLL_Int:yes'],
        'srcs': [
            'libgnat/s-imglllw.ads'],
        'requires': ['Add_Image_Int:yes']
    },
    'image/based_int': {
        'conditions': ['Add_Image_Based_Int:yes'],
        'srcs': [
            'libgnat/s-imageb.ads', 'libgnat/s-imageb.adb',
            'libgnat/s-imgbiu.ads']
    },
    'image/based_int_ll': {
        'conditions': ['Add_Image_LL_Based_Int:yes'],
        'srcs': [
            'libgnat/s-imgllb.ads'],
        'requires': ['Add_Image_Based_Int:yes']
    },
    'image/based_int_lll': {
        'conditions': ['Add_Image_LLL_Based_Int:yes'],
        'srcs': [
            'libgnat/s-imglllb.ads'],
        'requires': ['Add_Image_Based_Int:yes']
    },
    'image/char': {
        'conditions': ['Add_Image_Char:yes'],
        'srcs': [
            'libgnat/s-imgcha.adb', 'libgnat/s-imgcha.ads']
    },
    'image/wide_char': {
        'conditions': ['Add_Image_Wide_Char:yes'],
        'srcs': [
            'libgnat/s-imgwch.ads', 'libgnat/s-imgwch.adb'],
        'requires': ['Add_Image_Char:yes']
    },
    'image/util': {
        'conditions': ['Add_Image_Util:yes'],
        'srcs': [
            'libgnat/s-imguti.ads', 'libgnat/s-imguti.adb'],
        'requires': ['Add_Image_Int:yes']
    },
    # 'Value support
    'value/Boolean': {
        'conditions': ['Add_Value_Bool:yes'],
        'srcs': [
            'libgnat/s-valboo.ads', 'libgnat/s-valboo.adb'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/enum': {
        'conditions': ['Add_Value_Enum:yes'],
        'srcs': [
            'libgnat/s-valuen.ads', 'libgnat/s-valuen.adb',
            'libgnat/s-vaen16.ads',
            'libgnat/s-vaen32.ads',
            'libgnat/s-vaenu8.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/decimal': {
        'conditions': ['Add_Value_Decimal:yes'],
        'srcs': [
            'libgnat/s-valuer.ads', 'libgnat/s-valuer.adb',
            'libgnat/s-valued.ads', 'libgnat/s-valued.adb',
            'libgnat/s-vade32.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/decimal_ll': {
        'conditions': ['Add_Value_LL_Decimal:yes'],
        'srcs': [
            'libgnat/s-vade64.ads'],
        'requires': ['Add_Value_Decimal:yes']
    },
    'value/decimal_lll': {
        'conditions': ['Add_Value_LLL_Decimal:yes'],
        'srcs': [
            'libgnat/s-vade128.ads'],
        'requires': ['Add_Value_Decimal:yes']
    },
    'value/fixed': {
        'conditions': ['Add_Value_Fixed:yes'],
        'srcs': [
            'libgnat/s-arit32.ads', 'libgnat/s-arit32.adb',
            'libgnat/s-valuer.ads', 'libgnat/s-valuer.adb',
            'libgnat/s-valuef.ads', 'libgnat/s-valuef.adb',
            'libgnat/s-vafi32.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/fixed_ll': {
        'conditions': ['Add_Value_LL_Fixed:yes'],
        'srcs': [
            'libgnat/s-vafi64.ads'],
        'requires': ['Add_Value_Fixed:yes', 'Add_Arith64:yes']
    },
    'value/fixed_lll': {
        'conditions': ['Add_Value_LLL_Fixed:yes'],
        'srcs': [
            'libgnat/s-vafi128.ads'],
        'requires': ['Add_Value_Fixed:yes', 'Add_Arith128:yes']
    },
    'value/float': {
        'conditions': ['Add_Value_Float:yes', 'Has_FPU:yes'],
        'srcs': [
            'libgnat/s-valuer.ads', 'libgnat/s-valuer.adb',
            'libgnat/s-valrea.ads', 'libgnat/s-valrea.adb',
            'libgnat/s-valflt.ads', 'libgnat/s-vallfl.ads',
            'libgnat/s-valllf.ads'],
        'requires': ['Add_Value_Utils:yes', 'Add_Float_Util:yes']
    },
    'value/int': {
        'conditions': ['Add_Value_Int:yes'],
        'srcs': [
            'libgnat/s-valint.ads',
            'libgnat/s-valueu.ads', 'libgnat/s-valueu.adb',
            'libgnat/s-valuei.ads', 'libgnat/s-valuei.adb',
            'libgnat/s-valuns.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/int_ll': {
        'conditions': ['Add_Value_LL_Int:yes'],
        'srcs': [
            'libgnat/s-vallli.ads',
            'libgnat/s-valllu.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/int_lll': {
        'conditions': ['Add_Value_LLL_Int:yes'],
        'srcs': [
            'libgnat/s-valllli.ads',
            'libgnat/s-vallllu.ads'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/char': {
        'conditions': ['Add_Value_Char:yes'],
        'srcs': [
            'libgnat/s-valcha.ads', 'libgnat/s-valcha.adb'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/wide_char': {
        'conditions': ['Add_Value_Wide_Char:yes'],
        'srcs': [
            'libgnat/s-valwch.ads', 'libgnat/s-valwch.adb'],
        'requires': ['Add_Value_Utils:yes']
    },
    'value/utils': {
        'conditions': ['Add_Value_Utils:yes'],
        'srcs': [
            'libgnat/s-valuti.ads', 'libgnat/s-valuti.adb'],
        'requires': ['Add_Case_Util:yes']
    },
    # Utility packages
    'utils/float_fma': {
        'conditions': ['Add_Float_Util:yes', 'Has_FMA:yes'],
        'srcs': [
            'libgnat/s-flocon.ads', 'libgnat/s-flocon__none.adb',
            'libgnat/s-dourea.ads', 'libgnat/s-dourea.adb',
            'libgnat/s-dorepr__fma.adb',
            'libgnat/s-powflt.ads', 'libgnat/s-powlfl.ads',
            'libgnat/s-powllf.ads']
    },
    'utils/float': {
        'conditions': ['Add_Float_Util:yes', 'Has_FMA:no'],
        'srcs': [
            'libgnat/s-flocon.ads', 'libgnat/s-flocon__none.adb',
            'libgnat/s-dourea.ads', 'libgnat/s-dourea.adb',
            'libgnat/s-dorepr.adb',
            'libgnat/s-powflt.ads', 'libgnat/s-powlfl.ads',
            'libgnat/s-powllf.ads']
    },
    'utils/case_util': {
        'conditions': ['Add_Case_Util:yes'],
        'srcs': [
            'libgnat/s-casuti.ads', 'libgnat/s-casuti.adb'],
    },
    'ioexce': {
        'conditions': ['Add_IO_Exceptions:yes'],
        'srcs': ['libgnat/a-ioexce.ads']
    },

    # FPU/Floating point operations support
    'fpu': {
        'conditions': ['Has_FPU:yes'],
        'srcs': [
            'libgnat/s-fatgen.ads', 'libgnat/s-fatgen.adb',
            'libgnat/s-fatflt.ads',
            'libgnat/s-fatlfl.ads',
            'libgnat/s-fatllf.ads'],
    },
    'math': {
        'conditions': ['Add_Math_Lib:!no'],
        'srcs': [
            'hie/a-ngelfu__ada.ads', 'hie/a-ngelfu__ada.adb',
            'hie/a-nlelfu__ada.ads',
            'libgnat/a-nselfu.ads',
            'hie/a-nuelfu__ada.ads',
            'hie/a-numaux__ada.ads',
            'libgnat/a-numeri.ads',
            'hie/s-gcmain__ada.ads', 'hie/s-gcmain__ada.adb',
            'libgnat/s-gearop.ads',  'libgnat/s-gearop.adb',
            'hie/s-libdou__ada.ads', 'hie/s-libdou__ada.adb',
            'hie/s-libm__ada.ads', 'hie/s-libm__ada.adb',
            'hie/s-libpre__ada.ads',
            'hie/s-libsin__ada.ads', 'hie/s-libsin__ada.adb',
            'hie/s-lidosq__ada.ads',
            'hie/s-lisisq__ada.ads']
    },
    'math/long_long_64bit': {
        'conditions': ['Add_Math_Lib:!no', 'CPU_Family:!x86_64'],
        'srcs': ['libgnat/a-nllefu.ads']
    },
    'math/long_long_80bit': {
        'conditions': ['Add_Math_Lib:!no', 'CPU_Family:x86_64'],
        'srcs': [
            'hie/s-lilodo__ada.ads', 'hie/s-lilodo__ada.adb',
            'hie/s-lldosq__ada.ads', 'hie/s-lldosq__fpu.adb',
            'hie/a-nllefu__ada.ads']
    },
    'math/complex': {
        'conditions': ['Add_Math_Lib:!no', 'Add_Complex_Type_Support:yes'],
        'srcs': [
            'libgnat/a-ncelfu.ads',
            'libgnat/a-ngcefu.ads', 'libgnat/a-ngcefu.adb',
            'libgnat/a-ngcoty.ads', 'hie/a-ngcoty__ada.adb',
            'libgnat/a-nlcefu.ads',
            'libgnat/a-nlcoty.ads',
            'libgnat/a-nllcef.ads',
            'libgnat/a-nllcty.ads',
            'libgnat/a-nscefu.ads',
            'libgnat/a-nscoty.ads',
            'libgnat/a-nucoty.ads']
    },
    'math/full': {
        'conditions': ['Add_Math_Lib:!no', 'RTS_Profile:ravenscar-full'],
        'srcs': [
            'libgnat/a-ngcoar.ads',
            'libgnat/a-ngcoar.adb',
            'libgnat/a-ngrear.ads',
            'libgnat/a-ngrear.adb',
            'libgnat/a-nurear.ads']
    },
    'math/softsp': {
        'conditions': ['Add_Math_Lib:softfloat,hardfloat_dp'],
        'srcs': ['hie/s-lisisq__ada.adb']
    },
    'math/softdp': {
        'conditions': ['Add_Math_Lib:softfloat,hardfloat_sp'],
        'srcs': ['hie/s-lidosq__ada.adb']
    },
    'math/hardsp': {
        'conditions': ['Add_Math_Lib:hardfloat,hardfloat_sp'],
        'srcs': ['hie/s-lisisq__fpu.adb']
    },
    'math/harddp': {
        'conditions': ['Add_Math_Lib:hardfloat,hardfloat_dp'],
        'srcs': ['hie/s-lidosq__fpu.adb']
    },
    # Arithmetic
    'arith64': {
        'conditions': ['Add_Arith64:yes'],
        'srcs': [
            'libgnat/s-arit64.ads', 'libgnat/s-arit64.adb',
            'libgnat/s-aridou.ads', 'libgnat/s-aridou.adb']
    },
    'arith128': {
        'conditions': ['Add_Arith128:yes'],
        'srcs': [
            'libgnat/s-arit128.ads', 'libgnat/s-arit128.adb'],
        'requires': ['Add_Arith64:yes']
    },
    'exponent/int': {
        'conditions': ['Add_Exponent_Int:yes'],
        'srcs': [
            'libgnat/s-exponn.ads', 'libgnat/s-exponn.adb',
            'libgnat/s-expont.ads', 'libgnat/s-expont.adb',
            'libgnat/s-exponu.ads', 'libgnat/s-exponu.adb',
            'libgnat/s-exnint.ads',
            'libgnat/s-expint.ads',
            'libgnat/s-expuns.ads'],
    },
    'exponent/int_ll': {
        'conditions': ['Add_Exponent_LL_Int:yes'],
        'srcs': [
            'libgnat/s-exnlli.ads',
            'libgnat/s-explli.ads',
            'libgnat/s-expllu.ads'],
        'requires': ['Add_Exponent_Int:yes']
    },
    'exponent/int_lll': {
        'conditions': ['Add_Exponent_LLL_Int:yes'],
        'srcs': [
            'libgnat/s-exnllli.ads',
            'libgnat/s-expllli.ads',
            'libgnat/s-explllu.ads'],
        'requires': ['Add_Exponent_Int:yes']
    },
    'exponent/mod': {
        'conditions': ['Add_Exponent_Modular:yes'],
        'srcs': [
            'libgnat/s-expmod.ads', 'libgnat/s-expmod.adb'],
    },
    'exponent/float': {
        'conditions': ['Add_Exponent_Float:yes'],
        'srcs': [
            'libgnat/s-exponr.ads', 'libgnat/s-exponr.adb',
            'libgnat/s-exnflt.ads', 'libgnat/s-exnlfl.ads',
            'libgnat/s-exnllf.ads'],
    },

    # Ada streams
    'streams': {
        'conditions': ['Add_Streams:yes'],
        'requires': ['Add_IO_Exceptions:yes'],
        'srcs': [
            'libgnat/a-stream.ads', 'libgnat/a-stream.adb',
            'libgnat/s-stratt.ads', 'libgnat/s-stratt.adb',
            'libgnat/s-statxd.ads', 'libgnat/s-statxd.adb']
    },

    # Zero-cost-exception support
    'full/zcx-arm': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:arm'],
        'srcs': [
            'libgnat/s-excmac__arm.adb', 'libgnat/s-excmac__arm.ads',
            'hie/s-traceb__armeabi.adb']
    },
    'full/zcx-dw2': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:!arm'],
        'srcs': [
            'libgnat/s-excmac__gcc.ads', 'libgnat/s-excmac__gcc.adb',
            'libgcc/unwind-dw2-fde.h'],
        'bb_srcs': [
            'hie/unwind-dw2-fde-bb.c']
    },
    'full/zcx-aarch64': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:aarch64'],
        'srcs': ['hie/s-traceb__dwarf.adb']
    },
    'full/zcx-ppc': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:powerpc'],
        'srcs': ['hie/s-traceb__ppc.adb']
    },

    'full/zcx-riscv': {
        'conditions':
        ['RTS_Profile:ravenscar-full', 'CPU_Family:riscv64,riscv32'],
        'srcs': ['hie/s-traceb__dwarf.adb']
    },

    'full/zcx-leon': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:leon'],
        'srcs': ['hie/s-traceb__sparc.adb']
    },
    'full/zcx-x86': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:x86'],
        'srcs': ['hie/s-traceb__vx653-sim.adb']
    },
    'full/zcx-x86_64': {
        'conditions': ['RTS_Profile:ravenscar-full', 'CPU_Family:x86_64'],
        'srcs': ['hie/s-traceb__dwarf.adb']
    },

    # Containers
    'containers': {
        'conditions': ['RTS_Profile:ravenscar-full'],
        'srcs': [
            'libgnat/a-btgbso.adb', 'libgnat/a-btgbso.ads',
            'libgnat/a-cbdlli.adb', 'libgnat/a-cbdlli.ads',
            'libgnat/a-cbhama.adb', 'libgnat/a-cbhama.ads',
            'libgnat/a-cbhase.adb', 'libgnat/a-cbhase.ads',
            'libgnat/a-cbmutr.adb', 'libgnat/a-cbmutr.ads',
            'libgnat/a-cborma.adb', 'libgnat/a-cborma.ads',
            'libgnat/a-cborse.adb', 'libgnat/a-cborse.ads',
            'libgnat/a-cdlili.adb', 'libgnat/a-cdlili.ads',
            'libgnat/a-cfdlli.adb', 'libgnat/a-cfdlli.ads',
            'libgnat/a-cfhama.adb', 'libgnat/a-cfhama.ads',
            'libgnat/a-cfhase.adb', 'libgnat/a-cfhase.ads',
            'libgnat/a-cfinve.adb', 'libgnat/a-cfinve.ads',
            'libgnat/a-cforma.adb', 'libgnat/a-cforma.ads',
            'libgnat/a-cforse.adb', 'libgnat/a-cforse.ads',
            'libgnat/a-cgaaso.adb', 'libgnat/a-cgaaso.ads',
            'libgnat/a-cgarso.adb', 'libgnat/a-cgarso.ads',
            'libgnat/a-cgcaso.adb', 'libgnat/a-cgcaso.ads',
            'libgnat/a-chtgbk.adb', 'libgnat/a-chtgbk.ads',
            'libgnat/a-chtgbo.adb', 'libgnat/a-chtgbo.ads',
            'libgnat/a-chtgke.adb', 'libgnat/a-chtgke.ads',
            'libgnat/a-chtgop.adb', 'libgnat/a-chtgop.ads',
            'libgnat/a-cidlli.adb', 'libgnat/a-cidlli.ads',
            'libgnat/a-cihama.adb', 'libgnat/a-cihama.ads',
            'libgnat/a-cihase.adb', 'libgnat/a-cihase.ads',
            'libgnat/a-cimutr.adb', 'libgnat/a-cimutr.ads',
            'libgnat/a-ciorma.adb', 'libgnat/a-ciorma.ads',
            'libgnat/a-ciormu.adb', 'libgnat/a-ciormu.ads',
            'libgnat/a-ciorse.adb', 'libgnat/a-ciorse.ads',
            'libgnat/a-coboho.adb', 'libgnat/a-coboho.ads',
            'libgnat/a-cobove.adb', 'libgnat/a-cobove.ads',
            'libgnat/a-cofove.adb', 'libgnat/a-cofove.ads',
            'libgnat/a-cofuba.adb', 'libgnat/a-cofuba.ads',
            'libgnat/a-cofuma.adb', 'libgnat/a-cofuma.ads',
            'libgnat/a-cofuse.adb', 'libgnat/a-cofuse.ads',
            'libgnat/a-cofuve.adb', 'libgnat/a-cofuve.ads',
            'libgnat/a-cogeso.adb', 'libgnat/a-cogeso.ads',
            'libgnat/a-cohama.adb', 'libgnat/a-cohama.ads',
            'libgnat/a-cohase.adb', 'libgnat/a-cohase.ads',
            'libgnat/a-cohata.ads',
            'libgnat/a-coinho__shared.adb', 'libgnat/a-coinho__shared.ads',
            'libgnat/a-coinve.adb', 'libgnat/a-coinve.ads',
            'libgnat/a-comutr.adb', 'libgnat/a-comutr.ads',
            'libgnat/a-conhel.adb', 'libgnat/a-conhel.ads',
            'libgnat/a-contai.ads',
            'libgnat/a-convec.adb', 'libgnat/a-convec.ads',
            'libgnat/a-coorma.adb', 'libgnat/a-coorma.ads',
            'libgnat/a-coormu.adb', 'libgnat/a-coormu.ads',
            'libgnat/a-coorse.adb', 'libgnat/a-coorse.ads',
            'libgnat/a-coprnu.adb', 'libgnat/a-coprnu.ads',
            'libgnat/a-crbltr.ads',
            'libgnat/a-crbtgk.adb', 'libgnat/a-crbtgk.ads',
            'libgnat/a-crbtgo.adb', 'libgnat/a-crbtgo.ads',
            'libgnat/a-crdlli.adb', 'libgnat/a-crdlli.ads',
            'libgnat/a-csquin.ads',
            'libgnat/a-rbtgbk.adb', 'libgnat/a-rbtgbk.ads',
            'libgnat/a-rbtgbo.adb', 'libgnat/a-rbtgbo.ads',
            'libgnat/a-rbtgso.adb', 'libgnat/a-rbtgso.ads',
            'libgnat/a-iteint.ads',
            'libgnat/s-atocou__builtin.adb', 'libgnat/s-atocou.ads']
    },
    # Pragma Pack support
    'pack': {
        'conditions': ['Add_Pack:yes'],
        'srcs': [
            'libgnat/s-pack03.ads', 'libgnat/s-pack03.adb',
            'libgnat/s-pack05.ads', 'libgnat/s-pack05.adb',
            'libgnat/s-pack06.ads', 'libgnat/s-pack06.adb',
            'libgnat/s-pack07.ads', 'libgnat/s-pack07.adb',
            'libgnat/s-pack09.ads', 'libgnat/s-pack09.adb',
            'libgnat/s-pack10.ads', 'libgnat/s-pack10.adb',
            'libgnat/s-pack11.ads', 'libgnat/s-pack11.adb',
            'libgnat/s-pack12.ads', 'libgnat/s-pack12.adb',
            'libgnat/s-pack13.ads', 'libgnat/s-pack13.adb',
            'libgnat/s-pack14.ads', 'libgnat/s-pack14.adb',
            'libgnat/s-pack15.ads', 'libgnat/s-pack15.adb',
            'libgnat/s-pack17.ads', 'libgnat/s-pack17.adb',
            'libgnat/s-pack18.ads', 'libgnat/s-pack18.adb',
            'libgnat/s-pack19.ads', 'libgnat/s-pack19.adb',
            'libgnat/s-pack20.ads', 'libgnat/s-pack20.adb',
            'libgnat/s-pack21.ads', 'libgnat/s-pack21.adb',
            'libgnat/s-pack22.ads', 'libgnat/s-pack22.adb',
            'libgnat/s-pack23.ads', 'libgnat/s-pack23.adb',
            'libgnat/s-pack24.ads', 'libgnat/s-pack24.adb',
            'libgnat/s-pack25.ads', 'libgnat/s-pack25.adb',
            'libgnat/s-pack26.ads', 'libgnat/s-pack26.adb',
            'libgnat/s-pack27.ads', 'libgnat/s-pack27.adb',
            'libgnat/s-pack28.ads', 'libgnat/s-pack28.adb',
            'libgnat/s-pack29.ads', 'libgnat/s-pack29.adb',
            'libgnat/s-pack30.ads', 'libgnat/s-pack30.adb',
            'libgnat/s-pack31.ads', 'libgnat/s-pack31.adb',
            'libgnat/s-pack33.ads', 'libgnat/s-pack33.adb',
            'libgnat/s-pack34.ads', 'libgnat/s-pack34.adb',
            'libgnat/s-pack35.ads', 'libgnat/s-pack35.adb',
            'libgnat/s-pack36.ads', 'libgnat/s-pack36.adb',
            'libgnat/s-pack37.ads', 'libgnat/s-pack37.adb',
            'libgnat/s-pack38.ads', 'libgnat/s-pack38.adb',
            'libgnat/s-pack39.ads', 'libgnat/s-pack39.adb',
            'libgnat/s-pack40.ads', 'libgnat/s-pack40.adb',
            'libgnat/s-pack41.ads', 'libgnat/s-pack41.adb',
            'libgnat/s-pack42.ads', 'libgnat/s-pack42.adb',
            'libgnat/s-pack43.ads', 'libgnat/s-pack43.adb',
            'libgnat/s-pack44.ads', 'libgnat/s-pack44.adb',
            'libgnat/s-pack45.ads', 'libgnat/s-pack45.adb',
            'libgnat/s-pack46.ads', 'libgnat/s-pack46.adb',
            'libgnat/s-pack47.ads', 'libgnat/s-pack47.adb',
            'libgnat/s-pack48.ads', 'libgnat/s-pack48.adb',
            'libgnat/s-pack49.ads', 'libgnat/s-pack49.adb',
            'libgnat/s-pack50.ads', 'libgnat/s-pack50.adb',
            'libgnat/s-pack51.ads', 'libgnat/s-pack51.adb',
            'libgnat/s-pack52.ads', 'libgnat/s-pack52.adb',
            'libgnat/s-pack53.ads', 'libgnat/s-pack53.adb',
            'libgnat/s-pack54.ads', 'libgnat/s-pack54.adb',
            'libgnat/s-pack55.ads', 'libgnat/s-pack55.adb',
            'libgnat/s-pack56.ads', 'libgnat/s-pack56.adb',
            'libgnat/s-pack57.ads', 'libgnat/s-pack57.adb',
            'libgnat/s-pack58.ads', 'libgnat/s-pack58.adb',
            'libgnat/s-pack59.ads', 'libgnat/s-pack59.adb',
            'libgnat/s-pack60.ads', 'libgnat/s-pack60.adb',
            'libgnat/s-pack61.ads', 'libgnat/s-pack61.adb',
            'libgnat/s-pack62.ads', 'libgnat/s-pack62.adb',
            'libgnat/s-pack63.ads', 'libgnat/s-pack63.adb']
    },
    # Pragma Pack support on 64-bit platforms
    'pack64': {
        'conditions': ['Add_Pack64:yes'],
        'srcs': [
            'libgnat/s-pack65.ads', 'libgnat/s-pack65.adb',
            'libgnat/s-pack66.ads', 'libgnat/s-pack66.adb',
            'libgnat/s-pack67.ads', 'libgnat/s-pack67.adb',
            'libgnat/s-pack68.ads', 'libgnat/s-pack68.adb',
            'libgnat/s-pack69.ads', 'libgnat/s-pack69.adb',
            'libgnat/s-pack70.ads', 'libgnat/s-pack70.adb',
            'libgnat/s-pack71.ads', 'libgnat/s-pack71.adb',
            'libgnat/s-pack72.ads', 'libgnat/s-pack72.adb',
            'libgnat/s-pack73.ads', 'libgnat/s-pack73.adb',
            'libgnat/s-pack74.ads', 'libgnat/s-pack74.adb',
            'libgnat/s-pack75.ads', 'libgnat/s-pack75.adb',
            'libgnat/s-pack76.ads', 'libgnat/s-pack76.adb',
            'libgnat/s-pack77.ads', 'libgnat/s-pack77.adb',
            'libgnat/s-pack78.ads', 'libgnat/s-pack78.adb',
            'libgnat/s-pack79.ads', 'libgnat/s-pack79.adb',
            'libgnat/s-pack80.ads', 'libgnat/s-pack80.adb',
            'libgnat/s-pack81.ads', 'libgnat/s-pack81.adb',
            'libgnat/s-pack82.ads', 'libgnat/s-pack82.adb',
            'libgnat/s-pack83.ads', 'libgnat/s-pack83.adb',
            'libgnat/s-pack84.ads', 'libgnat/s-pack84.adb',
            'libgnat/s-pack85.ads', 'libgnat/s-pack85.adb',
            'libgnat/s-pack86.ads', 'libgnat/s-pack86.adb',
            'libgnat/s-pack87.ads', 'libgnat/s-pack87.adb',
            'libgnat/s-pack88.ads', 'libgnat/s-pack88.adb',
            'libgnat/s-pack89.ads', 'libgnat/s-pack89.adb',
            'libgnat/s-pack90.ads', 'libgnat/s-pack90.adb',
            'libgnat/s-pack91.ads', 'libgnat/s-pack91.adb',
            'libgnat/s-pack92.ads', 'libgnat/s-pack92.adb',
            'libgnat/s-pack93.ads', 'libgnat/s-pack93.adb',
            'libgnat/s-pack94.ads', 'libgnat/s-pack94.adb',
            'libgnat/s-pack95.ads', 'libgnat/s-pack95.adb',
            'libgnat/s-pack96.ads', 'libgnat/s-pack96.adb',
            'libgnat/s-pack97.ads', 'libgnat/s-pack97.adb',
            'libgnat/s-pack98.ads', 'libgnat/s-pack98.adb',
            'libgnat/s-pack99.ads', 'libgnat/s-pack99.adb',
            'libgnat/s-pack100.ads', 'libgnat/s-pack100.adb',
            'libgnat/s-pack101.ads', 'libgnat/s-pack101.adb',
            'libgnat/s-pack102.ads', 'libgnat/s-pack102.adb',
            'libgnat/s-pack103.ads', 'libgnat/s-pack103.adb',
            'libgnat/s-pack104.ads', 'libgnat/s-pack104.adb',
            'libgnat/s-pack105.ads', 'libgnat/s-pack105.adb',
            'libgnat/s-pack106.ads', 'libgnat/s-pack106.adb',
            'libgnat/s-pack107.ads', 'libgnat/s-pack107.adb',
            'libgnat/s-pack108.ads', 'libgnat/s-pack108.adb',
            'libgnat/s-pack109.ads', 'libgnat/s-pack109.adb',
            'libgnat/s-pack110.ads', 'libgnat/s-pack110.adb',
            'libgnat/s-pack111.ads', 'libgnat/s-pack111.adb',
            'libgnat/s-pack112.ads', 'libgnat/s-pack112.adb',
            'libgnat/s-pack113.ads', 'libgnat/s-pack113.adb',
            'libgnat/s-pack114.ads', 'libgnat/s-pack114.adb',
            'libgnat/s-pack115.ads', 'libgnat/s-pack115.adb',
            'libgnat/s-pack116.ads', 'libgnat/s-pack116.adb',
            'libgnat/s-pack117.ads', 'libgnat/s-pack117.adb',
            'libgnat/s-pack118.ads', 'libgnat/s-pack118.adb',
            'libgnat/s-pack119.ads', 'libgnat/s-pack119.adb',
            'libgnat/s-pack120.ads', 'libgnat/s-pack120.adb',
            'libgnat/s-pack121.ads', 'libgnat/s-pack121.adb',
            'libgnat/s-pack122.ads', 'libgnat/s-pack122.adb',
            'libgnat/s-pack123.ads', 'libgnat/s-pack123.adb',
            'libgnat/s-pack124.ads', 'libgnat/s-pack124.adb',
            'libgnat/s-pack125.ads', 'libgnat/s-pack125.adb',
            'libgnat/s-pack126.ads', 'libgnat/s-pack126.adb',
            'libgnat/s-pack127.ads', 'libgnat/s-pack127.adb'],
        'requires': ['Add_Pack:yes']
    },
    # LIBGNARL

    'gnarl/common': {
        'srcs': [
            'libgnarl/a-interr.ads', 'hie/a-interr__raven.adb',
            'hie/a-reatim.ads', 'hie/a-reatim.adb',
            'libgnarl/a-retide.ads', 'hie/a-retide__raven.adb',
            'hie/a-sytaco__xi.ads', 'hie/a-sytaco__xi.adb',
            'libgnarl/a-taside.ads', 'hie/a-taside__raven.adb',
            'hie/a-taster.ads', 'hie/a-taster.adb',
            'hie/s-interr__raven.ads',
            'hie/s-mufalo.ads', 'hie/s-mufalo.adb',
            'hie/s-musplo.ads',
            'hie/s-taprob__raven.ads', 'hie/s-taprob__raven.adb',
            'hie/s-taprop.ads',
            'libgnarl/s-tarest.ads', 'hie/s-tarest.adb',
            'hie/s-tasdeb__xi.ads', 'hie/s-tasdeb__raven.adb',
            'libgnarl/s-tasinf.ads', 'libgnarl/s-tasinf.adb',
            'hie/s-taskin.adb',
            'libgnarl/s-tasres.ads',
            'libgnarl/s-tpobmu.ads'],
        'bb_srcs': [
            'hie/a-exetim.ads', 'hie/a-exetim.adb',
            'hie/a-extiin.ads', 'hie/a-extiin.adb',
            'hie/a-rttiev.ads', 'hie/a-rttiev.adb',
            'hie/s-bbexti.ads', 'hie/s-bbexti.adb',
            'hie/s-bbinte.ads', 'hie/s-bbinte.adb',
            'hie/s-bbprot.ads', 'hie/s-bbprot.adb',
            'hie/s-bbthqu.ads', 'hie/s-bbthqu.adb',
            'hie/s-bbthre.ads', 'hie/s-bbthre.adb',
            'hie/s-bbtiev.ads', 'hie/s-bbtiev.adb',
            'hie/s-bbtime.ads',
            'hie/s-bcprmu.ads', 'hie/s-bcprmu.adb',
            'hie/s-interr.adb',
            'hie/s-multip.ads', 'hie/s-multip.adb',
            'hie/s-taprop__bb.adb',
            'hie/s-taspri.ads',
            'hie/s-tpobmu.adb',
            'hie/s-osinte.ads'],
        'pikeos_srcs': [
            'libgnat/text_io.ads',
            'hie/a-textio.ads', 'hie/a-textio__pikeos.adb',
            'hie/s-multip__raven-default.ads',
            'hie/s-multip__raven-default.adb',
            'hie/s-musplo.adb',
            'hie/s-taprop__pikeos.adb',
            'hie/s-taspri__pikeos.ads',
            'libgnarl/s-tpobmu.adb']
    },
    'gnarl/pikeos3': {
        'conditions': ['Pikeos_Version:pikeos3'],
        'pikeos_srcs': [
            'hie/s-interr__pikeos3.adb',
            'hie/s-osinte__pikeos3.ads', 'hie/s-osinte__pikeos3.adb']
    },
    'gnarl/pikeos4': {
        'conditions': ['Pikeos_Version:pikeos4'],
        'pikeos_srcs': [
            'hie/s-interr__pikeos4.adb',
            'hie/s-osinte__pikeos4.ads', 'hie/s-osinte__pikeos4.adb']
    },

    'gnarl/pikeos4.2': {
        'conditions': ['Pikeos_Version:pikeos4.2'],
        'pikeos_srcs': [
            'hie/s-interr__pikeos4.adb',
            'hie/s-osinte__pikeos4.ads', 'hie/s-osinte__p4ext.adb']
    },

    'gnarl/pikeos5': {
        'conditions': ['Pikeos_Version:pikeos5'],
        'pikeos_srcs': [
            'hie/s-interr__pikeos4.adb',
            'hie/s-osinte__pikeos4.ads', 'hie/s-osinte__p4ext.adb']
    },

    # SFP-specific files
    'gnarl/sfp': {
        'conditions': ['RTS_Profile:ravenscar-sfp'],
        'srcs': [
            'hie/s-taskin__raven.ads',
            'hie/s-tposen__raven.ads', 'hie/s-tposen__raven.adb']
    },

    # Ravenscar-full
    'gnarl/full': {
        'conditions': ['RTS_Profile:ravenscar-full'],
        'srcs': [
            'hie/s-taskin__full.ads',
            'hie/s-tposen__xi-full.adb', 'hie/s-tposen__xi-full.ads'],
        'bb_srcs': [
            'hie/s-btstch.ads', 'hie/s-btstch.adb'],
    },

    # Extended Ravenscar profile
    'gnarl/full/extended': {
        'conditions': ['RTS_Profile:ravenscar-full'],
        'srcs': [
            # relative delays:
            'hie/s-reldel.ads', 'hie/s-reldel.adb',
            # multiple entries:
            'libgnarl/a-synbar.adb', 'libgnarl/a-synbar.ads',
            'libgnarl/g-boubuf.adb', 'libgnarl/g-boubuf.ads',
            'libgnarl/g-boumai.ads',
            'libgnarl/g-semaph.adb', 'libgnarl/g-semaph.ads',
            'hie/s-tasque.ads', 'hie/s-tasque.adb',
            'hie/s-tpoben__bb.ads', 'hie/s-tpoben__bb.adb',
            'hie/s-tpobop__bb.ads', 'hie/s-tpobop__bb.adb']
    },

    # Timing support
    'gnarl/timer32': {
        'conditions': ['Timer:timer32'],
        'srcs': [
            'hie/s-bbbosu__timer-32.ads',
            'hie/s-bbtime__timer-32.adb']
    },
    'gnarl/timer64': {
        'conditions': ['Timer:timer64'],
        'srcs': [
            'hie/s-bbbosu__timer-64.ads',
            'hie/s-bbtime__timer-64.adb']
    },

    # spinlock support
    'gnarl/spinlock-gcc': {
        'conditions': ['CPU_Family:!leon'],
        'bb_srcs': ['hie/s-musplo.adb']
    },
    'gnarl/spinlock-leon': {
        'conditions': ['CPU_Family:leon'],
        'bb_srcs': ['hie/s-musplo__leon.adb']
    },

    # memory profile
    'gnat/parameters/small': {
        'conditions': ['RTS_Profile:!zfp', 'Memory_Profile:small'],
        'bb_srcs': ['hie/s-parame__small.ads',
                    'hie/s-parame.adb']
    },
    'gnat/parameters/large': {
        'conditions': ['RTS_Profile:!zfp', 'Memory_Profile:large'],
        'bb_srcs': ['hie/s-parame__large.ads',
                    'hie/s-parame.adb']
    },
    'gnat/parameters/huge': {
        'conditions': ['RTS_Profile:!zfp', 'Memory_Profile:huge'],
        'bb_srcs': ['hie/s-parame__huge.ads',
                    'hie/s-parame.adb']
    },
    'gnat/parameters/zfp-small': {
        'conditions': ['RTS_Profile:zfp', 'Memory_Profile:small'],
        'bb_srcs': ['hie/s-parame__zfp_small.ads']
    },
    'gnat/parameters/zfp-large': {
        'conditions': ['RTS_Profile:zfp', 'Memory_Profile:large'],
        'bb_srcs': ['hie/s-parame__zfp.ads']
    },
    'gnat/parameters/zfp-huge': {
        'conditions': ['RTS_Profile:zfp', 'Memory_Profile:huge'],
        'bb_srcs': ['hie/s-parame__zfp_huge.ads']
    },
}
