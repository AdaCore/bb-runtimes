#! /usr/bin/env python
#
# Copyright (C) 2016-2017, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support.files_holder import FilesHolder

import os
import sys


class SharedFilesHolder(FilesHolder):
    def update_pairs(self, dir, pairs):
        # overload the parent method: this one allows updating pairs in a
        # specific directory. This is needed in the shared sources case as
        # it is expected to have different version of the same source in
        # different sub-directories
        for k, v in pairs.items():
            if k not in self.dirs[dir]:
                print "in update_pairs: no such source: %s" % k
            else:
                self.dirs[dir][k] = v
        return True


class RTSOptions(object):
    """Defines the scenarii in the shared rts projects"""

    scenario = {
        'RTS_Profile': ['zfp', 'ravenscar-sfp', 'ravenscar-full'],
        'CPU_Family': ['arm', 'aarch64', 'leon', 'powerpc', 'x86'],
        'Has_FPU': ['no', 'yes'],
        'Memory_Profile': ['small', 'large'],
        'Timer': ['timer32', 'timer64'],
        'Pikeos_Version': ['pikeos3', 'pikeos4'],
        'Add_Math_Lib': [
            'no', 'softfloat', 'hardfloat',
            'hardfloat_dp', 'hardfloat_sp'],
        'Add_Arith64': ['no', 'yes'],
        # 'Image:
        'Add_Image_Enum': ['no', 'yes'],
        'Add_Image_Int': ['no', 'yes'],
        'Add_Image_LL_Int': ['no', 'yes'],
        'Add_Image_Based_Int': ['no', 'yes'],
        'Add_Image_LL_Based_Int': ['no', 'yes'],
        'Add_Image_Decimal': ['no', 'yes'],
        'Add_Image_LL_Decimal': ['no', 'yes'],
        'Add_Image_Float': ['no', 'yes'],
        'Add_Image_Char': ['no', 'yes'],
        'Add_Image_Wide_Char': ['no', 'yes'],
        # Exponentiation:
        'Add_Exponent_Int': ['no', 'yes'],
        'Add_Exponent_LL_Int': ['no', 'yes'],
        'Add_Exponent_Modular': ['no', 'yes'],
        'Add_Exponent_LL_Float': ['no', 'yes'],

        'Add_Memory_Operations': ['no', 'yes'],
        'Add_C_Support': ['no', 'ada_clib', 'newlib'],
        'Use_Semihosting_IO': ['no', 'yes']}

    def __init__(self, config):
        self.config = config

    def zfp_scenarios(self, mem_routines, math_lib):
        """Returns the list of directories contained in a base ZFP runtime"""
        ret = {}
        ret['RTS_Profile'] = 'zfp'

        if self.config.has_fpu:
            ret['Has_FPU'] = 'yes'
        else:
            ret['Has_FPU'] = 'no'

        if mem_routines:
            ret['Add_Memory_Operations'] = 'yes'
        else:
            ret['Add_Memory_Operations'] = 'no'

        if math_lib:
            if not self.config.is_pikeos:
                if self.config.has_single_precision_fpu:
                    if self.config.has_double_precision_fpu:
                        # Full hardware
                        ret['Add_Math_Lib'] = 'hardfloat'
                    else:
                        # Hardware only for SP.
                        ret['Add_Math_Lib'] = 'hardfloat_sp'
                else:
                    if self.config.has_double_precision_fpu:
                        # Hardware only for DP.
                        ret['Add_Math_Lib'] = 'hardfloat_dp'
                    else:
                        # No hardware support
                        ret['Add_Math_Lib'] = 'softfloat'
            else:
                ret['Add_Math_Lib'] = 'hardfloat'
        else:
            ret['Add_Math_Lib'] = 'no'

        ret['Add_C_Support'] = "no"
        ret['Add_Arith64'] = "no"
        ret['Add_Exponent_Int'] = "no"
        ret['Add_Exponent_LL_Int'] = "no"
        ret['Add_Exponent_Modular'] = "no"
        ret['Add_Exponent_LL_Float'] = "no"
        ret['Add_Image_Enum'] = "no"
        ret['Add_Image_Decimal'] = "no"
        ret['Add_Image_LL_Decimal'] = "no"
        ret['Add_Image_Float'] = "no"
        ret['Add_Image_Int'] = "no"
        ret['Add_Image_LL_Int'] = "no"
        ret['Add_Image_Based_Int'] = "no"
        ret['Add_Image_LL_Based_Int'] = "no"
        ret['Add_Image_Char'] = "no"
        ret['Add_Image_Wide_Char'] = "no"

        if self.config.use_semihosting_io:
            ret['Use_Semihosting_IO'] = 'yes'
        else:
            ret['Use_Semihosting_IO'] = 'no'

        return ret

    def sfp_scenarios(self, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base SFP runtime"""
        ret = self.zfp_scenarios(mem_routines, math_lib)
        ret['RTS_Profile'] = 'ravenscar-sfp'

        ret['Add_Image_Enum'] = 'yes'

        if self.config.target is not None:
            cpu = self.config.target.split('-')[0]

            if cpu in ('aarch64', ):
                ret['CPU_Family'] = 'aarch64'
            elif cpu in ('arm', ):
                ret['CPU_Family'] = 'arm'
            elif cpu in ('powerpc', 'ppc'):
                ret['CPU_Family'] = 'powerpc'
            elif cpu.startswith('leon'):
                ret['CPU_Family'] = 'leon'
            elif cpu in ('x86',):
                ret['CPU_Family'] = 'x86'
            elif cpu in ('visium',):
                ret['CPU_Family'] = 'visium'
            else:
                print "Unexpected cpu %s" % cpu
                print "  not in 'arm', 'aarch64', 'powerpc', 'leon', 'leon3'"
                sys.exit(2)

        if not self.config.is_pikeos:
            # source installation for PikeOS do not consider those
            if self.config.has_timer_64:
                ret['Timer'] = 'timer64'
            else:
                ret['Timer'] = 'timer32'

            if small_mem:
                ret['Memory_Profile'] = 'small'
            else:
                ret['Memory_Profile'] = 'large'

        else:
            ret['Pikeos_Version'] = self.config.pikeos_version

        return ret

    def full_scenarios(self, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base full runtime"""
        ret = self.sfp_scenarios(mem_routines, math_lib, small_mem)

        # override the RTS value
        ret['RTS_Profile'] = 'ravenscar-full'
        ret['Add_Arith64'] = "yes"
        ret['Add_Exponent_Int'] = "yes"
        ret['Add_Exponent_LL_Int'] = "yes"
        ret['Add_Exponent_Modular'] = "yes"
        ret['Add_Exponent_LL_Float'] = "yes"
        ret['Add_Image_Int'] = "yes"
        ret['Add_Image_LL_Int'] = "yes"
        ret['Add_Image_Based_Int'] = "yes"
        ret['Add_Image_LL_Based_Int'] = "yes"
        ret['Add_Image_Decimal'] = "yes"
        ret['Add_Image_LL_Decimal'] = "yes"
        ret['Add_Image_Float'] = "yes"
        ret['Add_Image_Char'] = "yes"
        ret['Add_Image_Wide_Char'] = "yes"

        if not self.config.is_pikeos:
            # PikeOS provides its own C library
            # ravenscar-full requires C memory operations, either via newlib
            # or via our own implementation in Ada
            ret['Add_C_Support'] = "newlib"

        return ret


# Definitions of shared source files.
# Keep spec and body on one line.

class SourceDirs(SharedFilesHolder):
    dest_sources = None
    dest_prjs = None

    def __init__(self, is_bb):
        super(SourceDirs, self).__init__()
        self._is_bb = is_bb
        self.scenario = RTSOptions.scenario
        self.libgnat_scenarios = []
        self.libgnarl_scenarios = []
        self.gnat_rules = {}
        self.gnarl_rules = {}

    def dump_scenario(self, var, current, indent):
        blank = ' ' * (3 * indent)
        ret = ''
        relpath = os.path.relpath(self.dest_sources, self.dest_prjs)
        if len(current['__dirs__']) > 0:
            ret += blank + '%s_Dirs := %s_Dirs &\n' % (var, var)
            ret += blank + '  (Project\'Project_Dir & "%s/' % relpath
            prefix = '   Project\'Project_Dir & "%s/' % relpath
            ret += ('",\n' + blank + prefix).join(current['__dirs__'])
            ret += '");\n'
            langs = []
            for d in current['__dirs__']:
                if 'C' not in langs and d in self.c_srcs:
                    langs.append('C')
                if 'Asm_Cpp' not in langs and d in self.asm_srcs:
                    langs.append('Asm_Cpp')
            if len(langs) > 0:
                ret += blank + '%s_Langs := %s_Langs & ("%s");\n' % (
                    var, var, '", "'.join(langs))

        if len(current) == 1:
            return ret

        for key in sorted(current.keys()):
            if key == '__dirs__':
                continue
            ret += '\n' + blank + 'case %s is\n' % key
            has_empty = False
            for val in self.scenario[key]:
                if len(current[key][val]) == 1 and \
                        len(current[key][val]['__dirs__']) == 0:
                    has_empty = True
                else:
                    ret += blank + '   when "%s" =>\n' % val
                    ret += self.dump_scenario(
                        var, current[key][val], indent + 2)
            ret += blank + '   when "undefined" =>\n'
            if has_empty:
                ret += blank + '   when others =>\n'
            ret += blank + 'end case;\n'
        return ret

    def dump_source_project_file(self):
        ret = 'abstract project Libgnat_Sources is\n'
        ret += '\n'
        ret += '   Gnat_Dirs := ();\n'
        ret += '   Gnat_Langs := ("Ada");\n'
        ret += '\n'

        for name in self.libgnat_scenarios:
            values = self.scenario[name]
            ret += '   type %s_Type is ("%s", "undefined");\n' % (
                name, '", "'.join(values))
            ret += '   %s : %s_Type := external ("%s", "undefined");\n\n' % (
                name, name, name)

        ret += self.dump_scenario('Gnat', self.gnat_rules, 1)
        ret += "end Libgnat_Sources;\n"

        fname = os.path.join(self.dest_prjs, 'libgnat_sources.gpr')
        with open(fname, 'w') as fp:
            fp.write(ret)

        if len(self.gnarl_rules) > 0:
            ret = 'abstract project Libgnarl_Sources is\n'
            ret += '\n'
            ret += '   Gnarl_Dirs := ();\n'
            ret += '   Gnarl_Langs := ("Ada");\n'
            ret += '\n'

            for name in sorted(self.libgnarl_scenarios):
                values = self.scenario[name]
                ret += '   type %s_Type is ("%s", "undefined");\n' % (
                    name, '", "'.join(values))
                ret += ('   %s : %s_Type := '
                        'external ("%s", "undefined");\n\n' % (
                            name, name, name))

            ret += '\n   -- libgnarl:\n\n'
            ret += self.dump_scenario('Gnarl', self.gnarl_rules, 1)

            ret += "end Libgnarl_Sources;\n"

            fname = os.path.join(self.dest_prjs, 'libgnarl_sources.gpr')
            with open(fname, 'w') as fp:
                fp.write(ret)

    def install(self):
        # Dump the shared rts sources project file
        self.dump_source_project_file()

        # now install the rts sources
        if not os.path.exists(self.dest_sources):
            os.makedirs(self.dest_sources)
        dirs = []
        self.__gather_used_dirs(self.gnat_rules, dirs)
        self.__gather_used_dirs(self.gnarl_rules, dirs)
        for d in dirs:
            installed = []
            self.__install_dir(d, installed)

    def __gather_used_dirs(self, src_prj, dirs):
        for name in src_prj:
            if name == '__dirs__':
                for d in src_prj[name]:
                    if d not in dirs:
                        dirs.append(d)
            else:
                for val in src_prj[name]:
                    self.__gather_used_dirs(src_prj[name][val], dirs)

    def __add_rule(self, collection, dir, rules, used_scenarii):
        # Check if the directory is always used
        if rules is None or len(rules) == 0:
            if '__dirs__' not in collection:
                collection['__dirs__'] = []
            collection['__dirs__'].append(dir)
            return

        rule = rules[0]
        if len(rules) == 1:
            rules = None
        else:
            rules = rules[1:]

        # Separate scenario variable and the cases where this rule applies
        var, cases = rule.split(':')
        assert var in self.scenario, "Unknown scenario variable: %s" % var

        if var not in used_scenarii:
            used_scenarii.append(var)
        cases = cases.split(',')

        # Make sure that the scenario variable is in the collection of rules
        if var not in collection:
            collection[var] = {}
            for case in self.scenario[var]:
                collection[var][case] = {'__dirs__': []}

        for case in cases:
            negate = False
            if case.startswith('!'):
                negate = True
                case = case[1:]
            assert case in self.scenario[var], \
                "Unexpected value for %s: %s" % (var, case)
            if negate:
                for scenario_case in self.scenario[var]:
                    if case != scenario_case:
                        if scenario_case not in collection[var]:
                            collection[var][scenario_case] = {}
                        self.__add_rule(
                            collection[var][scenario_case],
                            dir, rules, used_scenarii)
            else:
                if case not in collection[var]:
                    collection[var][case] = {}
                self.__add_rule(collection[var][case],
                                dir, rules, used_scenarii)

    def add_rule(self, dir, rules):
        if isinstance(rules, basestring):
            self.add_rule(dir, [rules])
            return

        if 'gnarl' in dir:
            collection = self.gnarl_rules
            scenarii = self.libgnarl_scenarios
        else:
            collection = self.gnat_rules
            scenarii = self.libgnat_scenarios
        self.__add_rule(collection, dir, rules, scenarii)

    def __install_dir(self, dirname, installed_files):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)

        destdir = os.path.join(self.dest_sources, dirname)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for k, v in self.dirs[dirname].items():
            self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                            installed_files=installed_files)

    def init_zfp(self):
        """Files common to all runtimes"""
        # files there no matter what

        self.add_rule('common', None)
        self.add_sources('common', [
            'libgnat/a-assert.ads', 'libgnat/a-assert.adb',
            'libgnat/a-unccon.ads',
            'libgnat/a-uncdea.ads',
            'libgnat/ada.ads',
            'hie/g-io__zfp.ads', 'hie/g-io__zfp.adb',
            'hie/g-io-put__bb.adb',
            'libgnat/g-souinf.ads',
            'libgnat/gnat.ads',
            'libgnat/i-cexten.ads',
            'libgnat/interfac.ads',
            'libgnat/machcode.ads',
            'libgnat/s-assert.ads',
            'libgnat/s-atacco.ads', 'libgnat/s-atacco.adb',
            'libgnat/s-imgboo.ads', 'libgnat/s-imgboo.adb',
            'libgnat/s-imgint.ads', 'libgnat/s-imgint.adb',
            'libgnat/s-imglli.ads', 'libgnat/s-imglli.adb',
            'libgnat/s-imgllu.ads', 'libgnat/s-imgllu.adb',
            'libgnat/s-imguns.ads', 'libgnat/s-imguns.adb',
            'libgnat/s-maccod.ads',
            'hie/s-macres.ads',
            'hie/s-secsta__zfp.ads', 'hie/s-secsta__zfp.adb',
            'libgnat/s-stoele.ads', 'libgnat/s-stoele.adb',
            'hie/s-textio.ads',
            'libgnat/s-unstyp.ads',
            'libgnat/unchconv.ads',
            'libgnat/unchdeal.ads'])

        if self._is_bb:
            self.add_sources('common', [
                'libgnat/text_io.ads',
                'hie/a-textio.ads',
                'hie/s-bb.ads'])

            self.add_rule('system_io', 'Use_Semihosting_IO:no')
            self.add_rule('semihost', 'Use_Semihosting_IO:yes')
            self.add_sources('system_io', [
                'hie/a-textio__bb.adb'])
            self.add_sources('semihost', [
                'hie/s-semiho.ads',
                'hie/s-semiho.adb',
                'hie/s-textio__semihosting.adb',
                'hie/a-textio__semihosting.adb'])

        # FPU support sources
        self.add_rule('fpu', 'Has_FPU:yes')
        self.add_sources('fpu', [
            'libgnat/s-fatflt.ads',
            'libgnat/s-fatgen.ads', 'libgnat/s-fatgen.adb',
            'libgnat/s-fatlfl.ads',
            'libgnat/s-fatllf.ads',
            'libgnat/s-fatsfl.ads'])

        # Memory support sources
        self.add_rule('mem', 'Add_Memory_Operations:yes')
        self.add_sources('mem', [
            'hie/s-memtyp.ads',
            'hie/s-memcom.ads', 'hie/s-memcom.adb',
            'hie/s-memcop.ads', 'hie/s-memcop.adb',
            'hie/s-memmov.ads', 'hie/s-memmov.adb',
            'hie/s-memset.ads', 'hie/s-memset.adb'])

        # Libc implementation
        self.add_rule('libc', 'Add_C_Support:ada_clib')
        self.add_sources('libc', [
            'hie/s-c.ads',
            'hie/s-cerrno.ads', 'hie/s-cerrno.adb',
            'hie/s-cmallo.ads', 'hie/s-cmallo.adb',
            'hie/s-cstrle.ads', 'hie/s-cstrle.adb'])

        # Newlib support
        self.add_rule('newlib', 'Add_C_Support:newlib')
        self.add_sources('newlib', [
            'hie/newlib-bb.c'])

        # Exponentiation support
        self.add_rule('exponent/int', 'Add_Exponent_Int:yes')
        self.add_rule('exponent/int_ll', 'Add_Exponent_LL_Int:yes')
        self.add_rule('exponent/mod', 'Add_Exponent_Modular:yes')
        self.add_rule('exponent/float_ll', 'Add_Exponent_LL_Float:yes')
        self.add_sources('exponent/int', [
            'libgnat/s-exnint.ads', 'libgnat/s-exnint.adb',
            'libgnat/s-expint.ads', 'libgnat/s-expint.adb',
            'libgnat/s-expuns.ads', 'libgnat/s-expuns.adb'])
        self.add_sources('exponent/int_ll', [
            'libgnat/s-exnlli.ads', 'libgnat/s-exnlli.adb',
            'libgnat/s-explli.ads', 'libgnat/s-explli.adb',
            'libgnat/s-expllu.ads', 'libgnat/s-expllu.adb'])
        self.add_sources('exponent/mod', [
            'libgnat/s-expmod.ads', 'libgnat/s-expmod.adb'])
        self.add_sources('exponent/float_ll', [
            'libgnat/s-exnllf.ads', 'libgnat/s-exnllf.adb'])

        # 64-bit arithmetic support
        self.add_rule('arith64', 'Add_Arith64:yes')
        self.add_sources('arith64', [
            'libgnat/s-arit64.ads', 'libgnat/s-arit64.adb'])

        # 'Image support
        # those extends the base 'Image support present in the zfp by default
        self.add_rule('image/enum', 'Add_Image_Enum:yes')
        self.add_rule('image/decimal', 'Add_Image_Decimal:yes')
        self.add_rule('image/decimal_ll', 'Add_Image_LL_Decimal:yes')
        self.add_rule('image/float', 'Add_Image_Float:yes')
        self.add_rule('image/int', 'Add_Image_Int:yes')
        self.add_rule('image/int_ll', 'Add_Image_LL_Int:yes')
        self.add_rule('image/based_int', 'Add_Image_Based_Int:yes')
        self.add_rule('image/based_int_ll', 'Add_Image_LL_Based_Int:yes')
        self.add_rule('image/char', 'Add_Image_Char:yes')
        self.add_rule('image/wide_char', 'Add_Image_Wide_Char:yes')

        self.add_sources('image/enum', [
            'libgnat/s-imenne.ads', 'libgnat/s-imenne.adb'])

        self.add_sources('image/decimal', [
            'libgnat/s-imgdec.adb', 'libgnat/s-imgdec.ads'])
        self.add_sources('image/decimal_ll', [
            'libgnat/s-imglld.ads', 'libgnat/s-imglld.adb'])

        self.add_sources('image/float', [
            'libgnat/s-imgrea.ads', 'libgnat/s-imgrea.adb',
            'libgnat/s-flocon.ads', 'libgnat/s-flocon__none.adb',
            'libgnat/s-powtab.ads'])

        self.add_sources('image/int', [
            'libgnat/s-imgwiu.ads', 'libgnat/s-imgwiu.adb'])
        self.add_sources('image/int_ll', [
            'libgnat/s-imgllw.ads', 'libgnat/s-imgllw.adb'])

        self.add_sources('image/based_int', [
            'libgnat/s-imgbiu.ads', 'libgnat/s-imgbiu.adb'])
        self.add_sources('image/based_int_ll', [
            'libgnat/s-imgllb.ads', 'libgnat/s-imgllb.adb'])

        self.add_sources('image/char', [
            'libgnat/s-imgcha.adb', 'libgnat/s-imgcha.ads'])
        self.add_sources('image/wide_char', [
            'libgnat/s-imgwch.ads', 'libgnat/s-imgwch.adb'])

        # Math support
        self.add_rule('math', 'Add_Math_Lib:!no')
        self.add_sources('math', [
            'libgnat/a-ncelfu.ads',
            'libgnat/a-ngcefu.ads', 'libgnat/a-ngcefu.adb',
            'libgnat/a-ngcoty.ads',
            'hie/a-ngcoty__ada.adb',
            'hie/a-ngelfu__ada.ads', 'hie/a-ngelfu__ada.adb',
            'libgnat/a-nlcefu.ads',
            'libgnat/a-nlcoty.ads',
            'hie/a-nlelfu__ada.ads',
            'libgnat/a-nllcef.ads',
            'libgnat/a-nllcty.ads',
            'libgnat/a-nllefu.ads',
            'libgnat/a-nscefu.ads',
            'libgnat/a-nscoty.ads',
            'libgnat/a-nselfu.ads',
            'libgnat/a-nucoty.ads',
            'hie/a-nuelfu__ada.ads',
            'hie/a-numaux__ada.ads',
            'libgnat/a-numeri.ads',
            'hie/s-gcmain__ada.ads', 'hie/s-gcmain__ada.adb',
            'libgnat/s-gearop.ads', 'libgnat/s-gearop.adb',
            'hie/s-libdou__ada.ads', 'hie/s-libdou__ada.adb',
            'hie/s-libm__ada.ads', 'hie/s-libm__ada.adb',
            'hie/s-libpre__ada.ads',
            'hie/s-libsin__ada.ads', 'hie/s-libsin__ada.adb',
            'hie/s-lidosq__ada.ads',
            'hie/s-lisisq__ada.ads'])

        self.add_rule('math/full',
                      ['RTS_Profile:ravenscar-full',
                       'Add_Math_Lib:!no'])
        self.add_sources('math/full', [
            'libgnat/a-ngcoar.ads', 'libgnat/a-ngcoar.adb',
            'libgnat/a-ngrear.ads', 'libgnat/a-ngrear.adb',
            'libgnat/a-nurear.ads'])

        if self._is_bb:
            self.add_rule('math/softsp', 'Add_Math_Lib:softfloat,hardfloat_dp')
            self.add_sources('math/softsp', [
                'hie/s-lisisq__ada.adb'])
            self.add_rule('math/softdp', 'Add_Math_Lib:softfloat,hardfloat_sp')
            self.add_sources('math/softdp', [
                'hie/s-lidosq__ada.adb'])

            self.add_rule('math/hardsp',
                          'Add_Math_Lib:hardfloat,hardfloat_sp')
            self.add_sources('math/hardsp', [
                'hie/s-lisisq__fpu.adb'])
            self.add_rule('math/harddp', 'Add_Math_Lib:hardfloat,hardfloat_dp')
            self.add_sources('math/harddp', [
                'hie/s-lidosq__fpu.adb'])
        else:
            # PikeOS
            self.add_sources('math', [
                'hie/s-lisisq__fpu.adb',
                'hie/s-lidosq__fpu.adb'])

        # ZFP & SFP-specific libgnat files
        self.add_rule('zfp', 'RTS_Profile:zfp,ravenscar-sfp')
        self.add_sources('zfp', [
            'hie/a-elchha__zfp.ads',
            'hie/a-elchha__zfp.adb',
            'hie/s-sssita.ads', 'hie/s-sssita.adb',
            'hie/a-except__zfp.ads', 'hie/a-except__zfp.adb',
            'hie/a-tags__hie.ads', 'hie/a-tags__hie.adb',
            'hie/i-c__hie.ads', 'hie/s-assert__xi.adb'])
        if self._is_bb:
            self.add_sources('zfp', [
                'hie/s-memory__zfp.ads',
                'hie/s-memory__zfp.adb'])
        else:
            self.add_rule('zfp-io', 'RTS_Profile:zfp')
            self.add_sources('zfp-io', [
                'libgnat/text_io.ads',
                'hie/a-textio.ads',
                'hie/a-textio__bb.adb'])
            self.add_sources('zfp', [
                'hie/s-memory__zfp.ads',
                'hie/s-memory__raven-min.adb'])

        # ZFP only libgnat files
        self.add_rule('zfp-parame', 'RTS_Profile:zfp')
        self.add_sources('zfp-parame', [
            'hie/s-parame__zfp.ads',
            'hie/s-parame__zfp.adb'])

    def init_sfp(self):
        """ravenscar-sfp files"""
        # libgnarl sources common to sfp/full
        self.add_rule('gnarl/common', None)
        self.add_sources('gnarl/common', [
            'libgnarl/a-interr.ads',
            'hie/a-interr__raven.adb',
            'hie/a-reatim.ads', 'hie/a-reatim.adb',
            'libgnarl/a-retide.ads',
            'hie/a-retide__raven.adb',
            'hie/a-sytaco__xi.ads', 'hie/a-sytaco__xi.adb',
            'libgnarl/a-taside.ads',
            'hie/a-taside__raven.adb',
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
            'hie/s-taspri.ads',
            'libgnarl/s-tasres.ads',
            'libgnarl/s-tpobmu.ads'])
        if self._is_bb:
            # BB case
            self.add_sources('gnarl/common', [
                'hie/a-exetim.ads', 'hie/a-exetim.adb',
                'hie/a-extiin.ads', 'hie/a-extiin.adb',
                'hie/a-rttiev.ads', 'hie/a-rttiev.adb',
                'hie/s-bbexti.ads', 'hie/s-bbexti.adb',
                'hie/s-bbinte.ads',
                'hie/s-bbprot.ads', 'hie/s-bbprot.adb',
                'hie/s-bbthqu.ads', 'hie/s-bbthqu.adb',
                'hie/s-bbthre.ads', 'hie/s-bbthre.adb',
                'hie/s-bbtiev.ads', 'hie/s-bbtiev.adb',
                'hie/s-bbtime.ads',
                'hie/s-bcprmu.ads', 'hie/s-bcprmu.adb',
                'hie/s-interr.adb',
                'hie/s-multip.ads', 'hie/s-multip.adb',
                'hie/s-taprop__bb.adb',
                'hie/s-tpobmu.adb',
                'hie/s-osinte.ads'])
        else:
            # PikeOS case
            self.add_sources('gnarl/common', [
                'libgnat/text_io.ads',
                'hie/a-textio.ads',
                'hie/a-textio__pikeos.adb',
                'hie/s-multip__raven-default.ads',
                'hie/s-multip__raven-default.adb',
                'hie/s-taprop__pikeos.adb',
                'libgnarl/s-tpobmu.adb'])
            self.add_rule('gnarl/pikeos3', 'Pikeos_Version:pikeos3')
            self.add_sources('gnarl/pikeos3', [
                'hie/s-interr__pikeos3.adb',
                'hie/s-osinte__pikeos3.ads',
                'hie/s-osinte__pikeos3.adb'])
            self.add_rule('gnarl/pikeos4', 'Pikeos_Version:pikeos4')
            self.add_sources('gnarl/pikeos4', [
                'hie/s-interr__pikeos4.adb',
                'hie/s-osinte__pikeos4.ads',
                'hie/s-osinte__pikeos4.adb'])

        # SFP-specific files
        self.add_rule('gnarl/sfp', 'RTS_Profile:ravenscar-sfp')
        self.add_sources('gnarl/sfp', [
            'hie/s-taskin__raven.ads',
            'hie/s-tposen__raven.adb',
            'hie/s-tposen__raven.ads'])

        # timer support
        self.add_rule('gnarl/timer32', 'Timer:timer32')
        self.add_sources('gnarl/timer32', [
            'hie/s-bbbosu__timer-32.ads',
            'hie/s-bbtime__timer-32.adb'])
        self.add_rule('gnarl/timer64', 'Timer:timer64')
        self.add_sources('gnarl/timer64', [
            'hie/s-bbbosu__timer-64.ads',
            'hie/s-bbtime__timer-64.adb'])

        # spinlock support (leon workaround)
        if self._is_bb:
            self.add_rule('gnarl/spinlock-leon', 'CPU_Family:leon')
            self.add_rule('gnarl/spinlock-gcc', 'CPU_Family:!leon')
            self.add_sources('gnarl/spinlock-gcc', 'hie/s-musplo.adb')
            self.add_sources('gnarl/spinlock-leon', 'hie/s-musplo__leon.adb')
        else:
            self.add_sources('gnarl/common', 'hie/s-musplo.adb')

        # memory profile
        if self._is_bb:
            self.add_rule('gnat/mem-small', 'Memory_Profile:small')
            self.add_sources('gnat/mem-small', [
                'hie/s-parame__small.ads'])
            self.add_sources('gnat/mem-large', [
                'hie/s-parame.adb'])
            self.add_rule('gnat/mem-large', 'Memory_Profile:large')
            self.add_sources('gnat/mem-large', [
                'hie/s-parame__large.ads'])
            self.add_sources('gnat/mem-small', [
                'hie/s-parame.adb'])
        else:
            # PikeOS:
            self.add_rule('gnat/common', None)
            self.add_sources('gnat/common', [
                'hie/s-parame__large.ads'])
            self.add_sources('gnat/common', [
                'hie/s-parame.adb'])

    def init_full(self):
        """ravenscar-full files"""
        # libgnat files for the full profile
        self.add_rule('full', 'RTS_Profile:ravenscar-full')
        self.add_sources('full', [
            'libgnat/a-chacon.ads', 'libgnat/a-chacon.adb',
            'libgnat/a-chahan.ads', 'libgnat/a-chahan.adb',
            'libgnat/a-charac.ads',
            'libgnat/a-chlat1.ads',
            'libgnat/a-chlat9.ads',
            'libgnat/a-cwila1.ads',
            'libgnat/a-cwila9.ads',
            'libgnat/a-chzla1.ads',
            'libgnat/a-chzla9.ads',
            'libgnat/a-decima.ads', 'libgnat/a-decima.adb',
            'libgnat/a-einuoc.ads', 'libgnat/a-einuoc.adb',
            'libgnat/a-elchha.ads', 'hie/a-elchha__full.adb',
            'hie/a-excach__cert.adb',
            'libgnat/a-except.ads', 'libgnat/a-except.adb',
            'libgnat/a-excpol.adb',
            'libgnat/a-exctra.ads', 'libgnat/a-exctra.adb',
            'libgnat/a-exexda.adb',
            'libgnat/a-exexpr.adb',
            'libgnat/a-exextr.adb',
            'libgnat/a-exstat.adb',
            'libgnat/a-finali.ads', 'libgnat/a-finali.adb',
            'libgnat/a-ioexce.ads',
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
            'libgnat/a-storio.ads', 'libgnat/a-storio.adb',
            'libgnat/a-strbou.ads', 'libgnat/a-strbou.adb',
            'libgnat/a-stream.ads', 'libgnat/a-stream.adb',
            'libgnat/a-strfix.ads', 'libgnat/a-strfix.adb',
            'libgnat/a-string.ads',
            'libgnat/a-strmap.ads', 'libgnat/a-strmap.adb',
            'libgnat/a-strsea.ads', 'libgnat/a-strsea.adb',
            'libgnat/a-strsup.ads', 'libgnat/a-strsup.adb',
            'libgnat/a-strunb.ads', 'libgnat/a-strunb.adb',
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
            'libgnat/s-casuti.ads', 'libgnat/s-casuti.adb',
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
            'libgnat/s-fore.ads', 'libgnat/s-fore.adb',
            'libgnat/s-geveop.ads', 'libgnat/s-geveop.adb',
            'libgnat/s-htable.ads', 'libgnat/s-htable.adb',
            'hie/s-init.ads',
            'hie/s-init__bb.adb',
            'libgnat/s-io.ads', 'hie/s-io.adb',
            'libgnat/s-mantis.ads', 'libgnat/s-mantis.adb',
            'libgnat/s-mastop.ads', 'libgnat/s-mastop.adb',
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
            'libgnat/s-pack63.ads', 'libgnat/s-pack63.adb',
            'libgnat/s-pooglo.ads', 'libgnat/s-pooglo.adb',
            'libgnat/s-pooloc.ads', 'libgnat/s-pooloc.adb',
            'libgnat/s-poosiz.ads', 'libgnat/s-poosiz.adb',
            'libgnat/s-rannum.ads', 'libgnat/s-rannum.adb',
            'libgnat/s-ransee.ads', 'hie/s-ransee.adb',
            'libgnat/s-regexp.ads', 'libgnat/s-regexp.adb',
            'libgnat/s-restri.ads', 'libgnat/s-restri.adb',
            'libgnat/s-rident.ads',
            'libgnat/s-scaval.ads', 'libgnat/s-scaval.adb',
            'hie/s-soflin.ads', 'hie/s-soflin.adb',
            'libgnat/s-sopco3.ads', 'libgnat/s-sopco3.adb',
            'libgnat/s-sopco4.ads', 'libgnat/s-sopco4.adb',
            'libgnat/s-sopco5.ads', 'libgnat/s-sopco5.adb',
            'libgnat/s-spsufi.ads', 'libgnat/s-spsufi.adb',
            'libgnat/s-stalib.ads', 'libgnat/s-stalib.adb',
            'libgnat/s-stopoo.ads', 'libgnat/s-stopoo.adb',
            'libgnat/s-stposu.ads', 'libgnat/s-stposu.adb',
            'libgnat/s-stratt.ads', 'libgnat/s-stratt.adb',
            'libgnat/s-strhas.ads', 'libgnat/s-strhas.adb',
            'libgnat/s-string.ads', 'libgnat/s-string.adb',
            'libgnat/s-tasloc.ads', 'libgnat/s-tasloc.adb',
            'hie/s-traceb__cert.ads',
            'libgnat/s-traent.ads', 'libgnat/s-traent.adb',
            'libgnat/s-trasym.ads', 'libgnat/s-trasym.adb',
            'libgnat/s-utf_32.ads', 'libgnat/s-utf_32.adb',
            'libgnat/s-valboo.ads', 'libgnat/s-valboo.adb',
            'libgnat/s-valcha.ads', 'libgnat/s-valcha.adb',
            'libgnat/s-valdec.ads', 'libgnat/s-valdec.adb',
            'libgnat/s-valenu.ads', 'libgnat/s-valenu.adb',
            'libgnat/s-valint.ads', 'libgnat/s-valint.adb',
            'libgnat/s-vallld.ads', 'libgnat/s-vallld.adb',
            'libgnat/s-vallli.ads', 'libgnat/s-vallli.adb',
            'libgnat/s-valllu.ads', 'libgnat/s-valllu.adb',
            'libgnat/s-valrea.ads', 'libgnat/s-valrea.adb',
            'libgnat/s-valuns.ads', 'libgnat/s-valuns.adb',
            'libgnat/s-valuti.ads', 'libgnat/s-valuti.adb',
            'libgnat/s-valwch.ads', 'libgnat/s-valwch.adb',
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
            'libgnat/s-widlli.ads', 'libgnat/s-widlli.adb',
            'libgnat/s-widllu.ads', 'libgnat/s-widllu.adb',
            'libgnat/s-widwch.ads', 'libgnat/s-widwch.adb',
            'libgnat/s-wwdcha.ads', 'libgnat/s-wwdcha.adb',
            'libgnat/s-wwdenu.ads', 'libgnat/s-wwdenu.adb',
            'libgnat/s-wwdwch.ads', 'libgnat/s-wwdwch.adb',
            'hie/tconfig.h',
            'hie/tsystem.h',
            'libgcc/unwind-pe.h'])

        if self._is_bb:
            self.add_sources('full', 'hie/adaint-xi.c')
            self.add_sources('full', [
                'libgnat/s-memory.ads',
                'hie/s-memory__xi.adb'])
        else:
            # PikeOS
            self.add_sources('full', [
                'hie/s-memory__pikeos.ads',
                'hie/s-memory__pikeos.adb'])
            self.update_pairs('full', {
                's-init.adb': 'hie/s-init__pikeos.adb'})

        # Zero-cost-exception support
        self.add_rule('full/zcx-arm', [
            'RTS_Profile:ravenscar-full', 'CPU_Family:arm'])
        self.add_sources('full/zcx-arm', [
            'libgnat/s-excmac__arm.adb',
            'libgnat/s-excmac__arm.ads',
            'hie/s-traceb__armeabi.adb'])
        self.add_rule('full/zcx-dw2', [
            'RTS_Profile:ravenscar-full', 'CPU_Family:!arm'])
        self.add_sources('full/zcx-dw2', [
            'libgnat/s-excmac__gcc.ads',
            'libgnat/s-excmac__gcc.adb',
            'libgcc/unwind-dw2-fde.h'])
        if self._is_bb:
            self.add_sources('full/zcx-dw2', 'hie/unwind-dw2-fde-bb.c')

        self.add_rule('full/zcx-aarch64',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:aarch64'])
        self.add_sources('full/zcx-aarch64', [
            'hie/s-traceb__dwarf.adb'])
        self.add_rule('full/zcx-ppc',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:powerpc'])
        self.add_sources('full/zcx-ppc', [
            'hie/s-traceb__ppc.adb'])
        self.add_rule('full/zcx-leon',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:leon'])
        self.add_sources('full/zcx-leon', [
            'hie/s-traceb__sparc.adb'])
        self.add_rule('full/zcx-x86',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:x86'])
        self.add_sources('full/zcx-x86', [
            'hie/s-traceb__vx653-sim.adb'])

        # Containers
        self.add_rule('containers', 'RTS_Profile:ravenscar-full')
        self.add_sources('containers', [
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
            'libgnat/a-coinho.adb', 'libgnat/a-coinho.ads',
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
            'libgnat/s-atocou.adb', 'libgnat/s-atocou.ads'])
        self.update_pairs('containers', {
            'a-coinho.adb': 'libgnat/a-coinho__shared.adb',
            'a-coinho.ads': 'libgnat/a-coinho__shared.ads',
            's-atocou.adb': 'libgnat/s-atocou__builtin.adb'})

        # GNARL files for the full runtime
        self.add_rule('gnarl/full', 'RTS_Profile:ravenscar-full')
        self.add_sources('gnarl/full', [
            'hie/s-taskin__full.ads',
            'hie/s-tposen__xi-full.adb', 'hie/s-tposen__xi-full.ads'])

        if self._is_bb:
            self.add_sources('gnarl/full', [
                'hie/s-btstch.ads', 'hie/s-btstch.adb'])

        # Ravenscar extended: relative delays
        self.add_rule('gnarl/full/extended', 'RTS_Profile:ravenscar-full')
        self.add_sources('gnarl/full/extended', [
            'hie/s-reldel.ads',
            'hie/s-reldel.adb'])

        # Tasking extensions: multiple entries
        self.add_sources('gnarl/full/extended', [
            'libgnarl/a-synbar.adb', 'libgnarl/a-synbar.ads',
            'libgnarl/g-boubuf.adb', 'libgnarl/g-boubuf.ads',
            'libgnarl/g-boumai.ads',
            'libgnarl/g-semaph.adb', 'libgnarl/g-semaph.ads',
            'hie/s-tpoben.ads', 'hie/s-tpoben.adb',
            'hie/s-tasque.ads', 'hie/s-tasque.adb',
            'hie/s-tpobop.ads', 'hie/s-tpobop.adb'])
