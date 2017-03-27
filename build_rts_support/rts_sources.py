#! /usr/bin/env python
#
# Copyright (C) 2016-2017, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from build_rts_support.files_holder import FilesHolder

import os


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

        if self.config.use_semihosting_io:
            ret['Use_Semihosting_IO'] = 'yes'
        else:
            ret['Use_Semihosting_IO'] = 'no'

        return ret

    def sfp_scenarios(self, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base SFP runtime"""
        ret = self.zfp_scenarios(mem_routines, math_lib)
        ret['RTS_Profile'] = 'ravenscar-sfp'

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

        if not self.config.is_pikeos:
            # PikeOS provides its own C library
            # ravenscar-full requires C memory operations, either via newlib
            # or via our own implementation in Ada
            if self.config.has_newlib:
                ret['Add_C_Support'] = "newlib"
            else:
                ret['Add_C_Support'] = "ada_clib"

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
            'a-assert.ads', 'a-assert.adb',
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
            'unchconv.ads',
            'unchdeal.ads'])

        if self._is_bb:
            self.add_sources('common', [
                'text_io.ads',
                {'a-textio.ads': 'a-textio-zfp.ads'},
                's-bb.ads'])

            self.add_rule('system_io', 'Use_Semihosting_IO:no')
            self.add_rule('semihost', 'Use_Semihosting_IO:yes')
            self.add_sources('system_io', {
                'a-textio.adb': 'a-textio-zfp.adb'})
            self.add_sources('semihost', [
                's-semiho.ads',
                's-semiho.adb',
                {'s-textio.adb': 's-textio-semihosting.adb',
                 'a-textio.adb': 'a-textio-semihosting.adb'}])

        # FPU support sources
        self.add_rule('fpu', 'Has_FPU:yes')
        self.add_sources('fpu', [
            's-fatflt.ads',
            's-fatgen.ads', 's-fatgen.adb',
            's-fatlfl.ads',
            's-fatllf.ads',
            's-fatsfl.ads'])

        # Memory support sources
        self.add_rule('mem', 'Add_Memory_Operations:yes')
        self.add_sources('mem', [
            's-memcom.ads', 's-memcom.adb',
            {'s-memcop.ads': 's-memcop-zfp.ads',
             's-memcop.adb': 's-memcop-zfp.adb'},
            's-memmov.ads', 's-memmov.adb',
            's-memset.ads', 's-memset.adb'])

        # Libc implementation
        self.add_rule('libc', 'Add_C_Support:ada_clib')
        self.add_sources('libc', {
            's-c.ads': 's-c-zfp.ads',
            's-cerrno.ads': 's-cerrno-zfp.ads',
            's-cerrno.adb': 's-cerrno-zfp.adb',
            's-cmallo.ads': 's-cmallo-zfp.ads',
            's-cmallo.adb': 's-cmallo-zfp.adb',
            's-cstrle.ads': 's-cstrle-zfp.ads',
            's-cstrle.adb': 's-cstrle-zfp.adb'})

        # Newlib support
        self.add_rule('newlib', 'Add_C_Support:newlib')
        self.add_sources('newlib', [
            'newlib-bb.c'])

        # Math support
        self.add_rule('math', 'Add_Math_Lib:!no')
        self.add_sources('math', [
            'a-ncelfu.ads',
            'a-ngcefu.ads', 'a-ngcefu.adb',
            'a-ngcoty.ads', {'a-ngcoty.adb': 'a-ngcoty-ada.adb'},
            {'a-ngelfu.ads': 'a-ngelfu-ada.ads',
             'a-ngelfu.adb': 'a-ngelfu-ada.adb'},
            'a-nlcefu.ads',
            'a-nlcoty.ads',
            {'a-nlelfu.ads': 'a-nlelfu-ada.ads'},
            'a-nllcef.ads',
            'a-nllcty.ads',
            'a-nllefu.ads',
            'a-nscefu.ads',
            'a-nscoty.ads',
            'a-nselfu.ads',
            'a-nucoty.ads',
            {'a-nuelfu.ads': 'a-nuelfu-ada.ads'},
            {'a-numaux.ads': 'a-numaux-ada.ads'},
            'a-numeri.ads',
            's-exnllf.ads', 's-exnllf.adb',
            {'s-gcmain.ads': 's-gcmain-ada.ads',
             's-gcmain.adb': 's-gcmain-ada.adb'},
            's-gearop.ads', 's-gearop.adb',
            {'s-libdou.ads': 's-libdou-ada.ads',
             's-libdou.adb': 's-libdou-ada.adb'},
            {'s-libm.ads': 's-libm-ada.ads', 's-libm.adb': 's-libm-ada.adb'},
            {'s-libpre.ads': 's-libpre-ada.ads'},
            {'s-libsin.ads': 's-libsin-ada.ads',
             's-libsin.adb': 's-libsin-ada.adb'},
            {'s-lidosq.ads': 's-lidosq-ada.ads'},
            {'s-lisisq.ads': 's-lisisq-ada.ads'}])

        self.add_rule('math/full',
                      ['RTS_Profile:ravenscar-full',
                       'Add_Math_Lib:!no'])
        self.add_sources('math/full', [
            'a-ngcoar.ads', 'a-ngcoar.adb',
            'a-ngrear.ads', 'a-ngrear.adb',
            'a-nurear.ads'])

        if self._is_bb:
            self.add_rule('math/softsp', 'Add_Math_Lib:softfloat,hardfloat_dp')
            self.add_sources('math/softsp', {
                's-lisisq.adb': 's-lisisq-ada.adb'})
            self.add_rule('math/softdp', 'Add_Math_Lib:softfloat,hardfloat_sp')
            self.add_sources('math/softdp', {
                's-lidosq.adb': 's-lidosq-ada.adb'})

            self.add_rule('math/hardsp',
                          'Add_Math_Lib:hardfloat,hardfloat_sp')
            self.add_sources('math/hardsp', {
                's-lisisq.adb': 's-lisisq-fpu.adb'})
            self.add_rule('math/harddp', 'Add_Math_Lib:hardfloat,hardfloat_dp')
            self.add_sources('math/harddp', {
                's-lidosq.adb': 's-lidosq-fpu.adb'})
        else:
            # PikeOS
            self.add_sources('math', {
                's-lisisq.adb': 's-lisisq-fpu.adb',
                's-lidosq.adb': 's-lidosq-fpu.adb'})

        # Finally, the ZFP & SFP-specific libgnat files
        self.add_rule('zfp', 'RTS_Profile:zfp,ravenscar-sfp')
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
            self.add_rule('zfp-io', 'RTS_Profile:zfp')
            self.add_sources('zfp-io', [
                'text_io.ads',
                {'a-textio.ads': 'a-textio-zfp.ads',
                 'a-textio.adb': 'a-textio-zfp.adb'}])
            self.add_sources('zfp', {
                's-memory.adb': 's-memory-raven-min.adb'})

    def init_sfp(self):
        """ravenscar-sfp files"""
        # libgnarl sources common to sfp/full
        self.add_rule('gnarl/common', None)
        self.add_sources('gnarl/common', [
            'a-interr.ads', {'a-interr.adb': 'a-interr-raven.adb'},
            {'a-reatim.ads': 'a-reatim-xi.ads',
             'a-reatim.adb': 'a-reatim-xi.adb'},
            'a-retide.ads', {'a-retide.adb': 'a-retide-raven.adb'},
            {'a-sytaco.ads': 'a-sytaco-xi.ads',
             'a-sytaco.adb': 'a-sytaco-xi.adb'},
            'a-taside.ads', {'a-taside.adb': 'a-taside-raven.adb'},
            {'a-taster.ads': 'a-taster-raven.ads',
             'a-taster.adb': 'a-taster-raven.adb'},
            {'s-interr.ads': 's-interr-raven.ads'},
            's-mufalo.ads', 's-mufalo.adb',
            's-musplo.ads',
            {'s-parame.adb': 's-parame-xi.adb'},
            {'s-taprob.ads': 's-taprob-raven.ads',
             's-taprob.adb': 's-taprob-raven.adb'},
            {'s-taprop.ads': 's-taprop-xi.ads'}, 's-taprop.adb',
            's-tarest.ads', {'s-tarest.adb': 's-tarest-raven.adb'},
            {'s-tasdeb.ads': 's-tasdeb-xi.ads',
             's-tasdeb.adb': 's-tasdeb-raven.adb'},
            's-tasinf.ads', 's-tasinf.adb',
            {'s-taskin.adb': 's-taskin-raven.adb'},
            {'s-taspri.ads': 's-taspri-xi.ads'},
            's-tasres.ads',
            's-tpobmu.ads'])
        if self._is_bb:
            # BB case
            self.add_sources('gnarl/common', [
                {'a-exetim.ads': 'a-exetim-bb.ads',
                 'a-exetim.adb': 'a-exetim-bb.adb'},
                {'a-extiin.ads': 'a-extiin-bb.ads',
                 'a-extiin.adb': 'a-extiin-bb.adb'},
                {'a-rttiev.ads': 'a-rttiev-bb.ads',
                 'a-rttiev.adb': 'a-rttiev-bb.adb'},
                's-bbbosu.ads',
                's-bbexti.ads', 's-bbexti.adb',
                's-bbinte.ads',
                's-bbprot.ads', 's-bbprot.adb',
                's-bbthqu.ads', 's-bbthqu.adb',
                's-bbthre.ads', 's-bbthre.adb',
                's-bbtiev.ads', 's-bbtiev.adb',
                's-bbtime.ads',
                's-bcprmu.ads', 's-bcprmu.adb',
                {'s-interr.adb': 's-interr-xi.adb'},
                {'s-multip.ads': 's-multip-bb.ads',
                 's-multip.adb': 's-multip-bb.adb'},
                {'s-taprop.adb': 's-taprop-xi.adb'},
                {'s-tpobmu.adb': 's-tpobmu-bb.adb'},
                {'s-osinte.ads': 's-osinte-bb.ads'}])
        else:
            # PikeOS case
            self.add_sources('gnarl/common', [
                'text_io.ads',
                {'a-textio.ads': 'a-textio-zfp.ads',
                 'a-textio.adb': 'a-textio-raven.adb',
                 's-multip.ads': 's-multip-raven-default.ads',
                 's-multip.adb': 's-multip-raven-default.adb',
                 's-taprop.adb': 's-taprop-pikeos.adb'},
                's-tpobmu.adb'])
            self.add_rule('gnarl/pikeos3', 'Pikeos_Version:pikeos3')
            self.add_sources('gnarl/pikeos3', {
                's-interr.adb': 's-interr-pikeos.adb',
                's-osinte.ads': 's-osinte-pikeos.ads',
                's-osinte.adb': 's-osinte-pikeos.adb'})
            self.add_rule('gnarl/pikeos4', 'Pikeos_Version:pikeos4')
            self.add_sources('gnarl/pikeos4', {
                's-interr.adb': 's-interr-pikeos4.adb',
                's-osinte.ads': 's-osinte-pikeos4.ads',
                's-osinte.adb': 's-osinte-pikeos4.adb'})

        # SFP-specific files
        self.add_rule('gnarl/sfp', 'RTS_Profile:ravenscar-sfp')
        self.add_sources('gnarl/sfp', {
            's-taskin.ads': 's-taskin-raven.ads',
            's-tposen.adb': 's-tposen-raven.adb',
            's-tposen.ads': 's-tposen-raven.ads'})

        # timer support
        self.add_rule('gnarl/timer32', 'Timer:timer32')
        self.add_sources('gnarl/timer32', 's-bbtime.adb')
        self.add_rule('gnarl/timer64', 'Timer:timer64')
        self.add_sources('gnarl/timer64', {'s-bbtime.adb': 's-bbtime-ppc.adb'})

        # spinlock support (leon workaround)
        if self._is_bb:
            self.add_rule('gnarl/spinlock-leon', 'CPU_Family:leon')
            self.add_rule('gnarl/spinlock-gcc', 'CPU_Family:!leon')
            self.add_sources('gnarl/spinlock-gcc', 's-musplo.adb')
            self.add_sources('gnarl/spinlock-leon',
                             {'s-musplo.adb': 's-musplo-leon.adb'})
        else:
            self.add_sources('gnarl/common', 's-musplo.adb')

        # memory profile
        if self._is_bb:
            self.add_rule('gnarl/mem-small', 'Memory_Profile:small')
            self.add_rule('gnarl/mem-large', 'Memory_Profile:large')
            self.add_sources('gnarl/mem-small',
                             {'s-parame.ads': 's-parame-xi-small.ads'})
            self.add_sources('gnarl/mem-large',
                             {'s-parame.ads': 's-parame-xi.ads'})
        else:
            # PikeOS:
            self.add_sources('gnarl/common',
                             {'s-parame.ads': 's-parame-xi.ads'})

    def init_full(self):
        """ravenscar-full files"""

        # libgnat files for the full profile
        self.add_rule('full', 'RTS_Profile:ravenscar-full')
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
            'g-dyntab.ads', 'g-dyntab.adb',
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
        self.add_rule('full/zcx-arm', [
            'RTS_Profile:ravenscar-full', 'CPU_Family:arm'])
        self.add_sources('full/zcx-arm', {
            's-excmac.adb': 's-excmac-arm.adb',
            's-excmac.ads': 's-excmac-arm.ads',
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})
        self.add_rule('full/zcx-dw2', [
            'RTS_Profile:ravenscar-full', 'CPU_Family:!arm'])
        self.add_sources('full/zcx-dw2', [
            {'s-excmac.ads': 's-excmac-gcc.ads',
             's-excmac.adb': 's-excmac-gcc.adb'},
            'libgcc/unwind-dw2-fde.h'])
        if self._is_bb:
            self.add_sources('full/zcx-dw2', 'src/unwind-dw2-fde-bb.c')

        self.add_rule('full/zcx-aarch64',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:aarch64'])
        self.add_sources('full/zcx-aarch64', {
            's-traceb.adb': 's-traceb-xi-dwarf.adb'})
        self.add_rule('full/zcx-ppc',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:powerpc'])
        self.add_sources('full/zcx-ppc', {
            's-traceb.adb': 's-traceb-xi-ppc.adb'})
        self.add_rule('full/zcx-leon',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:leon'])
        self.add_sources('full/zcx-leon', {
            's-traceb.adb': 's-traceb-xi-sparc.adb'})
        self.add_rule('full/zcx-x86',
                      ['RTS_Profile:ravenscar-full', 'CPU_Family:x86'])
        self.add_sources('full/zcx-x86', {
            's-traceb.adb': 's-traceb-vx653-sim.adb'})

        # Containers
        self.add_rule('containers', 'RTS_Profile:ravenscar-full')
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
            'a-cofuba.adb', 'a-cofuba.ads',
            'a-cofuma.adb', 'a-cofuma.ads',
            'a-cofuse.adb', 'a-cofuse.ads',
            'a-cofuve.adb', 'a-cofuve.ads',
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
        self.add_rule('gnarl/full', 'RTS_Profile:ravenscar-full')
        self.add_sources('gnarl/full', {
            's-taskin.ads': 's-taskin-xi-full.ads',
            's-tposen.adb': 's-tposen-xi-full.adb',
            's-tposen.ads': 's-tposen-xi-full.ads'})

        if self._is_bb:
            self.add_sources('gnarl/full', [
                's-btstch.ads', 's-btstch.adb'])

        # Ravenscar extended: relative delays
        self.add_rule('gnarl/full/extended', 'RTS_Profile:ravenscar-full')
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
