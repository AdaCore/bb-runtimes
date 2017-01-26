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

# PikeOS
from pikeos import ArmPikeOS, PpcPikeOS, X86PikeOS

# Cortex-M runtimes
from arm.stm32 import Stm32
from arm.sam import Sam
from arm.smartfusion2 import SmartFusion2
from arm.lm3s import LM3S

# Cortex-A/R runtimes
from arm.tms570 import TMS570
from arm.rpi2 import Rpi2
from arm.zynq import Zynq7000

# Aarch64
from aarch64.rpi3 import Rpi3
from aarch64.qemu import AARCH64QEMU

# leon
from sparc.leon import Leon2
from sparc.leon3 import Leon3
from sparc.leon4 import Leon4

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
import copy


# Definitions of shared source files.
# Keep spec and body on one line.

class SourceDirs(SharedFilesHolder):
    def __init__(self, is_bb):
        super(SourceDirs, self).__init__()
        self._is_bb = is_bb
        self.scenario = {
            'RTS': ['zfp', 'ravenscar-sfp', 'ravenscar-full'],
            'CPU_Family': ['arm', 'leon', 'powerpc', 'x86'],
            'Has_FPU': ['true', 'false'],
            'Memory_Profile': ['small', 'large'],
            'Timer': ['timer32', 'timer64'],
            'Pikeos_Version': ['pikeos3', 'pikeos4'],
            'Add_Math_Lib': [
                'no', 'softfloat', 'hardfloat_sp', 'hardfloat_dp'],
            'Add_Memory_Operations': ['no', 'yes'],
            'Add_C_Support': ['no', 'ada_clib', 'newlib']}
        self.libgnat_scenarios = [
            'RTS',
            'CPU_Family',
            'Has_FPU',
            'Add_Math_Lib',
            'Add_Memory_Operations',
            'Add_C_Support']
        self.gnat_rules = {}
        self.gnarl_rules = {}

        self._init_zfp()
        self._init_sfp()
        self._init_full()
        self.rts = []
        self.targets = []
        # the targets objects

    def dump_scenario(self, var, current, indent):
        blank = ' ' * (3 * indent)
        ret = ''
        if len(current['__dirs__']) > 0:
            ret += blank + '%s_Dirs := %s_Dirs &\n' % (var, var)
            ret += blank + \
                '  (Project\'Project_Dir & "%s/' % Config.shared_sources
            prefix = '   Project\'Project_Dir & "%s/' % Config.shared_sources
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
            if has_empty:
                ret += blank + '   when others =>\n'
            ret += blank + 'end case;\n'
        return ret

    def dump_source_project_file(self, dest):
        ret = 'abstract project Libgnat_Sources is\n'
        ret += '\n'
        ret += '   Gnat_Dirs := ();\n'
        ret += '   Gnat_Langs := ("Ada");\n'
        ret += '\n'

        for name in self.libgnat_scenarios:
            if name in self.scenario:
                values = self.scenario[name]
                ret += '   type %s_Type is ("%s");\n' % (
                    name, '", "'.join(values))
                ret += '   %s : %s_Type := external ("%s");\n\n' % (
                    name, name, name)

        ret += self.dump_scenario('Gnat', self.gnat_rules, 1)
        ret += "end Libgnat_Sources;\n"

        with open(os.path.join(dest, 'libgnat_sources.gpr'), 'w') as fp:
            fp.write(ret)

        ret = 'abstract project Libgnarl_Sources is\n'
        ret += '\n'
        ret += '   Gnarl_Dirs := ();\n'
        ret += '   Gnarl_Langs := ("Ada");\n'
        ret += '\n'

        for name in sorted(self.scenario):
            values = self.scenario[name]
            ret += '   type %s_Type is ("%s");\n' % (
                name, '", "'.join(values))
            ret += '   %s : %s_Type := external ("%s");\n\n' % (
                name, name, name)

        ret += '\n   -- libgnarl:\n\n'
        ret += self.dump_scenario('Gnarl', self.gnarl_rules, 1)

        ret += "end Libgnarl_Sources;\n"

        with open(os.path.join(dest, 'libgnarl_sources.gpr'), 'w') as fp:
            fp.write(ret)

    def install(self, dest):
        # Do some postprocessing on the scenario variables to simplify them
        # and remove the unused ones
        for name in self.scenario.keys():
            for val in self.scenario[name]:
                used = False
                for rts in self.rts:
                    if name in rts and rts[name] == val:
                        used = True
                if not used:
                    self.scenario[name].remove(val)
        self.__postprocess_project(
            self.gnat_rules, copy.deepcopy(self.rts))
        self.__postprocess_project(
            self.gnarl_rules, copy.deepcopy(self.rts))
        scenario = []
        self.__gather_used_scenario(self.gnat_rules, scenario)
        self.__gather_used_scenario(self.gnarl_rules, scenario)
        for name in self.scenario.keys():
            if name not in scenario:
                del self.scenario[name]

        # for rts in self.rts:
        #     for val in rts.keys():
        #         if val not in scenario:
        #             del rts[val]

        # Dump the shared rts sources project file
        self.dump_source_project_file(dest)

        # now install the rts sources
        src_dir = os.path.join(dest, Config.shared_sources)
        if not os.path.exists(src_dir):
            os.makedirs(src_dir)
        dirs = []
        self.__gather_used_dirs(self.gnat_rules, dirs)
        self.__gather_used_dirs(self.gnarl_rules, dirs)
        for d in dirs:
            installed = []
            self.__install_dir(d, src_dir, installed)

    def __gather_used_dirs(self, src_prj, dirs):
        for name in src_prj:
            if name == '__dirs__':
                for d in src_prj[name]:
                    if d not in dirs:
                        dirs.append(d)
            else:
                for val in src_prj[name]:
                    self.__gather_used_dirs(src_prj[name][val], dirs)

    def __gather_used_scenario(self, src_prj, scenario):
        for rule in src_prj:
            if rule == '__dirs__':
                continue
            if rule not in scenario:
                scenario.append(rule)
            for val in src_prj[rule]:
                self.__gather_used_scenario(src_prj[rule][val], scenario)

    def __postprocess_project(self, src_prj, rts_prjs):
        # First step: remove the directories from the rules that are not
        # used by any runtime
        self.__simplify_rules(src_prj, rts_prjs)

        # Second step: detect the scenario variables that can be considered
        # constant, and merge the directories with the upper level rule
        for key in self.scenario:
            choices = []
            for rts in rts_prjs:
                if key in rts:
                    if rts[key] not in choices:
                        choices.append(rts[key])
                else:
                    if '__default__' not in choices:
                        choices.append('__default__')
            if len(choices) <= 1:
                # scenario not used or used with always the same value: can be
                # removed
                self.__remove_scenario(key, src_prj)

    def __simplify_rules(self, src_prj, rts_prjs):
        for name in src_prj.keys():
            if name == '__dirs__':
                continue
            # if a scenario variable in rules is never set in the various rts
            # views, then we can remove it
            in_use = False
            for rts in rts_prjs:
                if name in rts:
                    in_use = True
                    break
            if not in_use:
                del (src_prj[name])
                continue

            # the scenario variable is in use: let's check the values that
            # are actually used by the installed runtimes
            for val in src_prj[name].keys():
                # recursive simplification to handle nested case statements
                self.__simplify_rules(src_prj[name][val], rts_prjs)
                in_use = False
                for rts in rts_prjs:
                    if name in rts and rts[name] == val:
                        in_use = True
                        break
                if not in_use:
                    # the scenario value is not used, we can remove the
                    # directories for this scenario case
                    src_prj[name][val] = {'__dirs__': []}

    def __remove_scenario(self, scenario, src_prj):
        """Removes any mention of 'scenario' in the source project files"""
        for name in src_prj.keys():
            if name == '__dirs__':
                continue
            for val in src_prj[name]:
                self.__remove_scenario(scenario, src_prj[name][val])
        if scenario in src_prj:
            for val in src_prj[scenario]:
                # merge the directories to the upper level
                src_prj['__dirs__'] += src_prj[scenario][val]['__dirs__']
                for sub in src_prj[scenario][val]:
                    if sub != '__dirs__':
                        # also move the nested case statements to the upper
                        # level
                        if sub not in src_prj:
                            src_prj[sub] = src_prj[scenario][val][sub]
                        else:
                            src_prj[sub].update(src_prj[scenario][val][sub])
            del src_prj[scenario]

    def __add_rule(self, current, dir, rules):
        if rules is None or len(rules) == 0:
            if '__dirs__' not in current:
                current['__dirs__'] = []
            current['__dirs__'].append(dir)
            return

        rule = rules[0]
        if len(rules) == 1:
            rules = None
        else:
            rules = rules[1:]

        var, cases = rule.split(':')
        assert var in self.scenario, "Unknown scenario variable: %s" % var
        cases = cases.split(',')
        if var not in current:
            current[var] = {}
            for case in self.scenario[var]:
                current[var][case] = {'__dirs__': []}
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
                        if scenario_case not in current[var]:
                            current[var][scenario_case] = {}
                        self.__add_rule(
                            current[var][scenario_case], dir, rules)
            else:
                if case not in current[var]:
                    current[var][case] = {}
                self.__add_rule(current[var][case], dir, rules)

    def add_rule(self, dir, rules):
        if isinstance(rules, basestring):
            self.add_rule(dir, [rules])
            return

        if 'gnarl' in dir:
            current = self.gnarl_rules
        else:
            current = self.gnat_rules
        self.__add_rule(current, dir, rules)

    def __install_dir(self, dirname, destination, installed_files):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)
        if dirname not in self.rts:
            self.rts.append(dirname)

        destdir = os.path.join(destination, dirname)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for k, v in self.dirs[dirname].items():
            self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                            installed_files=installed_files)

    def _init_zfp(self):
        """Files common to all runtimes"""
        # files there no matter what

        self.add_rule('common', None)
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
            self.add_rule('zfp_io', 'RTS:zfp')
            self.add_sources('zfp_io', {'a-textio.adb': 'a-textio-zfp.adb'})
            self.add_rule('sfp_io', 'RTS:!zfp')
            self.add_sources('sfp_io', {'a-textio.adb': 'a-textio-raven.adb'})

        # FPU support sources
        self.add_rule('fpu', 'Has_FPU:true')
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
        self.add_rule('newlib', ['Add_C_Support:newlib'])
        self.add_sources('newlib', [
            'newlib-bb.c'])

        # Math support
        self.add_rule('math',
                      ['Add_Math_Lib:softfloat,hardfloat_sp,hardfloat_dp'])
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
                      ['RTS:ravenscar-full',
                       'Add_Math_Lib:!no'])
        self.add_sources('math/full', [
            'a-ngcoar.ads', 'a-ngcoar.adb',
            'a-ngrear.ads', 'a-ngrear.adb',
            'a-nurear.ads'])

        if self._is_bb:
            self.add_rule('math/softsp', 'Add_Math_Lib:softfloat')
            self.add_sources('math/softsp', {
                's-lisisq.adb': 's-lisisq-ada.adb'})
            self.add_rule('math/softdp', 'Add_Math_Lib:softfloat,hardfloat_sp')
            self.add_sources('math/softdp', {
                's-lidosq.adb': 's-lidosq-ada.adb'})

            self.add_rule('math/hardsp',
                          'Add_Math_Lib:hardfloat_sp,hardfloat_dp')
            self.add_sources('math/hardsp', {
                's-lisisq.adb': 's-lisisq-fpu.adb'})
            self.add_rule('math/harddp', 'Add_Math_Lib:hardfloat_dp')
            self.add_sources('math/harddp', {
                's-lidosq.adb': 's-lidosq-fpu.adb'})
        else:
            # PikeOS
            self.add_sources('math', {
                's-lisisq.adb': 's-lisisq-fpu.adb',
                's-lidosq.adb': 's-lidosq-fpu.adb'})

        # Finally, the ZFP & SFP-specific libgnat files
        self.add_rule('zfp', 'RTS:zfp,ravenscar-sfp')
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
                {'s-multip.ads': 's-multip-raven-default.ads',
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
        self.add_rule('gnarl/sfp', 'RTS:ravenscar-sfp')
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

    def _init_full(self):
        """ravenscar-full files"""

        # libgnat files for the full profile
        self.add_rule('full', 'RTS:ravenscar-full')
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
            'RTS:ravenscar-full', 'CPU_Family:arm'])
        self.add_sources('full/zcx-arm', {
            's-excmac.adb': 's-excmac-arm.adb',
            's-excmac.ads': 's-excmac-arm.ads',
            's-traceb.adb': 's-traceb-xi-armeabi.adb'})
        self.add_rule('full/zcx-dw2', [
            'RTS:ravenscar-full', 'CPU_Family:!arm'])
        self.add_sources('full/zcx-dw2', [
            {'s-excmac.ads': 's-excmac-gcc.ads',
             's-excmac.adb': 's-excmac-gcc.adb'},
            'libgcc/unwind-dw2-fde.h'])
        if self._is_bb:
            self.add_sources('full/zcx-dw2', 'src/unwind-dw2-fde-bb.c')

        self.add_rule('full/zcx-ppc',
                      ['RTS:ravenscar-full', 'CPU_Family:powerpc'])
        self.add_sources('full/zcx-ppc', {
            's-traceb.adb': 's-traceb-xi-ppc.adb'})
        self.add_rule('full/zcx-leon',
                      ['RTS:ravenscar-full', 'CPU_Family:leon'])
        self.add_sources('full/zcx-leon', {
            's-traceb.adb': 's-traceb-xi-sparc.adb'})
        self.add_rule('full/zcx-x86',
                      ['RTS:ravenscar-full', 'CPU_Family:x86'])
        self.add_sources('full/zcx-x86', {
            's-traceb.adb': 's-traceb-vx653-sim.adb'})

        # Containers
        self.add_rule('containers', 'RTS:ravenscar-full')
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
        self.add_rule('gnarl/full', 'RTS:ravenscar-full')
        self.add_sources('gnarl/full', {
            's-taskin.ads': 's-taskin-xi-full.ads',
            's-tposen.adb': 's-tposen-xi-full.adb',
            's-tposen.ads': 's-tposen-xi-full.ads'})

        if self._is_bb:
            self.add_sources('gnarl/full', [
                's-btstch.ads', 's-btstch.adb'])

        # Ravenscar extended: relative delays
        self.add_rule('gnarl/full/extended', 'RTS:ravenscar-full')
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

    def zfp_scenarios(self, config, mem_routines, math_lib):
        """Returns the list of directories contained in a base ZFP runtime"""
        ret = {}
        ret['RTS'] = 'zfp'

        if config.has_fpu:
            ret['Has_FPU'] = 'true'
        else:
            ret['Has_FPU'] = 'false'

        if mem_routines:
            ret['Add_Memory_Operations'] = 'yes'
        else:
            ret['Add_Memory_Operations'] = 'no'

        if math_lib:
            if self._is_bb:
                if not config.has_single_precision_fpu:
                    ret['Add_Math_Lib'] = 'softfloat'
                elif not config.has_double_precision_fpu:
                    ret['Add_Math_Lib'] = 'hardfloat_sp'
                else:
                    ret['Add_Math_Lib'] = 'hardfloat_dp'
            else:
                ret['Add_Math_Lib'] = 'hardfloat_dp'
        else:
            ret['Add_Math_Lib'] = 'no'

        ret['Add_C_Support'] = "no"

        cpu = config.target.split('-')[0]

        if cpu in ('arm', 'aarch64'):
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

        self.targets.append(config)
        self.rts.append(ret)
        return ret

    def sfp_scenarios(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base SFP runtime"""
        ret = self.zfp_scenarios(config, mem_routines, math_lib)
        ret['RTS'] = 'ravenscar-sfp'

        if not config.is_pikeos:
            # source installation for PikeOS do not consider those
            if config.has_timer_64:
                ret['Timer'] = 'timer64'
            else:
                ret['Timer'] = 'timer32'

            if small_mem:
                ret['Memory_Profile'] = 'small'
            else:
                ret['Memory_Profile'] = 'large'

        else:
            ret['Pikeos_Version'] = config.pikeos_version

        return ret

    def full_scenarios(self, config, mem_routines, math_lib, small_mem):
        """Returns the list of directories contained in a base full runtime"""
        ret = self.sfp_scenarios(config, mem_routines, math_lib, small_mem)

        # override the RTS value
        ret['RTS'] = 'ravenscar-full'

        if not config.is_pikeos:
            # PikeOS provides its own C library
            # ravenscar-full requires C memory operations, either via newlib
            # or via our own implementation in Ada
            if config.has_newlib:
                ret['Add_C_Support'] = "newlib"
            else:
                ret['Add_C_Support'] = "ada_clib"

        return ret


def build_configs(target):
    if target == 'arm-pikeos':
        t = ArmPikeOS()
    elif target == 'ppc-pikeos':
        t = PpcPikeOS()
    elif target == 'x86-pikeos':
        t = X86PikeOS()
    elif target == 'zynq7000':
        t = Zynq7000()
    elif target == 'rpi2':
        t = Rpi2()
    elif target == 'rpi3':
        t = Rpi3()
    elif target == 'aarch64-qemu':
        t = AARCH64QEMU()
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
    elif target == 'leon4':
        t = Leon4()
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

    return t


def usage():
    print "usage: build-rts.py OPTIONS board1 board2 ..."
    print "Options are:"
    print " -v --verbose     be verbose"
    print " --output=DIR     output directory"
    print " --prefix=DIR     where built rts will be installed"
    print " --gcc-dir=DIR    gcc source directory"
    print " --gnat-dir=DIR   gnat source directory"
    print " --cross-dir=DIR  cross source directory"
    print " --link           create symbolic links"


def main():
    # global link, gccdir, gnatdir, crossdir, verbose, create_common

    Config.install = Config.objdir

    try:
        opts, args = getopt.getopt(
            sys.argv[1:], "hvl",
            ["help", "verbose",
             "output=", "prefix=",
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
        elif opt == "--prefix":
            Config.prefix = arg
        else:
            print "unexpected switch: %s" % opt
            sys.exit(2)

    if len(args) < 1:
        print "error: missing configuration"
        print "Try --help"
        sys.exit(2)

    is_pikeos = 'pikeos' in args[0]
    Config.rts_srcs = SourceDirs(not is_pikeos)
    bsp_root = os.path.join(Config.install, Config.bsp_sources)

    # install all bsps
    for target in args:
        if 'pikeos' in target:
            assert is_pikeos, \
                "Cannot generate the runtimes for a mix of pikeos and bb rts"
        target = build_configs(target)
        target.install(bsp_root)

    # post-processing, install ada_object_path and ada_source_path to be
    # installed in all runtimes by gprinstall
    bsp_support = os.path.join(bsp_root, 'support')
    if not os.path.exists(bsp_support):
        os.mkdir(bsp_support)
        with open(os.path.join(bsp_support, 'ada_source_path'), 'w') as fp:
            fp.write('gnat\ngnarl\n')
        with open(os.path.join(bsp_support, 'ada_object_path'), 'w') as fp:
            fp.write('adalib\n')

    Config.rts_srcs.install(Config.install)

if __name__ == '__main__':
    main()
