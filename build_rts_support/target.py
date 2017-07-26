import copy
import os

from bsp import BSP
from build_rts_support import readfile
from files_holder import FilesHolder, fullpath
from rts_sources import RTSOptions


class TargetConfiguration(object):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def name(self):
        raise Exception("not implemented")

    @property
    def target(self):
        raise Exception("not implemented")

    @property
    def is_pikeos(self):
        return self.target is not None and 'pikeos' in self.target

    @property
    def is_native(self):
        return False

    @property
    def has_fpu(self):
        return self.is_pikeos or \
            self.has_single_precision_fpu or self.has_double_precision_fpu

    @property
    def has_single_precision_fpu(self):
        return self.has_double_precision_fpu

    @property
    def has_double_precision_fpu(self):
        raise Exception("not implemented")

    @property
    def use_semihosting_io(self):
        return False

    @property
    def has_timer_64(self):
        raise Exception("not implemented")

    @property
    def has_newlib(self):
        if self.is_pikeos:
            return False
        raise Exception("not implemented")

    @property
    def bspclass(self):
        raise Exception("not implemented")

    @property
    def add_linker_section(self):
        """Whether runtime.xml contains a linker section"""
        return True

    @property
    def compiler_switches(self):
        """Switches to be used when compiling. Common to Ada, C, ASM"""
        return ()

    @property
    def c_switches(self):
        """Switches to be used when compiling C code."""
        return ()

    @property
    def zfp_system_ads(self):
        """The system.ads file to use for a zfp runtime"""
        return None

    @property
    def sfp_system_ads(self):
        """The system.ads file to use for a ravenscar-sfp runtime"""
        return None

    @property
    def full_system_ads(self):
        """The system.ads file to use for a ravenscar-full runtime"""
        return None


class Target(TargetConfiguration, BSP):
    """Handles the creation of runtimes for a particular target"""
    @property
    def rel_path(self):
        return self._parent.rel_path + self.name + '/'

    def __init__(self, mem_routines, small_mem):
        """Initialize the target
        :param mem_routines: True for adding memory functions (memcpy..)
        :param small_mem: True when targetting a board with minimal memory

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        TargetConfiguration.__init__(self)
        BSP.__init__(self)
        self._mem_routines = mem_routines
        self._small_mem = small_mem
        self.config_files = {}
        self.runtimes = {}
        self.rts_options = RTSOptions(self)

        self.build_flags = {'source_dirs': None,
                            'common_flags': ['-fcallgraph-info=su,da',
                                             '-ffunction-sections',
                                             '-fdata-sections'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc']}

        readme = self.readme_file
        if readme:
            self.config_files.update({'README': readfile(readme)})

        if self.zfp_system_ads is not None:
            self.runtimes['zfp'] = FilesHolder()
            self.runtimes['zfp'].rts_vars = self.rts_options.zfp_scenarios(
                self._mem_routines, math_lib=False)
            self.runtimes['zfp'].add_sources('arch', {
                'system.ads': 'src/system/%s' % self.zfp_system_ads})

        if self.sfp_system_ads is not None:
            self.runtimes['ravenscar-sfp'] = FilesHolder()
            self.runtimes['ravenscar-sfp'].rts_vars = \
                self.rts_options.sfp_scenarios(
                    self._mem_routines,
                    math_lib=False,
                    small_mem=self._small_mem)
            self.runtimes['ravenscar-sfp'].add_sources('arch', {
                'system.ads': 'src/system/%s' % self.sfp_system_ads})

        if self.full_system_ads is not None:
            self.runtimes['ravenscar-full'] = FilesHolder()
            self.runtimes['ravenscar-full'].rts_vars = \
                self.rts_options.full_scenarios(
                    mem_routines, math_lib=True, small_mem=small_mem)
            self.runtimes['ravenscar-full'].add_sources('arch', {
                'system.ads': 'src/system/%s' % self.full_system_ads})

        assert len(self.runtimes) > 0, "No runtime defined"

        for k, v in self.runtimes.items():
            v.build_flags = copy.deepcopy(self.build_flags)
            v.config_files = {}

    def amend_zfp(self, cfg):
        """to be overriden by the actual target to refine the zfp runtime"""
        cfg.rts_xml = self.runtime_xml(cfg)

    def amend_ravenscar_sfp(self, cfg):
        self.amend_zfp(cfg)

    def amend_ravenscar_full(self, cfg):
        self.amend_ravenscar_sfp(cfg)

    #########################
    # dump_rts_project_file #
    #########################

    def dump_rts_project_file(self, rts, destination, rts_prefix):
        """Dumps the main project used to build the runtime"""
        rtsname = '%s-%s' % (rts['RTS_Profile'], self.name)
        prj = '%s.gpr' % rtsname.replace('-', '_')
        prjname = rtsname.replace('-', '_').title()
        prj = os.path.join(destination, prj)

        base = os.path.dirname(rts_prefix)
        ret = 'aggregate project %s is\n' % prjname
        ret += '\n'
        ret += '   Base_BSP_Source_Dir   := Project\'Project_Dir & "%s";\n' % \
               self.rel_path
        ret += '   Base_Installation_Dir := "%s/";\n' % base
        if not self.is_pikeos and not self.is_native:
            board = os.path.basename(rts_prefix).replace(
                '%s-' % rts['RTS_Profile'], '')
            ret += '   Board                 := "%s";\n' % board
            ret += '   Default_Prefix        := \n'
            ret += '     Base_Installation_Dir & "%s-" & Board;\n' % \
                   rts['RTS_Profile']
        else:
            ret += '   Default_Prefix        := \n'
            ret += '     Base_Installation_Dir & "%s";\n' % \
                   os.path.basename(rts_prefix)
        ret += '   Install_Dir           := ' \
               'external ("PREFIX", Default_Prefix);\n'
        ret += '\n'
        for val in rts:
            ret += '   for external ("%s") use "%s";\n' % (val, rts[val])
        ret += '\n'
        ret += '   for external ("INSTALL_PREFIX") use Install_Dir;\n'
        ret += '\n'

        if self.target is not None:
            ret += '   for Target use "%s";\n' % self.target
        ret += '   for Runtime ("Ada") use Base_BSP_Source_Dir &\n'
        ret += '       "%s";\n' % rts['RTS_Profile']
        ret += '\n'
        ret += '   for Project_Path use (Base_BSP_Source_Dir & "%s");\n' %\
               rts['RTS_Profile']
        ret += '   for Project_Files use\n'
        if rts['RTS_Profile'] == 'zfp':
            ret += '     (Base_BSP_Source_Dir & "internal/libgnat.gpr",\n'
        elif rts['RTS_Profile'] == 'ravenscar-sfp':
            ret += '     (Base_BSP_Source_Dir & "internal/libgnat.gpr",\n'
            ret += '      Base_BSP_Source_Dir & "internal/libgnarl.gpr",\n'
        else:
            ret += ('     (Base_BSP_Source_Dir & '
                    '"internal/libgnat_merged.gpr",\n')
            ret += ('      Base_BSP_Source_Dir & '
                    '"internal/libgnarl_empty.gpr", \n')
        ret += '      Base_BSP_Source_Dir & "%s/install.gpr");\n' % \
               rts['RTS_Profile']
        ret += '\n'
        ret += 'end %s;\n' % prjname

        with open(prj, 'w') as fp:
            fp.write(ret)

    ###############
    # runtime_xml #
    ###############

    def runtime_xml(self, rts):
        " Dumps the runtime.xml file that gives the configuration to gprbuild"
        ret = '<?xml version="1.0" ?>\n\n'
        ret += '<gprconfig>\n'
        ret += '  <configuration>\n'
        ret += '    <config><![CDATA[\n'
        if self.loaders is not None:
            ret += '   type Loaders is ("%s");\n' % '", "'.join(
                self.loaders)
            ret += '   Loader : Loaders := external("LOADER", "%s");\n\n' % (
                self.loaders[0])
        elif len(self.ld_scripts) == 1:
            # No loader defined, and a single ld script
            # Let's make it user-configurable
            ret += '   LDSCRIPT := external("LDSCRIPT",\n'
            ret += '                        "${RUNTIME_DIR(ada)}/ld/%s");' % (
                self.ld_scripts[0]['name'],)
            ret += '\n\n'

        ret += '   package Compiler is\n'
        if len(self.compiler_switches) > 0:
            ret += '      Common_Required_Switches := ("%s");\n' % \
                   '", "'.join(self.compiler_switches)
        else:
            ret += '      Common_Required_Switches := ();\n'

        if len(self.c_switches) > 0:
            ret += '      C_Required_Switches := ("%s");\n' % \
                   '", "'.join(self.c_switches)

        ret += '\n'

        for lang in ('Ada', 'C', 'Asm', 'Asm2', 'Asm_Cpp'):
            w = '      '
            ret += w + 'for Leading_Required_Switches ("%s") use\n' % lang
            w = '         '
            ret += w + 'Compiler\'Leading_Required_Switches ("%s") &\n' % \
                       lang
            ret += w + 'Common_Required_Switches'
            if lang != 'Ada' and len(self.c_switches) > 0:
                ret += ' &\n' + w
                ret += 'C_Required_Switches'
            ret += ';\n'
        ret += '   end Compiler;\n\n'

        if not self.add_linker_section:
            ret += ']]>\n'
            ret += '    </config>\n'
            ret += '  </configuration>\n'
            ret += '</gprconfig>\n'
            return ret

        switches = []
        if len(self.ld_scripts) == 1 and self.loaders is None:
            switches.append('"-T", LDSCRIPT')
        else:
            for val in self.ld_scripts:
                if val['loader'] is None:
                    # use for all loaders
                    switches.append('"-T", "%s"' % val['name'])
        for sw in self.ld_switches:
            if sw['loader'] is None or sw['loader'] == '':
                switches.append('"%s"' % sw['switch'])

        if rts.rts_vars['RTS_Profile'] == 'ravenscar-full':
            do_merge = True
        else:
            do_merge = False

        if do_merge:
            # We need a binder section to prevent the link with libgnarl, which
            # we don't have as it's merged with libgnat in this case
            ret += '   package Binder is\n'
            ret += ('      for Required_Switches ("Ada") use '
                    'Binder\'Required_Switches ("Ada") &\n')
            ret += '        ("-nostdlib");\n'
            ret += '   end Binder;\n\n'

        ret += '   package Linker is\n'
        indent = 6
        blank = indent * ' '
        ret += blank + \
            'for Required_Switches use Linker\'Required_Switches &\n'
        ret += blank + '  ("-L${RUNTIME_DIR(ada)}/adalib",\n'
        indent = 9
        blank = indent * ' '

        ret += blank + '"-nostartfiles"'
        if rts.rts_vars['RTS_Profile'] != "ravenscar-full":
            ret += ', "-nolibc"'
        else:
            ret += ', "-lgnat"'
            if self.has_newlib:
                ret += ', "-lc", "-lgnat"'
            ret += ', "-lgcc"'

        if len(self.ld_scripts) > 0:
            ret += ',\n' + blank + '"-L${RUNTIME_DIR(ada)}/ld"'

        if len(switches) > 0:
            ret += ',\n' + blank
            ret += (',\n' + blank).join(switches)
            blank = indent * ' '
        ret += ') &\n' + blank + 'Compiler.Common_Required_Switches;\n'
        indent = 6
        blank = indent * ' '

        if self.loaders is not None:
            ret += '\n' + blank
            ret += 'case Loader is\n'
            indent += 3
            blank = indent * ' '

            for l in self.loaders:
                ret += blank
                ret += 'when "%s" =>\n' % l
                indent += 3
                blank = indent * ' '

                switches = []
                for val in self.ld_scripts:
                    if val['loader'] is None:
                        continue
                    if isinstance(val['loader'], basestring):
                        if val['loader'] == l:
                            switches.append('"-T", "%s"' % val['name'])
                    else:
                        if l in val['loader']:
                            switches.append('"-T", "%s"' % val['name'])
                for sw in self.ld_switches:
                    if isinstance(sw['loader'], basestring) \
                            and sw['loader'] == l:
                        switches.append('"%s"' % sw['switch'])
                    if isinstance(sw['loader'], list) \
                            and l in sw['loader']:
                        switches.append('"%s"' % sw['switch'])
                if len(switches) > 0:
                    ret += blank
                    ret += \
                        'for Required_Switches use Linker\'Required_Switches'
                    ret += ' &\n' + blank + '  '
                    ret += '(%s);\n' % (',\n   ' + blank).join(switches)
                indent -= 3
                blank = indent * ' '

            indent -= 3
            blank = indent * ' '
            ret += '%send case;' % blank

        ret += ('   end Linker;\n'
                ']]>\n'
                '   </config>\n'
                '  </configuration>\n'
                '</gprconfig>\n')
        return ret

    def install(self, destination, prefix):
        # Update the runtimes objects according to target specifications
        if 'zfp' in self.runtimes:
            self.amend_zfp(self.runtimes['zfp'])
        if 'ravenscar-sfp' in self.runtimes:
            self.amend_ravenscar_sfp(self.runtimes['ravenscar-sfp'])
        if 'ravenscar-full' in self.runtimes:
            self.amend_ravenscar_full(self.runtimes['ravenscar-full'])

        # Build target directories
        destination = os.path.abspath(destination)
        if not os.path.exists(destination):
            os.mkdir(destination)

        installed_files = []

        gnarl_dirs = []
        gnarl_langs = []
        gnat_dirs = []
        gnat_langs = []
        script_files = []

        # Install the bsp
        base = destination
        base_bsp = os.path.join(base, self.rel_path)
        base_prj = os.path.join(base_bsp, 'internal')

        scripts = []
        self.install_ld_scripts(
            destination, scripts, installed_files)

        for d in scripts:
            full = os.path.join(base, d)
            rel = os.path.relpath(full, base_prj)
            script_files.append(rel)

        # Install source files for the BSP/RTSs
        bsp_gnat = []
        bsp_gnarl = []
        has_ravenscar = False
        for rts in self.runtimes.keys():
            if 'ravenscar' in rts:
                has_ravenscar = True
                break

        self.install_libgnat(
            destination, bsp_gnat, installed_files)

        if has_ravenscar:
            # install ravenscar support
            self.install_libgnarl(
                destination, bsp_gnarl, installed_files)

        for d in bsp_gnat:
            full = os.path.join(base, d)
            rel = os.path.relpath(full, base_prj)
            # gnat_dirs is used to generate libgnat.gpr, so relative to the
            # bsp directory
            gnat_dirs.append('"%s"' % rel)
            if 'C' not in gnat_langs and self.has_c(d):
                gnat_langs.append('C')
            if 'Asm_Cpp' not in gnat_langs and self.has_asm(d):
                gnat_langs.append('Asm_Cpp')

        for d in bsp_gnarl:
            full = os.path.join(base, d)
            rel = os.path.relpath(full, base_prj)
            gnarl_dirs.append('"%s"' % rel)
            if 'C' not in gnarl_langs and self.has_c(d):
                gnarl_langs.append('C')
            if 'Asm_Cpp' not in gnarl_langs and self.has_asm(d):
                gnarl_langs.append('Asm_Cpp')

        # Now install the rts-specific sources
        for rts_name, rts_obj in self.runtimes.items():
            base_rts = os.path.join(base_bsp, rts_name)
            if prefix is not None:
                if prefix.endswith('/'):
                    install_prefix = prefix
                else:
                    install_prefix = prefix + '/'
            elif self.target is not None:
                install_prefix = \
                    self.target + '/lib/gnat/'
            else:
                install_prefix = 'lib/gnat/'
            if self.is_pikeos or self.is_native:
                install_prefix += 'rts-%s' % rts_name
            else:
                install_prefix += '%s-%s' % (rts_name, self.name)

            if not os.path.exists(base_rts):
                os.makedirs(base_rts)

            src_dirs = []

            for dirname, l in rts_obj.dirs.items():
                if l and len(l) > 0:
                    src_dirs.append(dirname)
                    rel = '"../" & Profile & "/%s"' % dirname
                    if 'gnarl' in dirname:
                        if rel not in gnarl_dirs:
                            gnarl_dirs.append(rel)
                        if 'C' not in gnarl_langs and \
                           dirname in rts_obj.c_srcs:
                            gnarl_langs.append('C')
                        if 'Asm_Cpp' not in gnarl_langs and \
                           dirname in rts_obj.asm_srcs:
                            gnarl_langs.append('Asm_Cpp')
                    else:
                        if rel not in gnat_dirs:
                            gnat_dirs.append(rel)
                        if 'C' not in gnat_langs and \
                           dirname in rts_obj.c_srcs:
                            gnat_langs.append('C')
                        if 'Asm_Cpp' not in gnat_langs and \
                           dirname in rts_obj.asm_srcs:
                            gnat_langs.append('Asm_Cpp')

                    full = os.path.join(base_rts, dirname)

                    if not os.path.exists(full):
                        os.makedirs(full)

                    for srcname, pair in l.items():
                        self._copy_pair(srcname, pair, full)

            for d in ['obj', 'adalib']:
                path = os.path.join(base_rts, d)
                if not os.path.exists(path):
                    os.mkdir(path)

            # Generate ada_source_path, used for the rts bootstrap
            with open(os.path.join(base_rts, 'ada_source_path'), 'w') as fp:
                for d in sorted(src_dirs):
                    fp.write(d + '\n')

            # Generate ada_object_path
            with open(os.path.join(base_rts, 'ada_object_path'), 'w') as fp:
                fp.write('adalib\n')

            # Write config files
            for name, content in self.config_files.iteritems():
                with open(os.path.join(base_rts, name), 'w') as fp:
                    fp.write(content)
            if rts_obj.rts_xml is not None:
                with open(os.path.join(base_rts, 'runtime.xml'), 'w') as fp:
                    fp.write(rts_obj.rts_xml)

            # and now install the rts project with the proper scenario values
            self.dump_rts_project_file(
                rts_obj.rts_vars, destination, install_prefix)

            inst_files = ['runtime.xml']
            support_dir = os.path.relpath(
                os.path.join(destination, 'support'), base_rts)
            inst_files.append(os.path.join(support_dir, 'ada_source_path'))
            inst_files.append(os.path.join(support_dir, 'ada_object_path'))

            for name, content in rts_obj.config_files.iteritems():
                inst_files.append(name)
                with open(os.path.join(base_rts, name), 'w') as fp:
                    fp.write(content)

            if len(script_files) > 0:
                link_sources = '"%s"' % '",\n         "'.join(script_files)
            else:
                link_sources = ''

            build_flags = {
                'link_sources': link_sources,
                'rts_files': '",\n         "'.join(inst_files)}
            cnt = readfile(fullpath('src/install.gpr'))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(base_rts, 'install.gpr'), 'w') as fp:
                fp.write(cnt)

            # and the potentially runtime specific target_options.gpr project
            build_flags = {}
            for f in ['common_flags', 'asm_flags', 'c_flags']:
                build_flags[f] = '",\n        "'.join(rts_obj.build_flags[f])
            cnt = readfile(fullpath('src/target_options.gpr'))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(base_rts, 'target_options.gpr'), 'w') as fp:
                fp.write(cnt)

        if 'README' in self.config_files:
            cnt = self.config_files['README']
            readme_fname = os.path.join(
                destination, 'README-%s.txt' % self.name)
            with open(readme_fname, 'w') as fp:
                fp.write(cnt)

        # Set source_dirs and languages
        build_flags = {
            'gnat_source_dirs': ',\n      '.join(sorted(gnat_dirs)),
            'gnat_langs': '", "'.join(["Ada"] + gnat_langs),
            'gnarl_source_dirs': ',\n      '.join(sorted(gnarl_dirs)),
            'gnarl_langs': '", "'.join(["Ada"] + gnarl_langs),
            'all_langs': '", "'.join(["Ada"] + gnat_langs + gnarl_langs)
        }

        # Add the project files
        if not os.path.exists(base_prj):
            os.mkdir(base_prj)

        projects = ('libgnat', 'libgnarl', 'libgnat_merged', 'libgnarl_empty')
        for fname in projects:
            cnt = readfile(fullpath('src/%s.gpr' % fname))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(base_prj, '%s.gpr' % fname), 'w') as fp:
                fp.write(cnt)
        empty_c = os.path.join(base_prj, 'empty.c')
        with open(empty_c, 'w') as fp:
            fp.write('')


class DFBBTarget(Target):
    """BB target with single and double FPU"""

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def has_timer_64(self):
        return False

    @property
    def has_newlib(self):
        return True
