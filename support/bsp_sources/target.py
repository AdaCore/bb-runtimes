import copy

from support import readfile
from support.files_holder import FilesHolder
from support.bsp_sources.archsupport import ArchSupport
from support.rts_sources.profiles import RTSProfiles


class TargetConfiguration(object):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def name(self):
        raise Exception("not implemented")

    @property
    def system_ads(self):
        """a dictionary of runtime profiles and their associated system.ads"""
        raise Exception("not implemented")

    @property
    def target(self):
        """target name, as expected by gprbuild"""
        raise Exception("not implemented")

    @property
    def is_pikeos(self):
        return self.target is not None and 'pikeos' in self.target

    @property
    def has_fpu(self):
        """Whether the hardware provides a FPU.

        By default, set to True on PikeOS, or if has_*_precision_fpu is set.
        """
        return self.is_pikeos or \
            self.has_single_precision_fpu or self.has_double_precision_fpu

    @property
    def has_single_precision_fpu(self):
        """Whether the single precision floats are supported in FPU"""
        return self.has_double_precision_fpu

    @property
    def has_double_precision_fpu(self):
        """Whether the double precision floats are supported in FPU"""
        raise Exception("not implemented")

    @property
    def has_small_memory(self):
        """Set to True on targets with limited RAM"""
        return False

    @property
    def use_semihosting_io(self):
        """ARM specific: whether to use a serial text io or the semihosting"""
        return False

    @property
    def has_timer_64(self):
        """True if the hardware provide a 64-bit timer. Else 32-bit timer is
        assumed.
        """
        raise Exception("not implemented")

    def has_libc(self, profile):
        """Whether libc is available and used on the target"""
        if profile == 'ravenscar-full':
            # By default, we provide the newlib with the ravenscar-full
            # runtimes
            return True
        else:
            # Otherwise, we don't assume any libc is available on zfp or
            # ravenscar-sfp profiles
            return False

    @property
    def compiler_switches(self):
        """Switches to be used when compiling. Common to Ada, C, ASM"""
        return ()

    @property
    def c_switches(self):
        """Switches to be used when compiling C code."""
        return ()


class Target(TargetConfiguration, ArchSupport):
    """Handles the creation of runtimes for a particular target"""
    @property
    def rel_path(self):
        if self._parent is not None:
            return self._parent.rel_path + self.name + '/'
        else:
            return self.name + '/'

    def __init__(self):
        """Initialize the target

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        TargetConfiguration.__init__(self)
        ArchSupport.__init__(self)
        self.config_files = {}
        self.runtimes = {}

        self.rts_options = RTSProfiles(self)

        self.build_flags = {'source_dirs': None,
                            'common_flags': ['-fcallgraph-info=su,da',
                                             '-ffunction-sections',
                                             '-fdata-sections'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc']}

        readme = self.readme_file
        if readme:
            self.config_files.update({'README': readfile(readme)})

        for profile in self.system_ads:
            # Set the scenario variable values for the base profile
            rts = FilesHolder()
            self.runtimes[profile] = rts
            if 'ravenscar' not in profile:
                rts.rts_vars = self.rts_options.zfp_scenarios(math_lib=False)
            elif 'full' in profile:
                rts.rts_vars = self.rts_options.full_scenarios(math_lib=True)
            else:
                rts.rts_vars = self.rts_options.sfp_scenarios(math_lib=False)
            rts.add_sources('arch', {
                'system.ads': 'src/system/%s' % self.system_ads[profile]})
            rts.build_flags = copy.deepcopy(self.build_flags)
            rts.config_files = {}

            # Update the runtimes objects according to target specifications
            self.amend_rts(profile, rts)
            # Check that dependencies are met
            self.rts_options.check_deps(rts.rts_vars)

        assert len(self.runtimes) > 0, "No runtime defined"

    def amend_rts(self, rts_profile, rts):
        """to be overriden by the actual target to refine the runtime"""
        pass

    ###############
    # runtime.xml #
    ###############

    def dump_runtime_xml(self, rts_name, rts):
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

        ret += '   package Linker is\n'
        indent = 6
        blank = indent * ' '
        ret += blank + \
            'for Required_Switches use Linker\'Required_Switches &\n'
        ret += blank + '  ("-Wl,-L${RUNTIME_DIR(ada)}/adalib",\n'
        indent = 9
        blank = indent * ' '

        ret += blank + '"-nostartfiles"'
        if rts.rts_vars['RTS_Profile'] != "ravenscar-full":
            ret += ', "-nolibc"'
        else:
            # libgnat depends on libc for malloc stuff
            # libc and libgcc depends on libgnat for syscalls and abort
            ret += (', "-lgnat", "-lc", "-lgcc", "-lgnat"')

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
            ret += '%send case;\n' % blank

        ret += ('   end Linker;\n'
                ']]>\n'
                '   </config>\n'
                '  </configuration>\n'
                '</gprconfig>\n')
        return ret


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
