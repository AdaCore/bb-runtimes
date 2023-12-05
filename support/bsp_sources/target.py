import copy
import sys

from support import readfile, is_string, Compiler, target_compiler
from support.files_holder import FilesHolder
from support.bsp_sources.archsupport import ArchSupport
from support.rts_sources.profiles import RTSProfiles


class TargetConfiguration(object):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def name(self):
        """Board's name, as used to name the runtime (e.g. light-<name>)"""
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
    def is_64bit(self):
        """Set to True on 64-bit targets"""
        return False

    @property
    def is_pikeos(self):
        return self.target is not None and 'pikeos' in self.target

    @property
    def is_legacy_format(self):
        """Create the runtime with the legacy naming format.

        Create the runtime with the legacy format with an rts-
        prefix. By default this is only enabled for PikeOS.
        """
        return self.is_pikeos

    @property
    def is_native(self):
        return self.target is None or 'native' in self.target

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
    def has_huge_memory(self):
        """Set to True on targets with lots (> 512MB) of RAM"""
        return False

    @property
    def use_semihosting_io(self):
        """ARM specific: whether to use a serial text io or the semihosting"""
        return False

    @property
    def has_timer_64(self):
        """True if the hardware provides a 64-bit timer. Else 32-bit timer is
        assumed.
        """
        return False

    @property
    def has_command_line_arguments(self):
        """True if the OS supports command line arguments"""
        return False

    @property
    def has_compare_and_swap(self):
        """True if the hardware supports an atomic compare-and-swap function.

        The default is to return True here as (at least for now) only the
        LEON processor may not support CAS, all other having proper support or
        at the minimum proper emulation when they are uni-processor.

        On LEON target, some variants of the CPU may not support it, while gcc
        expects the support: this may thus generate invalid instructions.
        """
        return True

    @property
    def has_cheri(self):
        """True if the hardware supports CHERI instructions"""
        return False

    def has_libc(self, profile):
        """Whether libc is available and used on the target"""
        if profile == 'embedded':
            # By default, we provide the newlib with the Embedded
            # runtimes
            return True
        else:
            # Otherwise, we don't assume any libc is available on Light or
            # Light-Tasking profiles
            return False

    @property
    def use_certifiable_packages(self):
        """True if the Light and Light-Tasking runtimes are to use certifiable
        runtime components. In practise, most packages in these runtimes are
        certifiable with the notable exception of libgcc. When true, our Ada
        implementation of libgcc is used.
        """
        return False

    @property
    def compiler_switches(self):
        """Switches to be used when compiling. Common to Ada, C, ASM"""
        return ()

    @property
    def c_switches(self):
        """Switches to be used when compiling C code."""
        return ()

    @property
    def readme_file(self):
        return None


class Target(TargetConfiguration, ArchSupport):
    """Handles the creation of runtimes for a particular target"""

    def __init__(self):
        """Initialize the target

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        TargetConfiguration.__init__(self)
        ArchSupport.__init__(self)
        self.config_files = {}
        self.runtimes = {}

        self.rts_options = RTSProfiles(self)

        # Flags for keys in target_options.gpr.in must end in _flags. This is
        # to avoid duplicating the list of flags in support/bsp_sources/installer.py.
        self.build_flags = {'source_dirs': None,
                            'common_flags': ['-ffunction-sections',
                                             '-fdata-sections'],
                            'common_gnarl_flags': [],
                            'common_debug_flags': ['-g'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc', '-DLIGHT_RUNTIME']}
        # GNAT-LLVM doesn't support -fcallgraph-info
        if target_compiler() != Compiler.gnat_llvm:
            self.build_flags['common_flags'].append('-fcallgraph-info=su,da')

        # Add the following compiler switches to runtime.xml for all targets:
        #
        #   -fno-tree-loop-distribute-patterns (GNAT only):
        #     This optimization looks for code patterns that can be replaced
        #     with library calls, for example memset and strlen. This creates
        #     a hidden runtime dependency that is considered undesirable by many
        #     of our customers. strlen is particularly problematic, as its not
        #     provided in our light and light-tasking runtimes.
        if target_compiler() == Compiler.gnat:
            self.global_compiler_switches = (
                '-fno-tree-loop-distribute-patterns',
            )
        else:
            self.global_compiler_switches = ()

        readme = self.readme_file
        if readme:
            self.config_files.update({'README': readfile(readme)})

        for profile in self.system_ads:
            # Set the scenario variable values for the base profile
            rts = FilesHolder()
            self.runtimes[profile] = rts

            # Setup the base profile
            base_profile = self.base_profile(profile)
            if base_profile == 'none':
                rts.rts_vars = self.rts_options.no_scenario()
            elif base_profile == 'light':
                rts.rts_vars = self.rts_options.light_scenarios()
            elif base_profile == 'light-tasking':
                rts.rts_vars = self.rts_options.light_tasking_scenarios()
            elif base_profile == 'embedded':
                rts.rts_vars = self.rts_options.embedded_scenarios()
            elif base_profile == 'cert':
                rts.rts_vars = self.rts_options.cert_scenarios()
            else:
                print('Error: unknown profile %s' % profile)
                sys.exit(2)

            # By default, system.ads files are searched for in
            # bb-runtimes/src/system.
            # This works fine in general, however, for custom runtimes, we may
            # need to change the location of this file for various reasons
            # so if we detect a slash in the base name, this means that we
            # lookup the file as any other regular source file.
            system_ads = self.system_ads[profile]
            if '/' in system_ads:
                rts.add_source_alias('gnat', 'system.ads', system_ads)
            else:
                rts.add_source_alias('gnat', 'system.ads',
                                     'src/system/%s' % system_ads)
            rts.build_flags = copy.deepcopy(self.build_flags)
            rts.config_files = {}

            # Update the runtimes objects according to target specifications
            self.amend_rts(profile, rts)
            # Check that dependencies are met
            self.rts_options.check_deps(rts.rts_vars)

        assert len(self.runtimes) > 0, "No runtime defined"

    @property
    def compiler_switches(self):
        return ()

    def amend_rts(self, rts_profile, rts):
        """to be overriden by the actual target to refine the runtime"""
        pass

    def base_profile(self, profile):
        """Return the base profile for the given profile. Custom profiles
        are required to overide this to point to an existing profile to use as
        the base for the new profile."""
        return profile

    def other_sources(self, profile):
        """Used to return a list of source dirs to install.

         The expected returned format is:
         { 'rts_subdir': [ <dir1>, <dir2> ] }

         the sources in <dir1>, <dir2> will then be installed in the runtime's
         subdirectory rts_subdir.
         """
        return None

    def other_projects(self, profile):
        """List of projects to build in the runtime"""
        return None

    ###############
    # runtime.xml #
    ###############

    def dump_runtime_xml(self, rts_name, rts):
        """Dumps the runtime.xml file that gives the configuration to gprbuild
        """
        ret = '<?xml version="1.0" ?>\n\n'
        ret += '<gprconfig>\n'
        ret += '  <configuration>\n'
        ret += '    <config><![CDATA[\n'
        if self.loaders is not None:
            # Add USER loader so users can always specify their own linker
            # script. To ensure the USER loader is always used for this
            # purpose it cannot be defined by a target
            assert 'USER' not in self.loaders, \
                "target cannot define USER loader"

            loaders = list(self.loaders) + ['USER']
        else:
            assert len(self.ld_scripts) <= 1, (
                "target configuration error: no loader specified and several"
                " ld scripts are defined")
            if len(self.ld_scripts) == 1:
                loaders = ['DEFAULT', 'USER']
                self.ld_scripts[0].add_loader('DEFAULT')
            else:
                loaders = ['USER']

        ret += '   type Loaders is ("%s");\n' % '", "'.join(
            loaders)
        ret += '   Loader : Loaders := external("LOADER", "%s");\n\n' % \
            loaders[0]

        ret += '   package Compiler is\n'

        compiler_switches = self.compiler_switches + self.global_compiler_switches
        if len(compiler_switches) > 0:
            ret += '      Common_Required_Switches := ("%s");\n' % \
                   '", "'.join(compiler_switches)
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
        for sw in self.ld_switches:
            if sw['loader'] is None or sw['loader'] == '':
                switches.append('"%s"' % sw['switch'])

        ret += '   package Linker is\n'
        indent = 6
        blank = indent * ' '
        ret += blank + \
            'for Required_Switches use Linker\'Required_Switches &\n'
        ret += blank + '  ("-Wl,-L${RUNTIME_DIR(Ada)}/adalib",\n'
        indent = 9
        blank = indent * ' '

        if self.name == "qnx":
            # QNX does not require additional linker switches. If -nostartfiles
            # or -nolibc are used the binary will exit with a Memory fault.
            # Since we link with libc on this platform we can't use -nostdlib
            # either.
            pass
        elif rts.rts_vars['RTS_Profile'] != "embedded":
            # For the ZFP and Ravenscar SFP runtime we have the choice of
            # either using libgcc or our Ada libgcc replacement. For the
            # later choice we do not link with any of the standard libraries.
            if rts.rts_vars['Certifiable_Packages'] == "yes":
                ret += blank + '"-nostdlib",'
            else:
                ret += blank + '"-nostartfiles", "-nolibc",'
        else:
            # In the Embedded case, the runtime depends on
            # functionalities from newlib, such as memory allocation. This
            # runtime also does not support the certifiable packages option.
            # Also, there's interdependencies between libgnarl and libgnat,
            # so we need to force -lgnarl at link time, always.
            #
            # We provide the link arguments for libc ourselves. Inhibit the
            # gcc mechanism doing so with -nolibc first. Then we need to
            # account for intricacies in dependencies, e.g. libc depends on
            # libgcc as everyone, libgcc on libc for strlen, libgnat on libc
            # for __errno or other, libc on libgnat for sbrk, libgnat and
            # libgnarl on each other...

            ret += blank + \
                '"-nostartfiles", "-nolibc", ' + \
                '"-Wl,--start-group,-lgnarl,-lgnat,-lc,-lgcc,-lgcc_eh,--end-group",'

        # Add the user script path first, so that they have precedence
        ret += '\n' + blank + '"-L${RUNTIME_DIR(ada)}/ld_user"'
        # And then our own script(s), if any
        if len(self.ld_scripts) > 0:
            ret += ',\n' + blank + '"-L${RUNTIME_DIR(ada)}/ld"'

        if len(switches) > 0:
            ret += ',\n' + blank
            ret += (',\n' + blank).join(switches)
            blank = indent * ' '
        ret += ') &\n' + blank + 'Compiler.Common_Required_Switches;\n'
        indent = 6
        blank = indent * ' '

        if loaders is not None:
            ret += '\n' + blank
            ret += 'case Loader is\n'
            indent += 3
            blank = indent * ' '

            for loader in loaders:
                ret += blank
                ret += 'when "%s" =>\n' % loader
                if loader == 'USER':
                    continue
                indent += 3
                blank = indent * ' '

                switches = []
                for val in self.ld_scripts:
                    if val.loaders is None or loader in val.loaders:
                        switches.append('"-T", "%s"' % val.name)
                for sw in self.ld_switches:
                    if is_string(sw['loader']) \
                            and sw['loader'] == loader:
                        switches.append('"%s"' % sw['switch'])
                    if isinstance(sw['loader'], list) \
                            and loader in sw['loader']:
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
