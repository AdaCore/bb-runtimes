import os

from config import Config
from files_holder import FilesHolder, readfile, fullpath


class TargetConfiguration(FilesHolder):
    """Gives information on the target to allow proper configuration of the
    runtime"""

    @property
    def target(self):
        raise Exception("not implemented")

    @property
    def is_pikeos(self):
        return 'pikeos' in self.target

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
    def has_timer_64(self):
        raise Exception("not implemented")

    @property
    def has_newlib(self):
        if self.is_pikeos:
            return False
        raise Exception("not implemented")

    @property
    def bspclass(self):
        return None


class Target(TargetConfiguration):
    """Handles the creation of runtimes for a particular target"""

    def __init__(self, mem_routines, small_mem):
        """Initialize the target
        :param mem_routines: True for adding memory functions (memcpy..)
        :param small_mem: True when targetting a board with minimal memory

        The build_flags dictionnary is used to set attributes of
        runtime_build.gpr"""
        super(Target, self).__init__()
        self._mem_routines = mem_routines
        self._small_mem = small_mem
        self.config_files = {}
        self.shared = None

        if self.bspclass is None:
            self.bsp = None
        else:
            self.bsp = self.bspclass()

        self.build_flags = {'source_dirs': None,
                            'common_flags': ['-fcallgraph-info=su,da',
                                             '-ffunction-sections',
                                             '-fdata-sections'],
                            'asm_flags': [],
                            'c_flags': ['-DIN_RTS', '-Dinhibit_libc']}

        readme = None
        if self.bsp:
            readme = self.bsp.readme_file
        if readme:
            self.config_files.update({'README': readfile(readme)})

    def amend_zfp(self):
        if self.bsp is not None:
            self.config_files.update(
                {'runtime.xml': self.bsp.runtime_xml()})
            # s-textio moved to the BSP
            self.remove_source('s-textio.adb')
            self.remove_source('s-macres.adb')

    def amend_ravenscar_sfp(self):
        self.amend_zfp()
        if self.bsp is not None:
            # Moved to BSP
            self.remove_source('a-intnam.ads')
            self.remove_source('s-bbbosu.adb')
            self.remove_source('s-bbcppr.ads')
            self.remove_source('s-bbcppr.adb')
            self.remove_source('s-bbinte.adb')
            self.remove_source('s-bbpara.ads')

    def amend_ravenscar_full(self):
        self.amend_ravenscar_sfp()
        if self.has_newlib:
            self.config_files['runtime.xml'] = \
              self.config_files['runtime.xml'].replace(
                  '"-nolibc", ', '"-lc", "-lgnat", ')

    def _init_zfp_config_files(self):
        self.config_files.update(
            {'target_options.gpr': readfile('src/target_options.gpr')})
        self.add_sources('arch', {
            'system.ads': None,
            's-macres.adb': None})
        self.add_sources('bsp', {
            # Implementation of s-textio is board-specific
            's-textio.adb': None})

    def _init_sfp_config_files(self):
        self._init_zfp_config_files()
        self.config_files.update(
            {'ravenscar_build.gpr': readfile('src/ravenscar_build.gpr')})
        self.add_sources('gnarl/bsp', 'a-intnam.ads')

        if not self.is_pikeos:
            self.add_sources('gnarl/bsp', {'s-bbpara.ads': None})
            self.add_sources('gnarl/arch', [
                's-bbcppr.ads',
                's-bbcppr.adb',
                's-bbbosu.adb',
                's-bbinte.adb'])
            self.update_pairs({
                's-bbcppr.adb': None,
                's-bbbosu.adb': None})

    def init_as_zfp(self):
        self.shared = Config.rts_srcs.zfp_dirs(
            self, self._mem_routines, math_lib=False)
        self._init_zfp_config_files()

        self.amend_zfp()

    def init_as_sfp(self):
        self.shared = Config.rts_srcs.sfp_dirs(
            self, self._mem_routines, False, self._small_mem)
        self._init_sfp_config_files()

        self.amend_ravenscar_sfp()

    def init_as_full(self):
        self.shared = Config.rts_srcs.full_dirs(
            self, self._mem_routines, True, self._small_mem)
        self._init_sfp_config_files()

        self.amend_ravenscar_full()

    def install(self, destination):
        assert self.shared is not None, "Uninitialized Target object"

        # Build target directories
        destination = fullpath(destination)
        os.mkdir(destination)

        installed_files = []

        src_dirs = []
        gnarl_dirs = []
        gnarl_langs = ['Ada']
        gnat_dirs = []
        gnat_langs = ['Ada']

        # Install the shared rts files
        for d in sorted(self.shared):
            dest = Config.rts_srcs.install(d, destination, installed_files)
            src_dirs.append(dest)
            if 'gnarl' in d:
                gnarl_dirs.append(dest)
                if 'C' not in gnarl_langs and d in Config.rts_srcs.c_srcs:
                    gnarl_langs.append('C')
                if 'Asm_Cpp' not in gnarl_langs \
                   and d in Config.rts_srcs.asm_srcs:
                    gnarl_langs.append('Asm_Cpp')
            else:
                gnat_dirs.append(dest)
                if 'C' not in gnat_langs and d in Config.rts_srcs.c_srcs:
                    gnat_langs.append('C')
                if 'Asm_Cpp' not in gnat_langs \
                   and d in Config.rts_srcs.asm_srcs:
                    gnat_langs.append('Asm_Cpp')

        # Install the bsp
        if self.bsp is not None:
            self.bsp.install_ld_scripts(
                destination, installed_files)

            bsp_gnat = []
            self.bsp.install_libgnat(
                destination, bsp_gnat, installed_files)

            for d in bsp_gnat:
                src_dirs.append(d)
                gnat_dirs.append(d)
                if 'C' not in gnat_langs and self.bsp.has_c(d):
                    gnat_langs.append('C')
                if 'Asm_Cpp' not in gnat_langs and self.bsp.has_asm(d):
                    gnat_langs.append('Asm_Cpp')
            if len(gnarl_dirs) > 0:
                # install ravenscar support
                bsp_gnarl = []
                self.bsp.install_libgnarl(
                    destination, bsp_gnarl, installed_files)
                for d in bsp_gnarl:
                    src_dirs.append(d)
                    gnarl_dirs.append(d)
                    if 'C' not in gnarl_langs and self.bsp.has_c(d):
                        gnarl_langs.append('C')
                    if 'Asm_Cpp' not in gnarl_langs and self.bsp.has_asm(d):
                        gnarl_langs.append('Asm_Cpp')

        # Now install the rts-specific sources
        for dirname, l in self.dirs.items():
            subdir = os.path.join(destination, dirname)

            if dirname not in src_dirs:
                if l and len(l) > 0:
                    src_dirs.append(dirname)
                    if 'gnarl' in dirname:
                        gnarl_dirs.append(dirname)
                        if 'C' not in gnarl_langs and \
                           dirname in self.c_srcs:
                            gnarl_langs.append('C')
                        if 'Asm_Cpp' not in gnarl_langs and \
                           dirname in self.asm_srcs:
                            gnarl_langs.append('Asm_Cpp')
                    else:
                        gnat_dirs.append(dirname)
                        if 'C' not in gnat_langs and \
                           dirname in self.c_srcs:
                            gnat_langs.append('C')
                        if 'Asm_Cpp' not in gnat_langs and \
                           dirname in self.asm_srcs:
                            gnat_langs.append('Asm_Cpp')

                    if not os.path.exists(subdir):
                        os.makedirs(subdir)

                for srcname, pair in l.items():
                    self._copy_pair(srcname, pair, subdir, installed_files)

        for d in ['obj', 'adalib']:
            os.mkdir(os.path.join(destination, d))

        # Generate ada_source_path
        with open(os.path.join(destination, 'ada_source_path'), 'w') as fp:
            for d in sorted(src_dirs):
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
        cnt = readfile(fullpath('src/runtime_build.gpr'))
        # Set source_dirs and languages
        self.build_flags['source_dirs'] = '",\n    "'.join(sorted(gnat_dirs))
        self.build_flags['langs'] = '", "'.join(gnat_langs)
        # Flags array are converted to a string
        for f in ['common_flags', 'asm_flags', 'c_flags']:
            self.build_flags[f] = '",\n        "'.join(self.build_flags[f])
        self.build_flags['target'] = self.target
        # Format
        cnt = cnt.format(**self.build_flags)
        # Write
        fp = open(os.path.join(destination, 'runtime_build.gpr'), 'w')
        fp.write(cnt)
        fp.close()

        if len(gnarl_dirs) > 0:
            cnt = readfile(fullpath('src/ravenscar_build.gpr'))
            # Set source_dirs and languages
            self.build_flags['source_dirs'] = '",\n    "'.join(
                sorted(gnarl_dirs))
            self.build_flags['langs'] = '", "'.join(gnarl_langs)
            # Format
            cnt = cnt.format(**self.build_flags)
            # Write
            fp = open(os.path.join(destination, 'ravenscar_build.gpr'), 'w')
            fp.write(cnt)
            fp.close()


class DFBBTarget(Target):
    "BB target with single and double FPU"

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
