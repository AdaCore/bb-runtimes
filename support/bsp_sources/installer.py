from target import Target
from support import readfile, datapath
from support.files_holder import FilesHolder

import os


class Installer(object):
    """Responsible for generating the BSP source tree and the RTS project"""

    def __init__(self, target):
        assert isinstance(target, Target), "invalid target argument"
        self.tgt = target

    #########################
    # dump_rts_project_file #
    #########################

    def dump_rts_project_file(self, rts_base_name, rts, destination,
                              rts_prefix):
        """Dumps the main project used to build the runtime"""
        rtsname = '%s-%s' % (rts_base_name, self.tgt.name)
        prj = '%s.gpr' % rtsname.replace('-', '_')
        prjname = rtsname.replace('-', '_').title()
        prj = os.path.join(destination, prj)

        base = os.path.dirname(rts_prefix)
        ret = 'aggregate project %s is\n' % prjname
        ret += '\n'
        ret += '   Base_BSP_Source_Dir   := Project\'Project_Dir & "%s";\n' % \
               self.tgt.rel_path
        ret += '   Base_Installation_Dir := "%s/";\n' % base
        if not self.tgt.is_pikeos and self.tgt.target is not None:
            board = os.path.basename(rts_prefix).replace(
                '%s-' % rts_base_name, '')
            ret += '   Board                 := "%s";\n' % board
            ret += '   Default_Prefix        := \n'
            ret += '     Base_Installation_Dir & "%s-" & Board;\n' % \
                   rts_base_name
        else:
            ret += '   Default_Prefix        := \n'
            ret += '     Base_Installation_Dir & "%s";\n' % \
                   os.path.basename(rts_prefix)
        ret += '   Install_Dir           := ' \
               'external ("PREFIX", Default_Prefix);\n'
        ret += '\n'
        for key in sorted(rts.keys()):
            ret += '   for external ("%s") use "%s";\n' % (key, rts[key])
        ret += '\n'
        ret += '   for external ("INSTALL_PREFIX") use Install_Dir;\n'
        ret += '\n'

        if self.tgt.target is not None:
            ret += '   for Target use "%s";\n' % self.tgt.target
        ret += '   for Runtime ("Ada") use Base_BSP_Source_Dir &\n'
        ret += '       "%s";\n' % rts_base_name
        ret += '\n'
        ret += '   for Project_Path use\n'
        ret += '     (Base_BSP_Source_Dir & "%s",\n' % rts_base_name
        ret += '      "../lib/gnat");\n'  # pick up the local rts srcs if any
        ret += '   for Project_Files use\n'
        if 'ravenscar' in rts['RTS_Profile']:
            ret += '     (Base_BSP_Source_Dir & "%s/libgnat.gpr",\n' % \
                   rts_base_name
            ret += '      Base_BSP_Source_Dir & "%s/libgnarl.gpr",\n' % \
                   rts_base_name
        else:
            ret += '     (Base_BSP_Source_Dir & "%s/libgnat.gpr",\n' % \
                   rts_base_name
        ret += '      Base_BSP_Source_Dir & "%s/install.gpr");\n' % \
               rts_base_name
        ret += '\n'
        ret += 'end %s;\n' % prjname

        with open(prj, 'w') as fp:
            fp.write(ret)

    def install(self, destination, prefix):
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
        base_bsp = os.path.join(base, self.tgt.rel_path)

        if 'README' in self.tgt.config_files:
            cnt = self.tgt.config_files['README']
            readme_fname = os.path.join(
                destination, 'README-%s.txt' % self.tgt.name)
            with open(readme_fname, 'w') as fp:
                fp.write(cnt)

        scripts = []
        self.tgt.install_ld_scripts(
            destination, scripts, installed_files)

        for d in scripts:
            full = os.path.join(base, d)
            rel = os.path.join('..', os.path.relpath(full, base_bsp))
            script_files.append(rel)

        # Install source files for the BSP/RTSs
        bsp_gnat = []
        bsp_gnarl = []

        self.tgt.install_libgnat(
            destination, bsp_gnat, installed_files)

        has_ravenscar = False
        for rts in self.tgt.runtimes:
            if 'ravenscar' in rts:
                has_ravenscar = True
                break
        if has_ravenscar:
            # install ravenscar support
            self.tgt.install_libgnarl(
                destination, bsp_gnarl, installed_files)

        for d in bsp_gnat:
            full = os.path.join(base, d)
            rel = os.path.join('..', os.path.relpath(full, base_bsp))
            # gnat_dirs is used to generate libgnat.gpr, so relative to the
            # bsp directory
            gnat_dirs.append(rel)
            if 'C' not in gnat_langs and self.tgt.has_c(d):
                gnat_langs.append('C')
            if 'Asm' not in gnat_langs and self.tgt.has_asm(d):
                gnat_langs.append('Asm')
            if 'Asm_Cpp' not in gnat_langs and self.tgt.has_asm_cpp(d):
                gnat_langs.append('Asm_Cpp')

        for d in bsp_gnarl:
            full = os.path.join(base, d)
            rel = os.path.join('..', os.path.relpath(full, base_bsp))
            gnarl_dirs.append(rel)
            if 'C' not in gnarl_langs and self.tgt.has_c(d):
                gnarl_langs.append('C')
            if 'Asm' not in gnarl_langs and self.tgt.has_asm(d):
                gnarl_langs.append('Asm')
            if 'Asm_Cpp' not in gnarl_langs and self.tgt.has_asm_cpp(d):
                gnarl_langs.append('Asm_Cpp')

        # Now install the rts-specific sources
        for rts_name, rts_obj in self.tgt.runtimes.items():
            base_rts = os.path.join(base_bsp, rts_name)
            rts_gnat = [d for d in gnat_dirs]
            rts_gnarl = [d for d in gnarl_dirs]
            rts_gnat_langs = [l for l in gnat_langs]
            rts_gnarl_langs = [l for l in gnarl_langs]

            if prefix is not None:
                if prefix.endswith('/'):
                    install_prefix = prefix
                else:
                    install_prefix = prefix + '/'
            elif self.tgt.target is not None:
                if self.tgt.is_pikeos:
                    install_prefix = 'lib/gcc/%s/%s/' % (
                        self.tgt.target, FilesHolder.gcc_version())
                else:
                    install_prefix = self.tgt.target + '/lib/gnat/'
            else:
                install_prefix = 'lib/gnat/'
            if self.tgt.is_pikeos or self.tgt.target is None:
                install_prefix += 'rts-%s' % rts_name
            else:
                install_prefix += '%s-%s' % (rts_name, self.tgt.name)

            if not os.path.exists(base_rts):
                os.makedirs(base_rts)

            for d in ['obj', 'adalib']:
                path = os.path.join(base_rts, d)
                if not os.path.exists(path):
                    os.mkdir(path)

            for dirname, l in rts_obj.dirs.items():
                if l is None or len(l) == 0:
                    continue

                if 'gnarl' in dirname:
                    if dirname not in gnarl_dirs:
                        rts_gnarl.append(dirname)
                    if 'C' not in rts_gnarl_langs and \
                       dirname in rts_obj.c_srcs:
                        rts_gnarl_langs.append('C')
                    if 'Asm' not in rts_gnarl_langs and \
                       dirname in rts_obj.asm_srcs:
                        rts_gnarl_langs.append('Asm')
                    if 'Asm_Cpp' not in rts_gnarl_langs and \
                       dirname in rts_obj.asm_cpp_srcs:
                        rts_gnarl_langs.append('Asm_Cpp')
                else:
                    if dirname not in gnat_dirs:
                        rts_gnat.append(dirname)
                    if 'C' not in rts_gnat_langs and \
                       dirname in rts_obj.c_srcs:
                        rts_gnat_langs.append('C')
                    if 'Asm' not in rts_gnat_langs and \
                       dirname in rts_obj.asm_srcs:
                        rts_gnat_langs.append('Asm')
                    if 'Asm_Cpp' not in rts_gnat_langs and \
                       dirname in rts_obj.asm_cpp_srcs:
                        rts_gnat_langs.append('Asm_Cpp')

                full = os.path.join(base_rts, dirname)

                if not os.path.exists(full):
                    os.makedirs(full)

                for srcname, pair in l.items():
                    self.tgt._copy_pair(srcname, pair, full)

            # user-defined sources
            rts_gnat.append('user_srcs')
            path = os.path.join(base_rts, 'user_srcs')
            if not os.path.exists(path):
                os.mkdir(path)

            # Generate ada_source_path, used for the rts bootstrap
            with open(os.path.join(base_rts, 'ada_source_path'), 'w') as fp:
                for d in sorted(rts_gnat + rts_gnarl):
                    fp.write(d + '\n')

            # Generate ada_object_path
            with open(os.path.join(base_rts, 'ada_object_path'), 'w') as fp:
                fp.write('adalib\n')

            # Write config files
            for name, content in self.tgt.config_files.iteritems():
                with open(os.path.join(base_rts, name), 'w') as fp:
                    fp.write(content)
            with open(os.path.join(base_rts, 'runtime.xml'), 'w') as fp:
                fp.write(self.tgt.dump_runtime_xml(rts_name, rts_obj))

            # and now install the rts project with the proper scenario values
            self.dump_rts_project_file(
                rts_name, rts_obj.rts_vars, destination, install_prefix)

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
            cnt = readfile(datapath('install.gpr'))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(base_rts, 'install.gpr'), 'w') as fp:
                fp.write(cnt)

            # and the potentially runtime specific target_options.gpr project
            build_flags = {}
            for f in ['common_flags', 'asm_flags', 'c_flags']:
                build_flags[f] = '",\n        "'.join(rts_obj.build_flags[f])
            cnt = readfile(datapath('target_options.gpr'))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(base_rts, 'target_options.gpr'), 'w') as fp:
                fp.write(cnt)

            # Set source_dirs and languages
            prj_values = {}
            prj_values['gnat_source_dirs'] = '"%s"' % (
                '",\n      "'.join(sorted(rts_gnat)),)
            if len(rts_gnarl) == 0:
                prj_values['gnarl_source_dirs'] = ''
            else:
                prj_values['gnarl_source_dirs'] = '"%s"' % (
                    '",\n      "'.join(sorted(rts_gnarl)),)
            prj_values['gnat_langs'] = '", "'.join(["Ada"] + rts_gnat_langs)
            prj_values['gnarl_langs'] = '", "'.join(["Ada"] + rts_gnarl_langs)
            all_langs = []
            for l in rts_gnat_langs + rts_gnarl_langs:
                if l not in all_langs:
                    all_langs.append(l)
            prj_values['all_langs'] = '", "'.join(all_langs)

            if 'ravenscar' not in rts_name:
                projects = ('libgnat',)
            elif 'full' in rts_name:
                projects = ('libgnat_full', 'libgnarl_full')
            else:
                projects = ('libgnat', 'libgnarl')

            for fname in projects:
                cnt = readfile(datapath('%s.gpr' % fname))
                # Format
                cnt = cnt.format(**prj_values)
                # Write
                if '_full' in fname:
                    dest = fname.replace('_full', '')
                    empty_c = os.path.join(base_rts, 'empty.c')
                    with open(empty_c, 'w') as fp:
                        fp.write('')
                else:
                    dest = fname
                with open(os.path.join(base_rts, '%s.gpr' % dest), 'w') as fp:
                    fp.write(cnt)
