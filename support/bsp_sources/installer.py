from support import readfile, getdatafilepath
from support.bsp_sources.target import Target
from support.files_holder import _copy

import json
import os
import shutil
import subprocess
import sys


def copy_file(src, dest):
    if os.path.isdir(dest):
        dest = os.path.join(dest, os.path.basename(src))
    _copy(src, dest)


def install_files(flist, dest):
    if len(flist) == 0:
        return
    basenames = []
    if not os.path.isdir(dest):
        os.makedirs(dest)
    for item in flist:
        if os.path.isfile(item):
            copy_file(item, dest)
            basenames.append(os.path.basename(item))

        else:
            for fname in os.listdir(item):
                fsrc = os.path.join(item, fname)
                if os.path.isfile(fsrc):
                    copy_file(fsrc, dest)
                    basenames.append(fname)
    return basenames


class SharedRTSSources(object):
    def __init__(self, json_file):
        self._pwd = os.path.dirname(json_file)
        with open(json_file, 'r') as fp:
            cnt = fp.read()
        self.cnt = json.loads(cnt)

    @property
    def install_dir(self):
        return self._pwd

    def scenarios(self, lib):
        assert lib in self.cnt, \
            "The runtime sources don't provide support for lib%s" % lib
        return self.cnt[lib]['scenarios']

    def sources(self, lib):
        assert lib in self.cnt, \
            "The runtime sources don't provide support for lib%s" % lib
        return self.SharedSourcesItem(self.cnt[lib]['sources'], self._pwd)

    class SharedSourcesItem(object):
        def __init__(self, raw_data, base):
            self.cnt = raw_data
            self.base = base

        @property
        def source_dirs(self):
            if '_srcs' in self.cnt:
                return [os.path.normpath(os.path.join(self.base, d))
                        for d in self.cnt['_srcs']]
            else:
                return []

        def __iter__(self):
            for k, v in self.cnt.items():
                if k == '_srcs':
                    continue
                scenario, condition = k.split(':')
                yield (scenario, condition,
                       SharedRTSSources.SharedSourcesItem(v, self.base))


class Installer(object):
    """Responsible for generating the BSP source tree and the RTS project"""
    overwrite = False

    def __init__(self, target):
        assert isinstance(target, Target), "invalid target argument"
        self.tgt = target

    @property
    def is_native(self):
        return self.tgt.target is None

    def _find_rts_sources(self, destination, descriptor):
        """Find the runtime sources and the json file that describes them.
        """
        # First look in the relative path
        rts_json_file = 'rts-sources.json'

        ret = None
        if descriptor is not None:
            # First: take the one given on the command line if any
            ret = os.path.abspath(descriptor)
        else:
            # Next: try a relative path from the install directotry
            fname = os.path.join(destination, rts_json_file)
            if os.path.exists(fname):
                ret = os.path.normpath(fname)

        if ret is None:
            # Finally: Use gprls to retrieve gnat installation path and see
            # if we find the file somewhere in the project search path
            if not self.is_native:
                res = subprocess.check_output(
                    ['gprls', '-v', '--target=%s' % self.tgt.target],
                    stderr=subprocess.STDOUT).decode()
            else:
                res = subprocess.check_output(
                    ['gprls', '-v'],
                    stderr=subprocess.STDOUT).decode()
            in_prj_search_path = False
            ret = None
            for line in res.splitlines():
                if not in_prj_search_path:
                    if line == 'Project Search Path:':
                        in_prj_search_path = True
                    continue
                line = line.strip()
                if line == '<Current_Directory>':
                    continue
                if len(line) == 0:
                    break
                tentative = os.path.join(line, rts_json_file)
                if os.path.exists(tentative):
                    ret = os.path.normpath(tentative)
                    break
        assert ret is not None, "Cannot find %s" % rts_json_file
        return SharedRTSSources(ret)

    def _get_rts_dirs(self, rts_source_item, scenarios):
        """Recursively look for runtime source dirs to include in the runtime.

        This uses the scenario variables defined by the BSP to only use the
        proper folders in the runtime sources.
        """
        ret = rts_source_item.source_dirs
        for scenario, condition, sub in rts_source_item:
            if scenario in scenarios and \
                    scenarios[scenario] == condition:
                ret += self._get_rts_dirs(sub, scenarios)
        return ret

    def install(self, destination, rts_descriptor=None):
        # Build target directories
        destination = os.path.abspath(destination)
        if not os.path.exists(destination):
            os.mkdir(destination)

        # Retrieve runtime sources
        runtime_sources = self._find_rts_sources(destination, rts_descriptor)
        projects = []

        for rts_base_name, rts_obj in self.tgt.runtimes.items():
            if self.tgt.is_native or self.tgt.is_pikeos:
                rtsname = 'rts-%s' % rts_base_name
            else:
                rtsname = '%s-%s' % (rts_base_name, self.tgt.name)
            rts_path = os.path.join(destination, rtsname)
            if os.path.exists(rts_path):
                if not self.overwrite:
                    print("ERROR: a runtime already exists in")
                    print("  %s" % rts_path)
                    print("remove the runtime, use a different installation"
                          " path or use --force to overwrite")
                    sys.exit(1)
                else:
                    # remove everything there
                    print("WARNING: replacing a previously existing runtime")
                    print("  %s" % rts_path)
                    shutil.rmtree(rts_path)
            scenario_vars = rts_obj.rts_vars

            if 'ravenscar' in rts_base_name:
                libs = ('gnat', 'gnarl')
            else:
                libs = ('gnat', )

            # Amend the scenario variables with the default values
            for lib in libs:
                for scenario, vals in runtime_sources.scenarios(lib).items():
                    if scenario not in scenario_vars:
                        scenario_vars[scenario] = vals[0]

            # Placeholder for user-defined sources
            user_libs = ['%s_user' % d for d in libs]
            for lib in user_libs:
                dest = os.path.join(rts_path, lib)
                os.makedirs(dest)

            # GNARL extra directory for ravenscar-full:
            # With the ravenscar full, we can't split properly libgnat
            # and libgnarl, as we don't have the same soft-link
            # mechanism as the native runtime. This means that
            # atomic operations from libgnat need to call the
            # libgnarl functions, making the two libs inter-dependent.
            # To remove linking headache, we thus combine the two in
            # a single lib, keeping libgnarl as an empty lib (gnatlink will
            # still try to link with it when tasking is used, so we need to
            # have one available).
            if rts_base_name == 'ravenscar-full':
                dest = os.path.join(rts_path, 'gnarl_empty')
                os.makedirs(dest)
                with open(os.path.join(dest, 'empty.c'), 'w') as fp:
                    fp.write('\n')

            # Now copy the full set of sources to use for the runtime
            langs = {}
            for lib in libs:
                langs[lib] = ['Ada']
                dest = os.path.join(rts_path, lib)
                # Install sources from the shared rts sources
                dirs = self._get_rts_dirs(
                    runtime_sources.sources(lib), scenario_vars)
                install_files(dirs, dest)
                # and install sources from the BSP
                for pair in self.tgt.get_sources(lib):
                    pair.install(dest)
                if lib in rts_obj.dirs:
                    for pair in rts_obj.dirs[lib]:
                        pair.install(dest)
                # Check the list of languages used there, to produce the proper
                # _build.gpr project.
                for fname in os.listdir(dest):
                    _, ext = os.path.splitext(fname)
                    if 'C' not in langs[lib] and (ext == '.c' or ext == '.h'):
                        langs[lib].append('C')
                    if 'Asm' not in langs[lib] and ext == '.s':
                        langs[lib].append('Asm')
                    if 'Asm_Cpp' not in langs[lib] and ext == '.S':
                        langs[lib].append('Asm_Cpp')

            # Copy the ld scripts
            if len(self.tgt.ld_scripts) > 0:
                dest = os.path.join(rts_path, 'ld')
                if not os.path.isdir(dest):
                    os.makedirs(dest)
                for script in self.tgt.ld_scripts:
                    script.install(dest)
            # Add user-defined placeholder for ld scripts
            dest = os.path.join(rts_path, 'ld_user')
            if not os.path.isdir(dest):
                os.makedirs(dest)

            # Install target and run-time specific configuration files
            for name, content in self.tgt.config_files.items():
                with open(os.path.join(rts_path, name), 'w') as fp:
                    fp.write(content)
            for name, content in rts_obj.config_files.items():
                with open(os.path.join(rts_path, name), 'w') as fp:
                    fp.write(content)
            with open(os.path.join(rts_path, 'runtime.xml'), 'w') as fp:
                fp.write(self.tgt.dump_runtime_xml(rts_base_name, rts_obj))
            with open(os.path.join(rts_path, 'ada_source_path'), 'w') as fp:
                # Make sure the user-defined sources come first to preempt
                # default sources when needed
                fp.write('%s\n' % '\n'.join(list(user_libs) + list(libs)))
            with open(os.path.join(rts_path, 'ada_object_path'), 'w') as fp:
                fp.write('adalib\n')

            # And generate the project files used to build the rts

            build_flags = {}
            for f in ['common_flags', 'asm_flags', 'c_flags']:
                build_flags[f] = '",\n        "'.join(rts_obj.build_flags[f])
            cnt = readfile(getdatafilepath('target_options.gpr'))
            # Format
            cnt = cnt.format(**build_flags)
            # Write
            with open(os.path.join(rts_path, 'target_options.gpr'), 'w') as fp:
                fp.write(cnt)

            runtime_build = os.path.join(rts_path, "runtime_build.gpr")
            runtime_build_tmpl = getdatafilepath('runtime_build.gpr.in')
            with open(runtime_build_tmpl, 'r') as fp:
                template = fp.read()
            with open(runtime_build, 'w') as fp:
                if self.is_native:
                    target_directive = ''
                else:
                    target_directive = 'for Target use "%s";' % self.tgt.target
                source_dirs = ['gnat_user', 'gnat']
                languages = langs['gnat']
                if rts_base_name == 'ravenscar-full':
                    # ravenscar-full: combine libgnat and libgnarl
                    source_dirs.extend(['gnarl_user', 'gnarl'])
                    for lang in langs['gnarl']:
                        if lang not in languages:
                            languages.append(lang)
                fp.write(template.format(
                    target_directive=target_directive,
                    source_dirs='", "'.join(source_dirs),
                    languages='", "'.join(languages)))
            if 'gnarl' in libs:
                ravenscar_build = os.path.join(rts_path, "ravenscar_build.gpr")
                ravenscar_build_tmpl = getdatafilepath(
                    "ravenscar_build.gpr.in")
                with open(ravenscar_build_tmpl, 'r') as fp:
                    template = fp.read()
                if rts_base_name != 'ravenscar-full':
                    source_dirs = ['gnarl_user', 'gnarl']
                    languages = langs['gnarl']
                else:
                    # see above: libgnarl and libgnat are merged in
                    # ravenscar-full, and libgnarl remains there as an empty
                    # lib
                    source_dirs = ['gnarl_empty']
                    languages = ['C']
                with open(ravenscar_build, 'w') as fp:
                    fp.write(template.format(
                        source_dirs='", "'.join(source_dirs),
                        languages='", "'.join(languages)))
                projects.append(ravenscar_build)
            else:
                projects.append(runtime_build)

            # Finally install extra sources and projects if requested by the
            # target
            extra = self.tgt.other_sources(rts_base_name)
            if extra is not None:
                for subdir, src_list in extra.items():
                    dest = os.path.join(rts_path, subdir)
                    if not os.path.exists(dest):
                        os.makedirs(dest)
                    install_files(src_list, dest)
            extra_prjs = self.tgt.other_projects(rts_base_name)
            if extra_prjs is not None:
                projects += [os.path.join(rts_path, prj)
                             for prj in extra_prjs]

        return projects
