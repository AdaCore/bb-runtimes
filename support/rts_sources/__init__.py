#
# Copyright (C) 2016-2018, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support.files_holder import FilesHolder
from sources import scenarii, sources

import os
from copy import deepcopy


# Definitions of shared source files.

class SourceTree(FilesHolder):
    dest_sources = None
    dest_prjs = None

    def __init__(self, is_bb, profile):
        """This initializes the framework to generate the runtime source tree.

        is_bb: whether we're generating a bare metal hierarchy or a PikeOS one
        profile: the most complete profile used by the target. e.g.
         'ravenscar-full' will generate a full tree while 'zfp' will only
         consider the sources appropriate for zfp, and 'ravenscar-sfp' will
         consider the sources for both zfp and ravenscar-sfp, but will not
         add the files that are ravenscar-full specific.
        """
        super(SourceTree, self).__init__()
        self._is_bb = is_bb
        self.scenarii = deepcopy(scenarii)
        self.lib_scenarii = {'gnat': [], 'gnarl': []}
        self.rules = {'gnat': {}, 'gnarl': {}}

        if profile != 'ravenscar-full':
            if profile == 'zfp':
                self.scenarii['RTS_Profile'] = ['zfp']
            else:
                self.scenarii['RTS_Profile'] = ['zfp', 'ravenscar-sfp']

        for key, values in sources.iteritems():
            # filter out folders that are not used by the selected profiles
            if profile == 'zfp':
                if 'gnarl' in key.split('/'):
                    continue
            if profile in ('zfp', 'ravenscar-sfp'):
                if 'full' in key.split('/'):
                    continue
                if key == 'containers':
                    continue

            if 'srcs' in values:
                srcs = values['srcs']
            else:
                srcs = []
            if self._is_bb:
                if 'bb_srcs' in values:
                    srcs += values['bb_srcs']
            else:
                if 'pikeos_srcs' in values:
                    srcs += values['pikeos_srcs']
            if len(srcs) > 0:
                if 'conditions' not in values:
                    self.add_rule(key, None)
                else:
                    self.add_rule(key, values['conditions'])
                self.add_sources(key, srcs)

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

    def add_rule(self, dir, rules):
        """parses the rules defined in 'rules', applicable to the folder 'dir'

        This stores the rules internally for further processing
        """
        if isinstance(rules, basestring):
            self.add_rule(dir, [rules])
            return

        if dir.split('/')[0] == 'gnarl':
            collection = self.rules['gnarl']
            scenarii = self.lib_scenarii['gnarl']
        else:
            collection = self.rules['gnat']
            scenarii = self.lib_scenarii['gnat']
        self.__add_rule(collection, dir, rules, scenarii)

    def __add_rule(self, collection, dir, rules, used_scenarii):
        """actually adds a directory selection rule based on scenario variables

        collection: dictionary holding the rules
        dir: the selected directory
        rules: the string to parse
        used_scenarii: a list of scenario variables actually used.

        The tricky part here is that in order to generate a user-readable
        project file, we're reverting the logic here: the rts sources are
        declared with applicable rules, while in the generated project we
        start from the scenario variables to include the directories.

        As an example:
        'full': 'RTS_Profile:ravenscar-full'
        'containers': 'RTS_Profile:ravenscar-full'

        will generate:
        case RTS_Profile is
           when "ravenscar_full" =>
              GNAT_Dirs := GNAT_Dirs & ("full", "containers");

        To allow the magic, we use the following structure for the
        collection variable:

        { '__dirs__': ['list', 'of', 'always', 'used', 'dirs'],
          'scenario1': {
              'value1': {
                '__dirs__': ['applicable', 'dirs', 'when', 'scenario1=value1']
                'scenario1.1': {
                   # dirs that require a specific value for scenario 1 & 2
                   '__dirs__': ...
              },
              'value2': {
                 # similar to above
              },
            },
          'scenario2': ...
        }
        """
        # Check if the directory is always used
        if rules is None or len(rules) == 0:
            if '__dirs__' not in collection:
                collection['__dirs__'] = []
            collection['__dirs__'].append(dir)
            return

        # check for an already existing rule in collection
        found = False
        for r in rules:
            # Separate scenario variable and the cases where this rule applies
            var, cases = r.split(':')
            assert var in self.scenarii, "Unknown scenario variable: %s" % var

            if var in collection:
                found = True
                rule = r
                break
        if not found:
            # no rule at collection's top-level, so we select the first rule
            rule = rules[0]

        var, cases = rule.split(':')
        rules = rules[:]
        rules.remove(rule)

        # make sure to record all scenario variables that are actually useful
        if var not in used_scenarii:
            used_scenarii.append(var)
        cases = cases.split(',')

        # Make sure that the scenario variable is in the collection of rules
        if var not in collection:
            collection[var] = {}
            for case in self.scenarii[var]:
                collection[var][case] = {'__dirs__': []}

        # parse the rule
        for case in cases:
            negate = False
            if case.startswith('!'):
                negate = True
                case = case[1:]
            if negate:
                for other_case in self.scenarii[var]:
                    if case != other_case:
                        if other_case not in collection[var]:
                            collection[var][other_case] = {}
                        self.__add_rule(
                            collection[var][other_case],
                            dir, rules, used_scenarii)
            elif case in self.scenarii[var]:
                if case not in collection[var]:
                    collection[var][case] = {}
                self.__add_rule(collection[var][case],
                                dir, rules, used_scenarii)

    def install(self):
        # Dump the shared rts sources project file
        self.dump_project_files()

        # now install the rts sources
        if not os.path.exists(self.dest_sources):
            os.makedirs(self.dest_sources)
        dirs = []
        self.__gather_used_dirs(self.rules['gnat'], dirs)
        self.__gather_used_dirs(self.rules['gnarl'], dirs)
        for d in dirs:
            installed = []
            self.__install_dir(d, installed)

    def dump_project_files(self):
        for lib in ('gnat', 'gnarl'):
            if len(self.rules[lib]) == 0:
                continue

            lib_camelcase = 'Gnat' if lib == 'gnat' else 'Gnarl'

            ret = 'abstract project Lib%s_Sources is\n' % lib
            ret += '\n'
            ret += '   %s_Dirs := ();\n' % lib_camelcase
            ret += '   %s_Langs := ("Ada");\n' % lib_camelcase
            ret += '    \n'

            for name in sorted(self.lib_scenarii[lib]):
                values = self.scenarii[name]
                ret += '   type %s_Type is ("%s", "undefined");\n' % (
                    name, '", "'.join(values))
                ret += '   %s : %s_Type := external ("%s", "undefined");\n' % (
                    name, name, name)
                ret += '\n'

            ret += self.__dump_scenario(lib_camelcase, self.rules[lib], 1)
            ret += "end Lib%s_Sources;\n" % lib

            fname = os.path.join(self.dest_prjs, 'lib%s_sources.gpr' % lib)
            with open(fname, 'w') as fp:
                fp.write(ret)

    def __dump_scenario(self, var, current, indent):
        """Recursively dumps a case statement on scenario variables.

        This adds the directories defined when a scenario variable is set to
        a specific value, according to the conditions defined in sources.py
        """
        blank = ' ' * (3 * indent)
        ret = ''
        relpath = os.path.relpath(self.dest_sources, self.dest_prjs)
        if len(current['__dirs__']) > 0:
            ret += blank + '%s_Dirs := %s_Dirs &\n' % (var, var)
            strings = ['Project\'Project_dir & "%s/%s"' % (relpath, d)
                       for d in current['__dirs__']]
            ret += blank + '  ('
            ret += (',\n' + blank + '   ').join(strings)
            ret += ');\n'
            langs = []
            for d in sorted(current['__dirs__']):
                if 'C' not in langs and d in self.c_srcs:
                    langs.append('C')
                if 'Asm' not in langs and d in self.asm_srcs:
                    langs.append('Asm')
                if 'Asm_Cpp' not in langs and d in self.asm_cpp_srcs:
                    langs.append('Asm_Cpp')
            if len(langs) > 0:
                ret += blank + '%s_Langs := %s_Langs & ("%s");\n' % (
                    var, var, '", "'.join(langs))

        if len(current) == 1:
            return ret

        for key in sorted(current.keys()):
            if key == '__dirs__':
                continue
            ret += blank + 'case %s is\n' % key
            has_empty = False
            for val in sorted(self.scenarii[key]):
                if len(current[key][val]) == 1 and \
                        len(current[key][val]['__dirs__']) == 0:
                    has_empty = True
                else:
                    ret += blank + '   when "%s" =>\n' % val
                    ret += self.__dump_scenario(
                        var, current[key][val], indent + 2)
            if has_empty:
                ret += blank + '   when others =>\n'
            else:
                ret += blank + '   when "undefined" =>\n'
            ret += blank + 'end case;\n\n'
        return ret

    def __gather_used_dirs(self, src_prj, dirs):
        for name in src_prj:
            if name == '__dirs__':
                for d in src_prj[name]:
                    if d not in dirs:
                        dirs.append(d)
            else:
                for val in src_prj[name]:
                    self.__gather_used_dirs(src_prj[name][val], dirs)

    def __install_dir(self, dirname, installed_files):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)

        destdir = os.path.join(self.dest_sources, dirname)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for k, v in self.dirs[dirname].items():
            self._copy_pair(dst=k, srcfile=v, destdir=destdir,
                            installed_files=installed_files)
