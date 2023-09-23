#
# Copyright (C) 2016-2020, AdaCore
#
# Python script to gather files for the bareboard runtime.
# Don't use any fancy features.  Ideally, this script should work with any
# Python version starting from 2.6 (yes, it's very old but that's the system
# python on oldest host).

from support import is_string
from support.files_holder import FilesHolder

import os
from copy import deepcopy
from json import dumps


class Rule(object):
    # Collect some statistics on scenario variable usage, to better generate
    # the project file (most used scenario at the top-level of nested case
    # statements)
    __used_scenarios = {}

    def __init__(self, rules, scenarios, as_new_rule=False):
        """Create a new scenario variable condition rule.

        scenarios: check the rules against a list of scenario variables and
            accepted values. If None then no check is performed.
        as_new_rule: set to True if it's a rule added to the project file. This
            increases the initial counter of used scenario variables"""
        self._scenarios = {}
        self.invalid = False

        if rules is None or len(rules) == 0:
            return

        for rule in rules:
            assert ':' in rule, "Syntax error: wrong rule '%s'" % rule

            var, value = rule.split(':')
            var = var.strip()
            value = value.strip()

            assert len(var) != 0, "Syntax error: wrong rule '%s'" % rule
            assert len(value) != 0, "Syntax error: wrong rule '%s'" % rule
            assert var in scenarios, "Unknown scenario variable %s" % var

            # make sure to record all scenario variables that are actually
            # useful
            cases = [s.strip() for s in value.split(',')]

            if cases[0][0] == '!':
                negate = True
                n_cases = []
                for case in cases:
                    assert case.startswith('!'), \
                        ('negation needs to apply '
                         'to every item in the list: %s' % rule)
                    n_cases.append(case[1:])
                cases = n_cases
            else:
                negate = False

            assert var not in self._scenarios, \
                "duplicated scenario variable in %s" % str(rules)

            self._scenarios[var] = []

            if negate:
                self._scenarios[var] = scenarios[var][:]
            else:
                self._scenarios[var] = []

            # parse the rule
            for case in cases:
                # filter out values that are not expected
                if scenarios is None or case in scenarios[var]:
                    if negate:
                        self._scenarios[var].remove(case)
                    else:
                        self._scenarios[var].append(case)

            # ensure the possible values is not empty
            if len(self._scenarios[var]) == 0:
                # clear everything: it's a rule that can never match
                self._scenarios = {}
                self.invalid = True
                break

        if as_new_rule:
            # Update the list of used scenario variables
            for sv in self._scenarios.keys():
                if sv not in Rule.__used_scenarios:
                    Rule.__used_scenarios[sv] = 1
                else:
                    Rule.__used_scenarios[sv] += 1

    @property
    def is_empty(self):
        return len(self._scenarios) == 0

    @property
    def used_scenarios(self):
        return self._scenarios.keys()

    def has_scenario(self, var):
        return var in self._scenarios.keys()

    @staticmethod
    def count_scenario(var):
        if var in Rule.__used_scenarios:
            return Rule.__used_scenarios[var]
        else:
            return 0

    def matches(self, variables, exact=False):
        """Considering a set of variables, returns true if the rules match"""
        if self.invalid:
            return False
        for var in self._scenarios:
            if var not in variables:
                return False
            if variables[var] not in self._scenarios[var]:
                # not an expected value
                return False
        if exact:
            for var in variables:
                if var not in self._scenarios:
                    # some extra variable is defined. We wanted a full match so
                    # let's skip
                    return False
        return True

    def partial_match(self, variables):
        """If all variables match the rule (but not necessarily all the rules),
         then return True"""
        for var in variables:
            if var not in self._scenarios:
                # some extra variable is defined. We wanted a full match so
                # let's skip
                return False
            if variables[var] not in self._scenarios[var]:
                return False
        return True

    def corresponding_scenario(self):
        ret = {}
        for var in self._scenarios:
            assert len(self._scenarios[var]) == 1,\
                ("Cannot generate automatically a dependency,"
                 " when several choices are possible: %s:%s" % (
                    var, str(self._scenarios[var])))
            ret[var] = self._scenarios[var][0]
        return ret


# Definitions of shared source files.

class SourceTree(FilesHolder):
    def __init__(self, is_bb, profile, rts_sources, rts_scenarios):
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
        self.scenarios = deepcopy(rts_scenarios)
        self.lib_scenarios = {'gnat': [], 'gnarl': []}
        self.rules = {'gnat': {}, 'gnarl': {}}
        self.deps = {}
        SourceTree.__singleton = self

        if profile != 'ravenscar-full':
            if profile == 'zfp':
                self.scenarios['RTS_Profile'] = ['zfp']
            else:
                self.scenarios['RTS_Profile'] = ['zfp', 'ravenscar-sfp']

        for key, values in rts_sources.items():
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
                if 'requires' in values:
                    self.deps[key] = Rule(
                        values['requires'], self.scenarios, False)
                self.add_sources(key, srcs)
        # Sort the scenario variables from most used to less used
        self.lib_scenarios['gnat'] = sorted(
            self.lib_scenarios['gnat'],
            key=lambda x: Rule.count_scenario(x),
            reverse=True)
        self.lib_scenarios['gnarl'] = sorted(
            self.lib_scenarios['gnarl'],
            key=lambda x: Rule.count_scenario(x),
            reverse=True)

    def update_pairs(self, dir, pairs):
        # overload the parent method: this one allows updating pairs in a
        # specific directory. This is needed in the shared sources case as
        # it is expected to have different version of the same source in
        # different sub-directories
        for k, v in pairs.items():
            if k not in self.dirs[dir]:
                print("in update_pairs: no such source: %s" % k)
            else:
                self.dirs[dir][k] = v
        return True

    def add_rule(self, directory, rules):
        """parses the rules defined in 'rules', applicable to the folder 'dir'

        This stores the rules internally for further processing.

        :type directory: string
        :type rules: list or None
        """
        if is_string(rules):
            rules = [rules]

        if directory.split('/')[0] == 'gnarl':
            collection = self.rules['gnarl']
            used_scenarios = self.lib_scenarios['gnarl']
        else:
            collection = self.rules['gnat']
            used_scenarios = self.lib_scenarios['gnat']
        assert directory not in collection, \
            "directory %s defined twice" % directory

        # add the rule object to the current set of rules
        rule = Rule(rules, scenarios=self.scenarios, as_new_rule=True)

        collection[directory] = rule

        # and make sure to update the list of used scenario variables
        for sc in rule.used_scenarios:
            if sc not in used_scenarios:
                used_scenarios.append(sc)

    def install_tree(self, dest_json, dest_sources):
        """Install the runtime sources"""
        # Dump the json file describing the sources
        self.dump_json(dest_json, dest_sources)

        # now install the rts sources
        if not os.path.exists(dest_sources):
            os.makedirs(dest_sources)
        dirs = []
        dirs += self.rules['gnat'].keys()
        dirs += self.rules['gnarl'].keys()
        for d in dirs:
            self.__install_dir(d, dest_sources)

    def dump_json(self, path, dest_sources):
        cnt = {}
        for lib in ('gnat', 'gnarl'):
            if len(self.rules[lib]) == 0:
                continue
            cnt[lib] = {}
            lib_cnt = cnt[lib]
            lib_cnt['scenarios'] = {}

            for name in sorted(self.lib_scenarios[lib]):
                values = self.scenarios[name]
                lib_cnt['scenarios'][name] = values
            lib_cnt['sources'] = self.dump_sources_json(
                dest_sources,
                os.path.dirname(path),
                libname=lib,
                scenarios=deepcopy(self.lib_scenarios[lib]),
                dirs=deepcopy(self.rules[lib]),
                env={})

        with open(path, 'w') as fp:
            fp.write(dumps(cnt, indent=2, sort_keys=True))

    def dump_sources_json(self, dest_sources, dest_json,
                          libname, scenarios, dirs, env):
        if len(dirs) == 0:
            return None

        ret = {}
        relpath = os.path.relpath(dest_sources, dest_json)

        # First dump all directories that match the environment
        matched = []
        for d, rule in dirs.items():
            if rule.matches(env, exact=True):
                matched.append(d)

        if len(matched) > 0:
            ret['_srcs'] = ['%s/%s' % (relpath, m) for m in matched]

        if len(scenarios) == 0:
            return ret

        # now prune all dirs that cannot match anymore, due to the current
        # environment
        pruned = {}
        for d, rule in dirs.items():
            if not rule.partial_match(env):
                pruned[d] = rule
        for d in matched:
            pruned[d] = dirs[d]
        for d in pruned:
            del(dirs[d])

        if len(dirs) == 0:
            # restore the pruned items
            for d, rule in pruned.items():
                dirs[d] = rule

            return ret

        # Now look at the next scenario variable to see if some new directory
        # matches one of the values
        for j in range(0, len(scenarios)):
            next_var = scenarios[j]
            used = False
            for d, rule in dirs.items():
                if rule.has_scenario(next_var):
                    used = True
            if not used:
                continue

            for value in self.scenarios[next_var]:
                env[next_var] = value
                subret = self.dump_sources_json(
                    dest_sources, dest_json,
                    libname, scenarios[j + 1:], dirs, env)
                if subret is not None and len(subret) > 0:
                    ret["%s:%s" % (next_var, value)] = subret

            # remove variable from env, before moving to the next one
            del(env[next_var])

        # restore the pruned items
        for d, rule in pruned.items():
            dirs[d] = rule

        return ret

    def __install_dir(self, dirname, dest_sources):
        if dirname not in self.dirs:
            print('undefined shared directory %s' % dirname)

        destdir = os.path.join(dest_sources, dirname)

        if not os.path.exists(destdir):
            os.makedirs(destdir)

        for pair in self.dirs[dirname]:
            pair.install(destdir)
