#
# Copyright (C) 2025-2026, AdaCore
#

import os
from copy import deepcopy
from json import dumps
from pathlib import Path

from rts_prebuilder.base_types import (
    AllPossibleScenarioConfigsType,
    GnatOrGnarlType,
    RTSDescriptorType,
    RTSLibDescType,
    ScenarioKeyType,
    ScenarioLeafType,
    ScenariosConfigurationType,
    ScenarioValueType,
    SourceFamilyIdType,
)

from ..common.rule import SourceFamilyToConditionsMap


def gen_and_dump_json(
    conditions_rules: dict[GnatOrGnarlType, SourceFamilyToConditionsMap],
    lib_scenarios: dict[GnatOrGnarlType, list[ScenarioKeyType]],
    path: Path,
    dest_sources: Path,
    acceptable_scenarios: AllPossibleScenarioConfigsType,
) -> None:
    """
    Generate the JSON representation of the runtime sources directory
    and dump it in <path>.

    :param rules: Same as TargetizedRuntime.rules
    :param lib_scenarios: Same as TargetizedRuntime.lib_scenarios
    :param path: The path to the destination JSON file to create
    :param dest_sources: The path to the destination sources directory,
                         needed only to re-adjust paths in the output JSON
                         relatively to it.
    :param acceptable_scenarios: Same as TargetizedRuntime.acceptable_scenarios
    """

    json_content: RTSDescriptorType = {}

    for lib in conditions_rules.keys():
        # lib: GnatOrGnarlType
        if len(conditions_rules[lib]) == 0:
            continue
        json_content[lib] = {"scenarios": {}, "sources": {}}
        lib_content: RTSLibDescType = json_content[lib]

        for name in sorted(lib_scenarios[lib]):
            # name: ScenarioKeyType
            values: list[ScenarioValueType] = acceptable_scenarios[name]
            lib_content["scenarios"][name] = values

        lib_content["sources"] = src_descriptor_recursive_generator(
            dest_sources,
            path.parent,
            libname=lib,
            scenarios=deepcopy(lib_scenarios[lib]),
            conditions_map=deepcopy(conditions_rules[lib]),
            env={},
            acceptable_scenarios=acceptable_scenarios,
        )

    with open(path, "w") as fp:
        fp.write(dumps(json_content, indent=2, sort_keys=True))


def src_descriptor_recursive_generator(
    dest_sources: Path,
    dest_json: Path,
    libname: GnatOrGnarlType,
    scenarios: list[ScenarioKeyType],
    conditions_map: SourceFamilyToConditionsMap,
    env: ScenariosConfigurationType,
    acceptable_scenarios: AllPossibleScenarioConfigsType,
) -> ScenarioLeafType:
    """
    This function prepares a dictionary that is then used to generate
    the RTSDescriptorType by analysing by Analysing the rules of each.
    It is called recursively.

    TODO This functions is too complex and should be simplified. Or
          moved into a separate class. it could be merged with SharedSourcesItem
          class recursive class.

    :param dest_sources: The path to the destination sources directory
    :param dest_json: The path to the destination JSON file
    :param libname: The name of the library (e.g., "gnat" or "gnarl")
    :param scenarios: Ordered list of scenarios used by all. See self.lib_scenarios[libname]
    :param conditions_map: A dict that maps a ConditionsRule object for each family
    :param env: TODO:3 This seems unused document or drop
    :param acceptable_scenarios: Subset of all valid scenario keys and their values.

    See ``AllPossibleScenarioConfigsType`` for the details of the structure.
    :return: The JSON representation of the sources or None if no sources are found
    """
    ret: ScenarioLeafType = {}

    if len(conditions_map) == 0:
        return ret

    relpath = os.path.relpath(dest_sources, dest_json)

    # First dump all directories that match the environment
    matched: list[SourceFamilyIdType] = []
    for family_id, rule in conditions_map.items():
        if rule.matches(env, exact=True):
            matched.append(family_id)

    if len(matched) > 0:
        ret["_srcs"] = [f"{relpath}/{m}" for m in matched]

    if len(scenarios) == 0:
        return ret

    # now prune all dirs that cannot match anymore, due to the current
    # environment
    pruned = {}
    for family_id, rule in conditions_map.items():
        if not rule.partial_match(env):
            pruned[family_id] = rule
    for family_id in matched:
        pruned[family_id] = conditions_map[family_id]
    for family_id in pruned:
        del conditions_map[family_id]

    if len(conditions_map) == 0:
        # restore the pruned items
        for family_id, rule in pruned.items():
            conditions_map[family_id] = rule

        return ret

    # Now look at the next scenario variable to see if some new directory
    # matches one of the values
    for j in range(0, len(scenarios)):
        next_var = scenarios[j]
        used = False
        for _, rule in conditions_map.items():
            if rule.has_scenario(next_var):
                used = True
        if not used:
            continue

        for value in acceptable_scenarios[next_var]:
            env[next_var] = value
            subret = src_descriptor_recursive_generator(
                dest_sources,
                dest_json,
                libname,
                scenarios[j + 1 :],
                conditions_map,
                env,
                acceptable_scenarios,
            )
            if subret is not None and len(subret) > 0:
                ret[f"{next_var}:{value}"] = subret

        # Remove variable from env, before moving to the next one
        del env[next_var]

    # restore the pruned items
    for family_id, rule in pruned.items():
        conditions_map[family_id] = rule

    return ret
