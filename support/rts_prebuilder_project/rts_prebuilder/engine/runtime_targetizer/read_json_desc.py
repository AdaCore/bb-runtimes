#
# Copyright (C) 2025-2026, AdaCore
#

"""
Reads the rts-source.json descriptor and provides access to the sources
and scenarios tree.

TODO Consider to merge some of the logic of this module with write_json_desc.py
     (The lower abstractions should be the same)
"""

import json
import os
from typing import Iterator, Tuple

from rts_prebuilder.base_types import (
    AllPossibleScenarioConfigsType,
    GnatOrGnarlType,
    RTSDescriptorType,
    ScenarioKeyType,
    ScenarioLeafType,
    ScenariosConfigurationType,
    ScenarioValueType,
)


class SourcesTreeBase(object):
    """
    Locates the runtime source files given the rts-source.json decriptor
    """

    _pwd: str
    """
    Directory where the base json descriptor is located

    It is used as the base directory to locate the source files, since the paths
    of the sources are always relative to the json descriptor location. Hence the
    pwd name.

    TODO Consider typing this as Path from pathlib (same for other path strings
    in this class)
    """

    descriptor_content: RTSDescriptorType
    """
    Dict with the content of the JSON descriptor
    """

    base: str

    def __init__(self, json_file: str) -> None:
        """
        Loads cnt from a given json file

        :param json_file: JSON file to load
        """
        self._pwd = os.path.dirname(json_file)
        self._json_file = json_file
        with open(json_file, "r") as fp:
            content = fp.read()
        self.descriptor_content = json.loads(content)

    @property
    def descriptor_dir(self) -> str:
        """
        Returns the location of the rts-sources.json
        """
        return self._pwd

    def scenarios(self, lib: GnatOrGnarlType) -> AllPossibleScenarioConfigsType:
        """
        Returns all possible scenarios and their acceptable values for
        the given lib

        :param lib: "gnat" or "gnarl" library name
        """
        if lib not in self.descriptor_content:
            raise ValueError(
                f"The runtime sources don't provide support for lib{lib}"
                f" (Based on JSON descriptor at {os.path.abspath(self._json_file)})"
            )
        return self.descriptor_content[lib]["scenarios"]

    def sources(self, lib: GnatOrGnarlType) -> "SourcesTreeBranch":
        """
        Retuns an iterator over the tree of sources

        :param lib: "gnat" or "gnarl" library name
        """
        if lib not in self.descriptor_content:
            raise ValueError(
                f"The runtime sources don't provide support for lib{lib}"
                f" (Based on JSON descriptor at {os.path.abspath(self._json_file)})"
            )
        return self.SourcesTreeBranch(
            self.descriptor_content[lib]["sources"], self._pwd
        )

    def check_if_libs_present(self, *libs: GnatOrGnarlType) -> None:
        """
        Check that all given libs are present in the descriptor

        :param libs: List of libs to check
        """
        for lib in libs:
            if lib not in self.descriptor_content:
                raise ValueError(
                    f"The runtime sources don't provide support for lib{lib}"
                )

    class SourcesTreeBranch:
        """
        Iterator object iterating of the tree of ScenarioLeafType
        within the RTS descriptor, refer to RTSDescriptorType
        to understand this tree format.
        """

        cnt: ScenarioLeafType
        """
        Moving reference over the tree
        """

        base: str
        """
        Base path of the lib (Example: ${install_dir}/gnat or ${install_dir}/gnarl)
        """

        def __init__(self, raw_data: ScenarioLeafType, base: str) -> None:
            self.cnt = raw_data
            self.base = base

        @property
        def sources_paths(self) -> list[str]:
            """
            Return the list of normalized paths for the sources for
            currently references Leaf.
            """

            if "_srcs" in self.cnt:
                return [
                    os.path.normpath(os.path.join(self.base, d))
                    for d in self.cnt["_srcs"]
                ]
            else:
                return []

        def __iter__(
            self,
        ) -> Iterator[
            Tuple[
                ScenarioKeyType, ScenarioValueType, "SourcesTreeBase.SourcesTreeBranch"
            ]
        ]:
            """
            Iterate by moving in the Leaf within the current Leaf if it exists.
            """
            for k, v in self.cnt.items():
                # k: DefinedScenarioConfigurationStringType
                # v: ScenarioLeafType

                if k == "_srcs":
                    continue
                scenario, condition = k.split(":")
                yield (
                    scenario,
                    condition,
                    SourcesTreeBase.SourcesTreeBranch(v, self.base),  # type: ignore[arg-type]
                )
                # (TODO remove the static typing ignore when ScenarioLeafType is correctly
                # defined as recursive class)


def get_sources_paths_from_scenario_config(
    rts_source_branch: SourcesTreeBase.SourcesTreeBranch,
    scenarios: ScenariosConfigurationType,
) -> list[str]:
    """Explore the tree and fill list for sources paths that match the given
    scenario configuration.
    """
    sources_list: list[str] = rts_source_branch.sources_paths
    for scenario, condition, sub in rts_source_branch:
        # scenario: ScenarioKeyType
        # condition: ScenarioValueType
        # sub: SourcesTreeBase.SourcesTreeBranch

        if scenario in scenarios and scenarios[scenario] == condition:
            sources_list += get_sources_paths_from_scenario_config(sub, scenarios)

    return sources_list
