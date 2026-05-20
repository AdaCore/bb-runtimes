#
# Copyright (C) 2025-2026, AdaCore
#

from copy import deepcopy
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import SourceFile
from rts_prebuilder.base_types import (
    VALID_BASE_PROFILES_SET,
    VALID_PLATFORM_SPECIFIC_SOURCES_SET,
    AllPossibleScenarioConfigsType,
    GnatOrGnarlType,
    PlatformSpecificSourcesIdType,
    RtsSourcesDBType,
    RuleStringType,
    ScenarioKeyType,
    SourceFamilyIdType,
    ValidBaseProfileType,
)

from ..common.files_holder import FilesHolder
from ..common.infrastructure_interface import (
    get_all_possible_scenarios,
    get_all_sources_listing,
    get_resolver,
)
from ..common.rule import (
    ConditionsRule,
    RequirementsRule,
    SourceFamilyToConditionsMap,
    SourceFamilyToRequirementsMap,
)
from .write_json_desc import gen_and_dump_json


class RuntimeAssembler:
    """
    This class is responsible for processing the given profile and platform
    into an installable FilesHolder instance.
    It relies on different inputs/contexts to achieve this; Including a database
    of type RtsSourcesDBType.
    """

    _files_holder: FilesHolder
    """
    This is the FilesHolder instance that will be populated with the
    selected sources.
    """

    _acceptable_scenarios: AllPossibleScenarioConfigsType
    """
    Copy of all valid scenario keys and their values, but only for the selected profile.
    See ``AllPossibleScenarioConfigsType`` for the details of the structure.
    """

    _conditions_rules: dict[GnatOrGnarlType, SourceFamilyToConditionsMap]
    """
    This dict is used to save all conditions "ConditionsRule" object for each
    source families.
    """

    _lib_scenarios: dict[GnatOrGnarlType, list[ScenarioKeyType]]
    """
    All scenario keys usable with the sources in the TargetizedRuntime
    They are saved per library (gnat or gnal), and sorted by
    relevancy. Scenario keys affecting most source families are
    listed first.

    """

    _deps: SourceFamilyToRequirementsMap
    """
    This dict is used to save all dependency typed "Rule" object for each source families.
    This is deduced from the "requires" field of each family entry.
    """

    def __init__(
        self,
        platform: PlatformSpecificSourcesIdType,
        top_base_profile: ValidBaseProfileType,
    ) -> None:
        """
        This initializes the framework to assemble the runtime source tree.

        :param platform: Only sources targeting the given platform will be in the
                         assembled RTS source tree, see PlatformSpecificSourcesIdType
        :param top_base_profile: Only sources targeting this given profile (or lower) will be in
                                 the assembled RTS source tree.
                                 Example: if 'light-tasking', only sources for "light" and
                                 'light-tasking' are included in the assembled RTS source tree.
        """

        if top_base_profile not in VALID_BASE_PROFILES_SET:
            raise ValueError(f"Unexpected top base profile {top_base_profile}")

        if platform not in VALID_PLATFORM_SPECIFIC_SOURCES_SET:
            raise ValueError(f"Unexpected platform {platform}")

        super().__init__()

        self._files_holder = FilesHolder()
        self._deps = {}
        self._conditions_rules = {"gnat": {}, "gnarl": {}}
        self._lib_scenarios = {"gnat": [], "gnarl": []}
        self._top_base_profile = top_base_profile
        self._platform = platform

        # Deepcopy the scenarios because we will modify it based on the profile
        self._acceptable_scenarios = deepcopy(get_all_possible_scenarios())
        self._populate_sources_and_rules()

    def _populate_sources_and_rules(
        self,
    ) -> None:
        """
        Populate the FilesHolder instance with the sources matching the
        given platform and profile.
        Also populate the rules and lib_scenarios attributes.
        """

        # Get the sources listing database
        # See RtsSourcesDBType, all source families and their listings, with
        # conditions and requirements.
        rts_sources: RtsSourcesDBType = get_all_sources_listing()

        # Override the self.scenarios["RTS_Profile"] value based on the top_profile
        top_base_profile = self._top_base_profile
        if top_base_profile != "embedded":
            if top_base_profile == "light":
                self._acceptable_scenarios["RTS_Profile"] = ["light"]
            elif top_base_profile == "light-tasking":
                self._acceptable_scenarios["RTS_Profile"] = ["light", "light-tasking"]
            elif top_base_profile == "cert":
                self._acceptable_scenarios["RTS_Profile"] = ["light", "cert"]

        # Extract source families from rts_sources (common and platform specific sources)
        # Not all sources in rts_sources are processed, some are skipped given the profile.
        # The extracted sources are pushed into FilesHolder instance,
        # where each family of sources is pushed in a subdirectory with the name of the family.
        for family_id, family_entry in rts_sources.items():
            # family_id: SourceFamilyIdType
            # family_entry: SourcesFamilyEntryType

            # Skip folders that are not used by the selected profiles
            if top_base_profile == "light":
                if "gnarl" in family_id.split("/"):
                    continue
            if top_base_profile in ("light", "light-tasking"):
                if "full" in family_id.split("/"):
                    continue
                if family_id == "containers":
                    continue

            # Fill the srcs list
            #  - from the family_entry["srcs"] then from family_entry[self._platform]
            srcs = family_entry.get("srcs", []) + family_entry.get(self._platform, [])

            # If the filled srcs list is not empty
            # then the srcs will make it to the FilesHolder.
            # At this point, conditions and requires rules should be
            # from the current values are registered as well in the
            # rules/lib_scenario/deps dicts.
            if len(srcs) > 0:

                # Process conditions (or their lack of)
                # And fill self.rules then self.lib_scenarios
                if "conditions" not in family_entry:
                    self._add_conditions_rule(family_id=family_id, raw_conditions=None)
                else:
                    self._add_conditions_rule(
                        family_id=family_id, raw_conditions=family_entry["conditions"]
                    )

                # Process "requires" rules
                # And fill self.deps[key]
                if "requires" in family_entry:
                    self._deps[family_id] = RequirementsRule(
                        rules=family_entry["requires"],
                        all_acceptable_scenarios=self._acceptable_scenarios,
                    )

                # Convert to list of source files class
                sources: list[SourceFile] = [
                    SourceFile(unresolved_src_path=src, dest_subdir=Path(family_id))
                    for src in srcs
                ]

                # Push srcs to file holder
                # use the Family ID as the subdirectory
                self._files_holder.append_source_files(*sources)

        # Sort the scenario variables from most used to less used
        self._lib_scenarios["gnat"] = sorted(
            self._lib_scenarios["gnat"],
            key=ConditionsRule.scenario_global_stats,
            reverse=True,
        )
        self._lib_scenarios["gnarl"] = sorted(
            self._lib_scenarios["gnarl"],
            key=ConditionsRule.scenario_global_stats,
            reverse=True,
        )

    def _add_conditions_rule(
        self,
        family_id: SourceFamilyIdType,
        raw_conditions: list[RuleStringType] | None,
    ) -> None:
        """
        Parses the rules defined in 'rules', applicable to the given
        'family_id'.

        :param family_id: See SourceFamilyIdType
        :param rules: See ConditionType
        """
        # TODO A lot of the validation below should be move earlier
        # in the sources_db_processor.py
        if raw_conditions:
            if not isinstance(raw_conditions, list):
                raise TypeError(f"rules {raw_conditions} should be a list")

        # Take Aliases rules list into collection / used_scenarios
        conditions_in_lib: SourceFamilyToConditionsMap
        scenarios_in_lib: list[ScenarioKeyType]

        # Classify the source family as gnat or gnarl.
        # This is needed before inserting into self.rules and self.lib_scenarios
        # Classify with gnat rules any family that doesn't start with "gnat/"
        if family_id.split("/")[0] == "gnarl":
            conditions_in_lib = self._conditions_rules["gnarl"]
            scenarios_in_lib = self._lib_scenarios["gnarl"]
        else:
            # Everything else is gnat
            conditions_in_lib = self._conditions_rules["gnat"]
            scenarios_in_lib = self._lib_scenarios["gnat"]

        # Check to make sure the rule is not overridden
        if family_id in conditions_in_lib:
            raise RuntimeError(f"family_id {family_id} defined twice")

        # Add the rule object to the current set of rules
        # Example: self.lib_scenarios["gnat"]["common"] = <Rule>

        conditions = ConditionsRule(
            raw_conditions,
            all_acceptable_scenarios=self._acceptable_scenarios,
        )

        conditions_in_lib[family_id] = conditions

        # We append all scenarios from the newly added rule to
        # self.lib_scenarios[...] this will be sorted later. (end of __init__)
        for sc in conditions.used_scenarios:
            if sc not in scenarios_in_lib:
                scenarios_in_lib.append(sc)

    def install_tree(
        self,
        dest_json: Path,
        output_sources_dir: Path,
        link: bool,
        save_resolution_record: bool = True,
    ) -> None:
        """
        Install the runtime sources and the json descriptor.

        :param dest_json: The path to the destination JSON file to create
        :param output_sources_dir: The destination directory for the sources
        :param link: If true, files will be symbolically linked instead of copied
        """
        # Dump the JSON file describing the sources
        gen_and_dump_json(
            self._conditions_rules,
            self._lib_scenarios,
            dest_json,
            output_sources_dir,
            self._acceptable_scenarios,
        )

        # Install the assembled sources
        # template_config is null because that info is target specific
        # and will be added in the targetization step
        self._files_holder.install(
            output_sources_dir, template_config={}, link=link, overwrite=True
        )

        # Add the resolution record
        if save_resolution_record:
            get_resolver().write_resolution_records_to_metadata(
                filename="assembly_resolution_record.csv",
                subdir=Path("assembly") / f"{self._platform}_{self._top_base_profile}",
            )
