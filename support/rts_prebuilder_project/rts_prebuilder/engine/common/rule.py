#
# Copyright (C) 2025-2026, AdaCore
#

from typing import ClassVar

from rts_prebuilder.base_types import (
    AllPossibleScenarioConfigsType,
    RtsSourcesDBType,
    RuleScenariosType,
    RuleStringType,
    ScenarioKeyType,
    ScenariosConfigurationType,
    SourceFamilyIdType,
)


class Rule(object):
    """
    This class is collection of scenario keys and their expected value
    (see _scenarios attributes) with helper functions to interact with
    those rules.
    """

    _scenarios: RuleScenariosType
    """
    This is the main attribute abstracted by the <Rule> class.
    See RuleScenariosType documentation for more info.

    The main responsibility of the Rule class is to give easy handlers to
    interface with this attribute.
    """

    invalid: bool
    """
    Boolean set to True if the rules parsing failed.

    TODO document which parsing failure exactly, a lot of the failures
    raise Assertion errors instead of setting this bit.
    """

    def __init__(
        self,
        rules: list[RuleStringType] | None,
        all_acceptable_scenarios: AllPossibleScenarioConfigsType,
    ) -> None:
        """
        This constructor initializes parses the raw rules of (RuleStringType)
        into the _scenarios.

        :param rules: Raw list of RuleStringType to be processed
        :param all_acceptable_scenarios: See AllPossibleScenarioConfigsType
        :raise AssertionError: TODO Document raised assertion errors
        """
        # Initialize attributes
        self._scenarios = {}
        self.invalid = False

        if rules is None or len(rules) == 0:
            # If no rules, nothing to parse
            return

        for rule in rules:
            # Reminder: rule is of type RuleStringType

            scenario_key: ScenarioKeyType
            scenario_value: str  # Not yet ScenarioValueType, still needs processing

            # Split scenario_key and values from within the rule
            scenario_key, scenario_value = rule.split(":")
            scenario_key = scenario_key.strip()
            scenario_value = scenario_value.strip()

            if scenario_key not in all_acceptable_scenarios:
                raise ValueError(f"Unknown scenario variable {scenario_key}")

            # Handle RuleStringType with multiple comma separated values
            cases: list[str] = [s.strip() for s in scenario_value.split(",")]

            # Handle case RuleStringType with negated values
            # Only first value is checked for negation
            if cases[0][0] == "!":
                negate: bool = True
                n_cases: list[str] = []
                for case in cases:
                    # FIXME with the current implementation, if the first value
                    # is not negated but the others are, this assertion will
                    # not be checked
                    n_cases.append(case[1:])
                cases = n_cases
            else:
                negate = False

            # Initialize the list of acceptable values for the scenario key
            self._scenarios[scenario_key] = []

            if negate:
                # If negated, Initialize with all valid scenario values
                self._scenarios[scenario_key] = all_acceptable_scenarios[scenario_key][
                    :
                ]

            # Use the parsed values to fill self._scenarios
            for case in cases:
                # Reminder: case is of type ScenarioValueType

                # Filter out values that are not expected
                if (
                    all_acceptable_scenarios is None
                    or case in all_acceptable_scenarios[scenario_key]
                ):
                    if negate:
                        self._scenarios[scenario_key].remove(case)
                    else:
                        self._scenarios[scenario_key].append(case)

            # Ensure the possible values are not empty
            if len(self._scenarios[scenario_key]) == 0:
                # Clear everything: it's a rule that can never match
                self._scenarios = {}
                self.invalid = True
                break

    @property
    def is_empty(self) -> bool:
        """Returns True if there are no scenario keys in the Rule"""
        return len(self._scenarios) == 0

    @property
    def used_scenarios(self) -> list[ScenarioKeyType]:
        """Returns a list of all scenario keys in the Rule"""
        return list(self._scenarios.keys())

    def has_scenario(self, var: ScenarioKeyType) -> bool:
        """Returns True if the given scenario key is defined in the Rule"""
        return var in self._scenarios

    def matches(
        self,
        config: ScenariosConfigurationType,
        exact: bool = False,
    ) -> bool:
        """
        Considering a given configuration, compare it to self._scenarios
        to see if it matches the rule represented by self.

        if exact is True: All ScenarioKeyType keys in the rule must be
        represented in the input configuration.
        For example, if input is {"Has_A": True}
        and rule is              {"Has_A": True, "OtherScenario": B}
        -> Then this returns False.

        :param config: Configuration to compare
        :param exact:  The set of ScenarioKeys in both should be exactly the same.
        """
        if self.invalid:
            return False
        for scenario_key in self._scenarios:
            # Reminder: scenario_key is a ScenarioKeyType
            if scenario_key not in config:
                return False
            if config[scenario_key] not in self._scenarios[scenario_key]:
                # not an expected value
                return False
        if exact:
            for scenario_key in config:
                if scenario_key not in self._scenarios:
                    # Some extra variable is defined. We wanted a full match so
                    # let's skip
                    return False
        return True


class ConditionsRule(Rule):
    """
    A specialization of Rule class for "conditions" entries.
    """

    __scenarios_global_stats: ClassVar[dict[ScenarioKeyType, int]] = {}
    """
    A collection of all used scenario keys throughout the life of the python
    program with the respective count of usage for each.

    This serves to collect statistics on scenario variable usage, to better
    generate the json descriptor file (most used scenario at the top-level of
    nested case statements).
    """

    def __init__(
        self,
        rules: list[RuleStringType] | None,
        all_acceptable_scenarios: AllPossibleScenarioConfigsType,
    ) -> None:
        """
        Same as Rule.__init__(), but also updates the global usage statistics
        for condition scenario keys.
        """
        super().__init__(rules, all_acceptable_scenarios)

        # Conditions are also recorded for usage statistics purposes

        # Update the dict of all used scenario keys in this instance
        # to the class wide __scenarios_global_stats.keys().
        # or incrementing them if already accounted for.
        # For usage statistics purposes
        for sv in self._scenarios:
            if sv not in ConditionsRule.__scenarios_global_stats:
                ConditionsRule.__scenarios_global_stats[sv] = 1
            else:
                ConditionsRule.__scenarios_global_stats[sv] += 1

    def partial_match(self, config: ScenariosConfigurationType) -> bool:
        """
        If the scenarios given match the rule represented by self,
        then return True. But when comparing, only check the keys from
        the input scenarios.

        NOT all ScenarioKeyType keys in the rule must represented in the input
        configuration, for the check.

        For example, if input is {"Has_A": True}
        and rule is {"Has_A": True, "OtherScenario": B}
        -> Then this is OK.

        :param config: Rule set to compare
        """
        for scenario_key in config:
            # Reminder: scenario_key is a ScenarioKeyType

            if scenario_key not in self._scenarios:
                # some extra variable is defined. We wanted a full match so
                # let's skip
                return False
            if config[scenario_key] not in self._scenarios[scenario_key]:
                return False
        return True

    @staticmethod
    def scenario_global_stats(var: ScenarioKeyType) -> int:
        """Returns the count of the given scenario key in the all instantiated Rule classes
        Since the start of the program.
        """
        if var in ConditionsRule.__scenarios_global_stats:
            return ConditionsRule.__scenarios_global_stats[var]
        else:
            return 0


class RequirementsRule(Rule):
    """
    A specialization of Rule class for "requires" entries.
    """

    def corresponding_scenario_config(self) -> ScenariosConfigurationType:
        """
        Extracts scenarios from self._scenarios attribute, but only those that
        have one possible value, otherwise raise assertion error.
        See ScenariosConfigurationType for more details.

        :returns: A ScenariosConfigurationType object with a 1:1 mapping of
            scenario keys to their single possible value.
        """
        scenario_config: ScenariosConfigurationType = {}
        for scenario_key in self._scenarios:
            # Reminder: scenario_key is a ScenarioKeyType
            if len(self._scenarios[scenario_key]) != 1:
                raise RuntimeError(
                    "Cannot generate automatically a dependency,"
                    " when several choices are possible: %s:%s"
                    % (scenario_key, str(self._scenarios[scenario_key]))
                )
            scenario_config[scenario_key] = self._scenarios[scenario_key][0]
        return scenario_config


def complete_scenarios_from_deps(
    config: ScenariosConfigurationType,
    all_possible_scenarios: AllPossibleScenarioConfigsType,
    all_sources: RtsSourcesDBType,
) -> None:
    """
    Adds missing scenario to the given scenario configuration.

    The missing scenario detection is based on analysis of "requires"
    for all family entries (see SourcesFamilyEntryType) that "match"
    for the given config.

    :param config: Set of scenario configurations to be edited
    """
    while True:
        modified: bool = False
        for _, content in all_sources.items():
            # Reminder: content is of type SourcesFamilyEntryType
            matches: bool = False

            if "requires" not in content:
                continue

            if "conditions" not in content:
                matches = True
            else:  # i.e. conditions are present
                rule = ConditionsRule(content["conditions"], all_possible_scenarios)
                if rule.matches(config):
                    matches = True

            if matches:
                # if conditions and scenarios match but dependencies
                # do not match with `scenarios`, the missing dependency
                # is added automatically by the call to complete_scenarios_from_deps
                # Warning dep.corresponding_scenario_config() raises assertion
                # if the dependency has more that one scenario.
                dep = RequirementsRule(content["requires"], all_possible_scenarios)
                if not dep.matches(config):
                    modified = True
                    config.update(dep.corresponding_scenario_config())

        if not modified:
            break


# Type definition frequently use
type SourceFamilyToConditionsMap = dict[SourceFamilyIdType, ConditionsRule]
"""
Common pattern, to associate ConditionsRule to a source family type
"""

type SourceFamilyToRequirementsMap = dict[SourceFamilyIdType, RequirementsRule]
"""
Common pattern, to associate RequirementsRule to a source family type
"""
