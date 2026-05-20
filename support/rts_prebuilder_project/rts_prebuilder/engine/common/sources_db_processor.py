#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import cast

from rts_prebuilder.base_types import RtsSourcesDBType, RawRtsSourcesDBType


def sources_db_proc(raw_sources_db: RawRtsSourcesDBType) -> RtsSourcesDBType:
    """
    This function processes the raw sources database into a validated
    RtsSourcesDBType instance.
    """

    _converts_path_strings_to_path_objects(raw_sources_db)

    # At this point, we can safely cast the type
    # for static analysis purposes
    sources_db = cast(RtsSourcesDBType, raw_sources_db)

    # Validate the db
    _validate_db(sources_db)

    return sources_db


# PRIVATE FUNCTIONS BELOW


def _converts_path_strings_to_path_objects(db: RawRtsSourcesDBType) -> None:

    for family_id, family_entry in db.items():
        if not isinstance(family_entry, dict):
            raise Exception(
                f"Invalid type for family '{family_id}': expected dict, got {type(family_entry)} "
                f'With content "{family_entry}"'
            )
        for key, value in family_entry.items():
            if not key.endswith("srcs"):
                continue

            if not isinstance(value, list):
                raise Exception(
                    f"Invalid type for key '{key}': expected list or str, got {type(value)}"
                )

            # Convert each entry to Path
            # We need to ignore type checking because at this point we're midway
            # between RawRtsSourcesDBType and RtsSourcesDBType
            family_entry[key] = [Path(v) for v in value]  # type: ignore


def _validate_db(db: RtsSourcesDBType) -> bool:
    """
    Validate the database structure.
    This is a basic types validation, it does not check the logic of the conditions
    or the existence of the sources.
    """

    for family_id, family_entry in db.items():
        if not isinstance(family_id, str):
            raise ValueError(f"Family ID must be a string, got {type(family_id)}")

        if not isinstance(family_entry, dict):
            raise ValueError(f"Family entry must be a dict, got {type(family_entry)}")

        # Check 'conditions' field
        if "conditions" in family_entry:
            if not isinstance(family_entry["conditions"], list) or not all(
                isinstance(cond, str) for cond in family_entry["conditions"]
            ):
                raise ValueError(
                    f"'conditions' must be a list of strings in family '{family_id}'. "
                    f"Got \"{family_entry['conditions']}\" instead"
                )
            _validate_rules(family_entry["conditions"], family_id, "conditions")

        # Check 'requires' field
        if "requires" in family_entry:
            if not isinstance(family_entry["requires"], list) or not all(
                isinstance(req, str) for req in family_entry["requires"]
            ):
                raise ValueError(
                    f"'requires' must be a list of strings in family '{family_id}'"
                )
            _validate_rules(family_entry["requires"], family_id, "requires")

    return True


def _validate_rules(rules: list[str], family_id: str, rule_type: str) -> None:
    """
    Validate rule strings for _comment and requires fields.

    Args:
        rules: List of rule strings to validate
        family_id: The family ID for error reporting
        rule_type: Either "conditions" or "requires" for error reporting
    """
    seen_scenario_keys = set()

    for rule in rules:
        # Check that rule is of format "ScenarioKeyType:ScenarioValueType"
        if ":" not in rule:
            raise ValueError(
                f"Syntax error: wrong rule '{rule}' in {rule_type}"
                f"for family '{family_id}'"
            )

        # Split scenario_key and values from within the rule
        scenario_key, scenario_value = rule.split(
            ":", 1
        )  # Use maxsplit=1 to handle multiple colons
        scenario_key = scenario_key.strip()
        scenario_value = scenario_value.strip()

        if len(scenario_key) == 0:
            raise ValueError(
                f"Syntax error: empty scenario key in rule '{rule}'"
                f" in {rule_type} for family '{family_id}'"
            )

        if len(scenario_value) == 0:
            raise ValueError(
                f"Syntax error: empty scenario value in rule '{rule}'"
                f"in {rule_type} for family '{family_id}'"
            )

        # Ensure scenario keys are unique within the rules list
        if scenario_key in seen_scenario_keys:
            raise ValueError(
                f"Duplicated scenario variable '{scenario_key}' in {rule_type}"
                f"for family '{family_id}'"
            )
        seen_scenario_keys.add(scenario_key)

        # Handle rules with multiple comma separated values
        cases = [s.strip() for s in scenario_value.split(",")]

        # Handle negated values - check consistency
        if cases[0].startswith("!"):
            # If first value is negated, all values must be negated
            for case in cases:
                if not case.startswith("!"):
                    raise ValueError(
                        f"Negation must apply to every item in the list: "
                        f"'{rule}' in {rule_type} for family '{family_id}'"
                    )
