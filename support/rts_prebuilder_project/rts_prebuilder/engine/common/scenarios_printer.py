#
# Copyright (C) 2025-2026, AdaCore
#
"""Utilities for formatting and displaying scenario configurations."""

from rts_prebuilder.base_types import GnatOrGnarlType, ScenariosConfigurationType
from pathlib import Path

from rts_prebuilder.end_user_data.logger import get_logger
from rts_prebuilder.end_user_data.metadata import write_metadata

log = get_logger(__name__)


def format_scenario_table(scenarios: ScenariosConfigurationType) -> str:
    """Format scenario configuration as a compact two-column table.

    Args:
        scenarios: Dictionary of scenario variable names to values

    Returns:
        Formatted string with scenarios in two columns for compact display

    Example:
        >>> scenarios = {"Add_Arith64": "yes", "Target_Word_Size": "32"}
        >>> print(format_scenario_table(scenarios))
          Add_Arith64                    = yes              Target_Word_Size               = 32
    """
    items = sorted(scenarios.items())
    lines = []

    # Split into two columns
    mid = (len(items) + 1) // 2
    left_col = items[:mid]
    right_col = items[mid:]

    # Pad right column if needed
    while len(right_col) < len(left_col):
        right_col.append(("", ""))

    # Format each row with two columns
    for (k1, v1), (k2, v2) in zip(left_col, right_col):
        left = f"  {k1:30} = {v1:15}" if k1 else " " * 48
        right = f"  {k2:30} = {v2:15}" if k2 else ""
        lines.append(f"{left}{right}".rstrip())

    return "\n".join(lines)


def save_scenarios_to_metadata(
    scenarios: ScenariosConfigurationType,
    targetized_runtime_name: str,
    lib: GnatOrGnarlType,
) -> None:
    """Save scenario configuration to metadata file and log it.

    Args:
        scenarios: Dictionary of scenario variable names to values
        targetized_runtime_name: Name of the target runtime (e.g., "light-cortex-m3")
        lib: Library name ("gnat" or "gnarl")
    """
    # Format scenario configuration as table
    scenario_table = format_scenario_table(scenarios)

    # Log the scenario configuration
    log.debug(
        "Target %s (%s) scenario configuration:\n%s",
        targetized_runtime_name,
        lib,
        scenario_table,
    )

    # Format the content
    content = (
        f"Runtime name: {targetized_runtime_name}\n"
        f"Library: {lib}\n"
        f"Scenario Configuration:\n"
        f"{scenario_table}\n"
    )

    # Write to metadata file
    write_metadata(
        filename=Path(f"scenarios_{targetized_runtime_name}_{lib}.txt"),
        content=content,
        subdir=Path(targetized_runtime_name),
    )
