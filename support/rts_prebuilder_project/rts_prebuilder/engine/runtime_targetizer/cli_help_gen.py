#
# Copyright (C) 2025-2026, AdaCore
#

from __future__ import annotations

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)


def list_targets_for_help(
    targets: dict[str, AbstractTarget],
    generators: list[AbstractTargetGenerator],
    summary_only: bool = False,
) -> str:
    """Generate short or full target listings for CLI help.

    - Short: lists concrete target instances with class names and generator patterns.
    - Full:  lists concrete target instances with profiles and class, and enumerates
             generator variants (instantiating each to show class and profiles).
    """
    lines: list[str] = []

    if summary_only:
        lines.append(
            "Use --list-targets for the full exhaustive list of concrete target instances."
            " (Including those from generators.)"
        )
        if targets:
            lines.append("concrete target instances:")
            for target_name in sorted(targets.keys()):
                t = targets[target_name]
                lines.append(f"  - {target_name} ({t.__class__.__name__})")
        if generators:
            lines.append(
                "The following are patterns you can use to specify targets:"
                "  (with --list-targets, you can see the exhaustive list"
                "   but it may be long (use grep))"
            )
            for gen in generators:
                lines.append(f"  - {gen.cli_name_pattern}")
        lines.append("")
        return "\n".join(lines)

    # Full
    lines.append("Available targets (verbose):")

    profiles_list: list[str] = []

    # Concrete target instances with details
    for target_name, target in targets.items():
        profiles_list = [
            f"{p} (rts {target.runtime_name_generator(p)})" for p in target.system_ads
        ]
        lines.append(
            f"cli_name: {target_name:30}| name: {target.name:25}| "
            f"profiles: {', '.join(profiles_list)} | "
            f"Class: {target.__class__.__module__}.{target.__class__.__name__}"
        )

    # Enumerate generator variants lazily; instantiate to include class and profiles
    for gen in generators:
        for name in gen.generate_variants():
            t = gen.instantiate(name)

            if t is None:
                lines.append(
                    f"cli_name: {name:30}| from pattern: {gen.cli_name_pattern}"
                )
                continue

            profiles_list = [
                f"{p} (rts {t.runtime_name_generator(p)})" for p in t.system_ads
            ]

            lines.append(
                f"cli_name: {name:30}| name: {t.name:25}| "
                f"profiles: {', '.join(profiles_list)} | "
                f"Class: {t.__class__.__module__}.{t.__class__.__name__}"
            )

    return "\n".join(lines)
