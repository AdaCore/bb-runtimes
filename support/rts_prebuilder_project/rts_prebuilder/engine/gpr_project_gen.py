#
# Copyright (C) 2026, AdaCore
#

"""Render the gprbuild project files (.gpr) used to build a runtime.

Mirror of the runtime.xml path: the writer holds no template or substitution
logic, it hands resolved data here and writes back the rendered text. Each
function reads its template datafile and fills the quoted-list slots.
"""

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import GprSwitches

from .infrastructure_interface import resolve_and_read_file

# Separators as they must read inside a template's ("...") slot: the quotes
# around each element are already in the template, so joining glues them.
_GPR_LIST_SEP = '", "'
_SWITCH_LIST_SEP = '",\n        "'


def render_target_options_gpr(
    switches: GprSwitches,
    gnarl_switches: list[str],
    debug_switches: list[str],
) -> str:
    """target_options.gpr: the switches the runtime's own build compiles with.

    :param switches: the per-language sets and the library link.
    :param gnarl_switches: switches for the tasking half alone.
    :param debug_switches: switches for the Debug and Gnatcov builds alone.
    :returns: the rendered project.
    :raises ValueError: if a slot a switch was registered for is missing from
        the template, since rendering would drop it.
    """
    template = resolve_and_read_file(Path("datafiles/target_options.gpr.in"))
    slots = {
        "common_switches": switches.common,
        "c_switches": switches.c,
        "asm_switches": switches.asm,
        "runtime_link_switches": switches.link,
        "runtime_gnarl_switches": tuple(gnarl_switches),
        "runtime_debug_switches": tuple(debug_switches),
    }
    slotless = [
        slot
        for slot, values in slots.items()
        if values and "{" + slot + "}" not in template
    ]
    if slotless:
        raise ValueError(
            f"target_options.gpr template has no slot for "
            f"{', '.join(slotless)}: the switches registered for "
            f"{'those slots' if len(slotless) > 1 else 'that slot'} would be "
            f"dropped. Add the slot to the template, or stop registering them"
        )
    return template.format(
        **{slot: _SWITCH_LIST_SEP.join(values) for slot, values in slots.items()}
    )


def render_runtime_build_gpr(
    *, target_triple: str, is_native: bool, languages: set[str]
) -> str:
    """runtime_build.gpr: the libgnat build project."""
    target_directive = "" if is_native else f'for Target use "{target_triple}";'
    runtime_spark_units = resolve_and_read_file(
        Path("datafiles/runtime_spark_units.lst")
    ).splitlines()
    template = resolve_and_read_file(Path("datafiles/runtime_build.gpr.in"))
    return template.format(
        target_directive=target_directive,
        source_dirs=_GPR_LIST_SEP.join(["gnat_user", "gnat"]),
        runtime_spark_units=_GPR_LIST_SEP.join(runtime_spark_units),
        languages=_GPR_LIST_SEP.join(sorted(languages)),
    )


def render_ravenscar_build_gpr(languages: set[str]) -> str:
    """ravenscar_build.gpr: the libgnarl build project (tasking runtimes only)."""
    template = resolve_and_read_file(Path("datafiles/ravenscar_build.gpr.in"))
    return template.format(
        source_dirs=_GPR_LIST_SEP.join(["gnarl_user", "gnarl"]),
        languages=_GPR_LIST_SEP.join(sorted(languages)),
    )
