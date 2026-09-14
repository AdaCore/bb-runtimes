#
# Copyright (C) 2026, AdaCore
#

"""Exercise the .gpr renderers, in particular the target_options.gpr slots.

A target may ship its own target_options.gpr template, so the set of slots is
not guaranteed to match what a component registered. These tests pin what
happens when the two disagree.
"""

from pathlib import Path

import pytest

import rts_prebuilder.engine.gpr_project_gen as gprgen
from rts_prebuilder.abstract_infrastructure import GprSwitches
from rts_prebuilder.engine.gpr_project_gen import render_target_options_gpr


def _render(
    switches: GprSwitches | None = None,
    gnarl: list[str] | None = None,
    debug: list[str] | None = None,
) -> str:
    """Render with whatever is left unnamed empty."""
    return render_target_options_gpr(
        switches if switches is not None else GprSwitches(), gnarl or [], debug or []
    )


def _template(monkeypatch: pytest.MonkeyPatch, text: str) -> None:
    """Make the renderer read ``text`` instead of the shipped template."""
    monkeypatch.setattr(gprgen, "resolve_and_read_file", lambda path: text)


def test_slots_are_filled_per_field(monkeypatch):
    _template(monkeypatch, 'C := ("{c_switches}"); Asm := ("{asm_switches}");')

    rendered = _render(GprSwitches(c=("-DIN_RTS",), asm=("-x",)))

    assert rendered == 'C := ("-DIN_RTS"); Asm := ("-x");'


def test_field_no_target_filled_renders_empty(monkeypatch):
    _template(monkeypatch, 'Asm := ("{asm_switches}");')

    assert _render() == 'Asm := ("");'


def test_gnarl_and_debug_fill_their_own_slots(monkeypatch):
    _template(
        monkeypatch,
        'G := ("{runtime_gnarl_switches}"); D := ("{runtime_debug_switches}");',
    )

    assert _render(gnarl=["-mg"], debug=["-g"]) == 'G := ("-mg"); D := ("-g");'


def test_switches_for_a_slotless_field_raise(monkeypatch):
    # Rendering would drop them, so refuse instead.
    _template(monkeypatch, 'C := ("{c_switches}");')

    with pytest.raises(ValueError, match="no slot for asm_switches"):
        _render(GprSwitches(asm=("-x",)))


def test_slotless_field_without_switches_is_fine(monkeypatch):
    _template(monkeypatch, 'C := ("{c_switches}");')

    assert _render(GprSwitches(asm=())) == 'C := ("");'


_TEMPLATE_SLOTS = (
    "common_switches",
    "c_switches",
    "asm_switches",
    "runtime_link_switches",
    "runtime_gnarl_switches",
    "runtime_debug_switches",
)


def test_shipped_template_has_a_slot_for_every_switch():
    """The renderer's check only fires on a template that lost a slot. The one
    this repo ships must name all of them.

    Read it straight off disk, not through the resolver. The resolver's
    gnat/gcc dirs are process-global, and belong to whichever test ran last."""
    template = (
        Path(__file__).parents[2] / "src/datafiles/target_options.gpr.in"
    ).read_text()

    assert [s for s in _TEMPLATE_SLOTS if "{" + s + "}" not in template] == []
