#
# Copyright (C) 2026, AdaCore
#

"""Exercise the component contribution mechanism itself.

A component declares the sources and switches it brings through a
``contribute(target, profile)`` hook. The query driver chains the hook
base-to-derived across the MRO, so authors never call ``super``. It re-runs the
hook per (target, profile). That is the only home for entries, and the ``add_*``
helpers raise anywhere else.

Structure goes the other way round. Dependencies are composed in ``__init__``
only, and raise from ``contribute``. A target is the root component of its own
tree, so it gets the same split through the same hook.

These tests pin that machinery down directly, not through a full target
selection.
"""

from pathlib import Path
from typing import override

import pytest

from rts_prebuilder.base_types import PlatformIdType
from bb_runtimes_targets_gen.concrete_infrastructure import DFBBTarget
from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourceFile,
    SourcesAndFlagsComponent,
)

engine_interface.self_register()

_LIGHT = Profile(name="light", base_profile="light", system_ads=Path("s.ads"))
_EMBEDDED = Profile(name="embedded", base_profile="embedded", system_ads=Path("s.ads"))


def _some_target() -> AbstractTarget:
    return _MinimalTarget()


def _src_names(sources: list[SourceFile]) -> list[str]:
    """Basename each SourceFile carries (dest_path if set, else the source)."""
    return [
        Path(
            str(s.dest_path if s.dest_path is not None else s.unresolved_src_path)
        ).name
        for s in sources
    ]


# --- fixtures: tiny components exercising the hook -------------------------


class _MinimalTarget(AbstractTarget):
    """A target is the one component a class may derive from, so the chaining
    fixtures below are targets."""

    @property
    @override
    def name(self) -> str:
        return "faketarget"

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "bb"

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return False

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [_LIGHT]

    @property
    @override
    def target(self) -> str:
        return "faketarget-elf"


class _Base(_MinimalTarget):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("base.ads")


class _Derived(_Base):
    # Deliberately does NOT call super(): the driver must still run _Base.
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("derived.ads")


class _Foo(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("foo.ads")


class _Bar(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("bar.ads")


class _ProfileVarying(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(f"{profile.name}.ads")


# --- tests ----------------------------------------------------------------


def test_contribute_chains_mro_base_then_derived_without_super():
    """Every class's contribute runs once, base->derived, with no super call."""
    t = _some_target()
    p = t.profiles[0]
    names = _src_names(_Derived().get_sources(t, p, dest_subdir=Path("gnat")))
    assert names == ["base.ads", "derived.ads"]


def test_default_contribute_adds_nothing():
    t = _some_target()
    p = t.profiles[0]
    assert SourcesAndFlagsComponent().get_sources(t, p) == []


def test_registering_from_init_raises():
    """Entries have one home. Outside a contribute run there is no buffer to
    write to, so registering from __init__ fails at construction rather than
    going silently missing at query time."""

    class _Eager(SourcesAndFlagsComponent):
        def __init__(self) -> None:
            super().__init__()
            self.add_gnat_sources("eager.ads")

    with pytest.raises(TypeError, match="outside contribute"):
        _Eager()


def test_contribute_varies_by_profile_and_never_mutates_instance():
    """Same instance, queried for two profiles, returns each profile's set."""
    t = _some_target()
    comp = _ProfileVarying()
    first = _src_names(comp.get_sources(t, _LIGHT))
    second = _src_names(comp.get_sources(t, _EMBEDDED))
    assert first == ["light.ads"]
    assert second == ["embedded.ads"]
    # Re-querying the first profile still yields only its set (no accumulation).
    assert _src_names(comp.get_sources(t, _LIGHT)) == ["light.ads"]


def test_get_sources_recurses_into_children():
    t = _some_target()
    p = t.profiles[0]
    root = _Foo()
    root.add_dependency(_Bar())
    assert {"foo.ads", "bar.ads"} <= set(_src_names(root.get_sources(t, p)))


# --- runtime-build switches -----------------------------------------------
#
# One of the two destinations of add_switches: what the runtime is built with
# (target_options.gpr). Same contribution mechanism as sources, but merged
# field by field into one GprSwitches.


class _CommonSwitch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_runtime_switches(
            common_switches=("-fparent",),
        )


class _ChildSwitch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_runtime_switches(
            common_switches=("-fchild",),
        )
        self.add_runtime_switches(
            c_switches=("-DCHILD",),
        )


class _ProfileSwitch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        if "light" in profile.base_profile:
            self.add_runtime_switches(
                c_switches=("-DLIGHT",),
            )


def test_runtime_build_switches_merge_self_and_children_per_group():
    t = _some_target()
    p = t.profiles[0]
    root = _CommonSwitch()
    root.add_dependency(_ChildSwitch())
    switches = root.get_runtime_switches(t, p)
    # Children lead, this node's own follows (defaults-first ordering).
    assert switches.common == ("-fchild", "-fparent")
    assert switches.c == ("-DCHILD",)


def test_runtime_build_switches_vary_by_profile_without_mutating_instance():
    t = _some_target()
    comp = _ProfileSwitch()
    assert comp.get_runtime_switches(t, _LIGHT).c == ("-DLIGHT",)
    assert comp.get_runtime_switches(t, _EMBEDDED).c == ()
    # No accumulation on re-query.
    assert comp.get_runtime_switches(t, _LIGHT).c == ("-DLIGHT",)


def test_replace_channel_reaches_everything_this_node_registered():
    """With one home, replace_channel has one meaning: it drops the node's whole
    channel, including what a base class contributed earlier in the chain."""

    class _Base(_MinimalTarget):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_runtime_switches(
                common_switches=("-fbase",),
            )

    class _Overriding(_Base):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_runtime_switches(
                common_switches=("-fonly",),
                replace_channel=True,
            )

    t = _some_target()
    got = _Overriding().get_runtime_switches(t, t.profiles[0])
    assert got.common == ("-fonly",)


def test_replace_channel_reaches_both_destinations():
    """One call writes to both, so clearing has to reach both. An -mcpu variant
    that dropped only the runtime's copy would ship two -mcpu to the end user."""

    class _Base(_MinimalTarget):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_and_runtime_switches(
                common_switches=("-mcpu=base",),
            )

    class _Variant(_Base):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_and_runtime_switches(
                common_switches=("-mcpu=variant",),
                replace_channel=True,
            )

    t = _some_target()
    p = t.profiles[0]
    comp = _Variant()

    assert comp.get_app_switches(t, p).common == ("-mcpu=variant",)
    assert comp.get_runtime_switches(t, p).common == ("-mcpu=variant",)


# --- app-compile switches -------------------------------------------------
#
# The other destination of add_switches: what the end user compiles with
# (runtime.xml), as opposed to what the runtime is built with.


def test_one_call_reaches_both_destinations():
    class _Both(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_and_runtime_switches(
                common_switches=("-mcpu=x",),
                c_switches=("-DX",),
            )

    t = _some_target()
    p = t.profiles[0]
    comp = _Both()

    assert comp.get_app_switches(t, p).common == ("-mcpu=x",)
    assert comp.get_app_switches(t, p).c == ("-DX",)
    assert comp.get_runtime_switches(t, p).common == ("-mcpu=x",)
    assert comp.get_runtime_switches(t, p).c == ("-DX",)


def test_app_switches_lead_with_this_node_then_dependencies():
    """The two destinations fold the other way round from each other: the app
    side keeps this node's own entries first, the runtime side lets its
    dependencies lead."""

    class _Dependency(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_and_runtime_switches(
                common_switches=("-dependency",),
            )

    class _Root(SourcesAndFlagsComponent):
        def __init__(self) -> None:
            super().__init__()
            self.add_dependency(_Dependency())

        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_and_runtime_switches(
                common_switches=("-own",),
            )

    t = _some_target()
    p = t.profiles[0]
    root = _Root()

    assert root.get_app_switches(t, p).common == ("-own", "-dependency")
    assert root.get_runtime_switches(t, p).common == ("-dependency", "-own")


def test_runtime_only_switches_asked_for_the_app_raises():
    """Gnarl and debug narrow the runtime's own build, so only its method offers
    them."""

    class _GnarlForApp(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_switches(gnarl_switches=("-x",))  # type: ignore[call-arg]

    t = _some_target()
    with pytest.raises(TypeError, match="gnarl_switches"):
        _GnarlForApp().get_app_switches(t, t.profiles[0])


def test_link_switches_merge_like_any_other_field():
    """They apply whichever loader the program picks. The ones a single loader
    adds live on that ``Loader``, not here."""

    class _Dependency(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_switches(link_switches=("-nostdlib",))

    class _Board(SourcesAndFlagsComponent):
        def __init__(self) -> None:
            super().__init__()
            self.add_dependency(_Dependency())

        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_switches(link_switches=("-Wl,-r",))

    t = _some_target()
    assert _Board().get_app_switches(t, t.profiles[0]).link == (
        "-Wl,-r",
        "-nostdlib",
    )


def test_gnarl_and_debug_switches_have_their_own_channels():
    """They are not part of the set, so they neither reach nor take from it."""

    class _Tasking(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_runtime_switches(
                gnarl_switches=("-mgeneral-regs-only",),
                debug_switches=("-g",),
            )

    t = _some_target()
    p = t.profiles[0]
    comp = _Tasking()

    assert comp.get_runtime_gnarl_switches(t, p) == ["-mgeneral-regs-only"]
    assert comp.get_runtime_debug_switches(t, p) == ["-g"]
    assert not comp.get_runtime_switches(t, p)


def test_call_with_no_switch_raises():
    class _Empty(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_runtime_switches()

    t = _some_target()
    with pytest.raises(ValueError, match="no switch given"):
        _Empty().get_runtime_switches(t, t.profiles[0])


# --- gpr external variables -----------------------------------------------


def test_gpr_externals_merge_self_and_children():
    class _Child(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_gpr_external("Desk_Home", "DESKHOME")

    class _Root(SourcesAndFlagsComponent):
        def __init__(self) -> None:
            super().__init__()
            self.add_dependency(_Child())

        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_gpr_external("VSB_Dir", "VSB_DIR")

    t = _some_target()
    assert _Root().get_gpr_externals(t, t.profiles[0]) == [
        ("VSB_Dir", "VSB_DIR"),
        ("Desk_Home", "DESKHOME"),
    ]


# --- dependencies: composed in __init__, never from contribute --------------


class _Leaf(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("leaf.ads")


class _Other(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_source("other.ads")


class _WithDep(SourcesAndFlagsComponent):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(_Leaf())


def test_dependency_composed_in_init_flows_into_queries():
    t = _some_target()
    assert _src_names(_WithDep().get_sources(t, _LIGHT)) == ["leaf.ads"]


def test_get_dependencies_reports_the_composed_set():
    """The graph is runtime-independent, so the query takes no target/profile."""
    assert [type(c) for c in _WithDep().get_dependencies()] == [_Leaf]
    assert SourcesAndFlagsComponent().get_dependencies() == []


def test_get_dependencies_hands_back_a_copy():
    comp = _WithDep()
    comp.get_dependencies().clear()
    assert len(comp.get_dependencies()) == 1


def test_composing_a_dependency_from_contribute_raises():
    """The graph is fixed per instance: a per-runtime edit would either leak into
    the next runtime queried or silently differ from what get_dependencies
    reports. Gate the component on the profile instead."""

    class _Bad(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_dependency(_Other())

    with pytest.raises(TypeError, match="cannot be called from here"):
        _Bad().get_sources(_some_target(), _LIGHT)


def test_deriving_from_a_component_raises():
    """Reuse is composition. A subclass would inherit its base's dependencies
    and entries with no verb to drop either."""
    with pytest.raises(TypeError, match="compose it with add_dependency"):

        class _Derived(_Leaf):
            pass


def test_deriving_from_a_target_is_allowed():
    """The exception: a target hierarchy carries the properties describing the
    hardware, and nothing composes a property."""

    class _Board(_MinimalTarget):
        pass

    assert _Board().name == "faketarget"


def test_add_dependency_rejects_a_class_instead_of_an_instance():
    with pytest.raises(TypeError, match="must be an instance"):
        SourcesAndFlagsComponent().add_dependency(_Leaf)  # type: ignore[arg-type]


def test_add_dependency_rejects_a_duplicate_type():
    comp = SourcesAndFlagsComponent()
    comp.add_dependency(_Leaf())
    with pytest.raises(ValueError, match="already registered"):
        comp.add_dependency(_Leaf())


# --- the contribute chain ----------------------------------------------------


def test_calling_super_contribute_raises():
    """The MRO chain is driven for us. Calling super would register a base
    class's entries a second time, so the base's own body refuses."""

    class _Direct(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            super().contribute(target, profile)

    with pytest.raises(TypeError, match="do not call super"):
        _Direct().get_sources(_some_target(), _LIGHT)


# --- the target side --------------------------------------------------------
#
# A target is the root component of its own tree, so it declares its entries
# through the same hook, chained the same way. It is also passed to itself as
# the target the tree is queried for.


class _MiniTarget(DFBBTarget):
    """Just enough concrete target to resolve a component tree."""

    @property
    @override
    def name(self) -> str:
        return "minitarget"

    @property
    @override
    def target(self) -> str:
        return "faketarget-elf"

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [_LIGHT]


def _target_sources(target: AbstractTarget) -> list[str]:
    return _src_names(target.get_sources(target, target.profiles[0]))


def test_target_contribute_reaches_its_own_tree():
    class _T(_MiniTarget):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_gnat_source("from_target.ads")

    assert "from_target.ads" in _target_sources(_T())


def test_target_contribute_sees_the_profile():
    class _T(_MiniTarget):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_gnat_source(f"{profile.name}_target.ads")

    assert "light_target.ads" in _target_sources(_T())


def test_target_is_passed_to_itself_as_the_target():
    """A target is the root, so the tree it is queried for is its own."""
    seen: list[AbstractTarget] = []

    class _T(_MiniTarget):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            seen.append(target)
            self.add_gnat_source("seen.ads")

    instance = _T()
    _target_sources(instance)
    assert seen == [instance]


def test_a_target_cannot_be_a_dependency():
    """A target is the root of its own tree, never a node in someone else's."""
    with pytest.raises(TypeError, match="cannot be a dependency"):
        SourcesAndFlagsComponent().add_dependency(_MiniTarget())


def test_target_registering_entries_from_init_raises():
    """Same single-home rule on the target side."""

    class _Eager(_MiniTarget):
        def __init__(self) -> None:
            super().__init__()
            self.add_gnat_source("eager.ads")

    with pytest.raises(TypeError, match="outside contribute"):
        _Eager()
