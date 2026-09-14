#
# Copyright (C) 2026, AdaCore
#

"""Exercise profile_specializer.specialize and its private helpers."""

from pathlib import Path
from typing import cast, override

import pytest
from bb_runtimes_targets_gen.concrete_infrastructure import (
    RuntimeXmlSource,
    Target,
)
from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)
import bb_runtimes_targets_gen.concrete_infrastructure.def_gpr_xml_gen as xmlgen
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Loader,
    Profile,
    SourcesAndFlagsComponent,
)
from rts_prebuilder.end_user_data.compiler_selector import Compiler, set_compiler
from rts_prebuilder.engine.infrastructure_interface import generate_runtime_xml
from rts_prebuilder.engine.profile_specializer import (
    _build_system_ads_source_file,
    _check_unicity,
    runtime_loaders,
    specialize,
)


engine_interface.self_register()


def _profile(target: AbstractTarget, name: str) -> Profile:
    """The declared profile of ``target`` whose name is ``name``."""
    return next(p for p in target.profiles if p.name == name)


class _StubRuntimeXml:
    """Minimal runtime_xml_source stand-in: the generator calls render() and
    names ``path`` when it refuses an app-side registration."""

    path = Path("stub/runtime.xml")

    def render(self) -> str:
        return "<runtime/>"


class _FakeTarget(Target):
    """Supply profiles, loaders, and an optional runtime.xml without a concrete target."""

    def __init__(
        self,
        profiles=None,
        name="faketarget",
        *,
        runtime_xml_source=None,
        loaders=(),
        default_loader=None,
    ):
        super().__init__()
        self._profiles = (
            profiles
            if profiles is not None
            else [
                Profile(
                    name="light",
                    base_profile="light",
                    system_ads=Path("s.ads"),
                )
            ]
        )
        self._name = name
        self._runtime_xml_source = runtime_xml_source
        self._loaders = loaders
        self._default_loader = default_loader

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_loaders(*self._loaders)

    @property
    @override
    def name(self):
        return self._name

    @property
    @override
    def platform(self):
        return "bb"

    @property
    @override
    def has_double_precision_fpu(self):
        return False

    @property
    @override
    def profiles(self):
        return self._profiles

    @property
    @override
    def target(self):
        return f"{self._name}-elf"

    @property
    @override
    def runtime_xml_source(self):
        return self._runtime_xml_source

    @property
    @override
    def default_loader(self):
        return self._default_loader


class _EhFrameTarget(_FakeTarget):
    @property
    @override
    def uses_eh_frame_unwinding(self):
        return True


# --- specialize() integration ------------------------------------------------


def test_specialize_light_profile():
    target = _FakeTarget()
    profile = _profile(target, "light")
    spec = specialize(target, profile)

    assert spec.name == target.runtime_name_generator(profile)
    assert spec.profile_name == "light"
    assert spec.base_profile_name == profile.base_profile
    assert spec.libs_tuple == ("gnat",)
    assert spec.target_triple == target.target
    assert spec.is_native == target.is_native
    assert spec.system_ads_source_file.dest_path == Path("system.ads")
    assert spec.system_ads_source_file.dest_subdir == Path("gnat")


def test_specialize_tasking_profile_has_gnarl():
    class _Tasking(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_gnarl_sources("tasking.ads")

    target = _FakeTarget(
        profiles=[
            Profile(
                name="embedded",
                base_profile="embedded",
                system_ads=Path("s.ads"),
                components=[_Tasking()],
            )
        ]
    )
    spec = specialize(target, _profile(target, "embedded"))

    assert spec.libs_tuple == ("gnat", "gnarl")


def test_specialize_duplicate_component_selection_raises():
    # A component class appearing twice in the target's tree is a selection bug
    # specialize must reject.
    target = _FakeTarget()

    class _W1(SourcesAndFlagsComponent):
        pass

    class _W2(SourcesAndFlagsComponent):
        pass

    class _Dup(SourcesAndFlagsComponent):
        pass

    w1 = _W1()
    w1.add_dependency(_Dup())
    w2 = _W2()
    w2.add_dependency(_Dup())
    target.add_dependency(w1, w2)

    with pytest.raises(ValueError, match="selected more than once"):
        specialize(target, target.profiles[0])


# --- compiler-dependent runtime-build switches -------------------------------


def test_gcc_compiler_adds_callgraph_switch():
    target = _FakeTarget()
    set_compiler(Compiler.gnat)
    spec = specialize(target, _profile(target, "light"))
    assert "-fcallgraph-info=su,da" in spec.runtime_build_switches.common


def test_llvm_compiler_omits_callgraph_switch():
    target = _FakeTarget()
    set_compiler(Compiler.gnat_llvm)
    spec = specialize(target, _profile(target, "light"))
    assert "-fcallgraph-info=su,da" not in spec.runtime_build_switches.common


# --- _check_unicity ----------------------------------------------------------


def test_check_unicity_raises_on_duplicate_class():
    class _W1(SourcesAndFlagsComponent):
        pass

    class _W2(SourcesAndFlagsComponent):
        pass

    class _A(SourcesAndFlagsComponent):
        pass

    root = SourcesAndFlagsComponent()
    w1 = _W1()
    w1.add_dependency(_A())
    w2 = _W2()
    w2.add_dependency(_A())
    root.add_dependency(w1, w2)

    fake = _FakeTarget()
    with pytest.raises(ValueError, match="_A"):
        _check_unicity([root], fake, fake.profiles[0])


def test_check_unicity_allows_distinct_classes():
    class _A(SourcesAndFlagsComponent):
        pass

    class _B(SourcesAndFlagsComponent):
        pass

    root = SourcesAndFlagsComponent()
    root.add_dependency(_A(), _B())

    fake = _FakeTarget()
    _check_unicity([root], fake, fake.profiles[0])


# --- _build_system_ads_source_file -------------------------------------------


def _profile_with_system_ads(system_ads: str) -> Profile:
    return Profile(name="light", base_profile="light", system_ads=Path(system_ads))


def test_build_system_ads_source_file_bare_filename():
    source = _build_system_ads_source_file(_profile_with_system_ads("system_light.ads"))
    assert source.unresolved_src_path == Path("system/system_light.ads")
    assert source.dest_path == Path("system.ads")
    assert source.dest_subdir == Path("gnat")


def test_build_system_ads_source_file_with_directory_component():
    source = _build_system_ads_source_file(
        _profile_with_system_ads("boards/custom/system_x.ads")
    )
    assert source.unresolved_src_path == Path("boards/custom/system_x.ads")
    assert source.dest_path == Path("system.ads")
    assert source.dest_subdir == Path("gnat")


# --- runtime.xml generation ---------------------------------------------------


def test_runtime_xml_source_applies_substitutions(monkeypatch):
    monkeypatch.setattr(
        xmlgen, "resolve_and_read_file", lambda path: "v=@version@ t=@target@"
    )
    src = RuntimeXmlSource(Path("x.xml"), {"@version@": "1", "@target@": "fake"})
    assert src.render() == "v=1 t=fake"


def test_runtime_xml_generated_when_no_source():
    set_compiler(Compiler.gnat)
    target = _FakeTarget()
    xml = generate_runtime_xml(target, _profile(target, "light"))
    assert "package Linker" in xml


def test_runtime_xml_uses_declared_source_over_generation():
    target = _FakeTarget(runtime_xml_source=_StubRuntimeXml())
    assert generate_runtime_xml(target, target.profiles[0]) == "<runtime/>"


def test_eh_frame_target_keeps_unwind_symbol():
    set_compiler(Compiler.gnat_llvm)
    target = _EhFrameTarget()
    profile = Profile(
        name="embedded", base_profile="embedded", system_ads=Path("s.ads")
    )
    xml = generate_runtime_xml(target, profile)
    assert '"_Unwind_Find_FDE"' in xml


def test_gcc_gets_eh_frame_from_spec_not_explicit():
    set_compiler(Compiler.gnat)
    target = _EhFrameTarget()
    profile = Profile(
        name="embedded", base_profile="embedded", system_ads=Path("s.ads")
    )
    xml = generate_runtime_xml(target, profile)
    assert '"_Unwind_Find_FDE"' not in xml
    assert "link-zcx.spec" in xml


def test_declared_source_rejects_app_compile_switches():
    """A hand-maintained runtime.xml is copied as it stands, so an app-compile
    switch registered for that target has nowhere to land."""

    class _AppSwitch(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_switches(common_switches=("-DAPP",))

    target = _FakeTarget(runtime_xml_source=_StubRuntimeXml())
    target.add_dependency(_AppSwitch())

    with pytest.raises(ValueError, match="hand-maintained"):
        generate_runtime_xml(target, target.profiles[0])


def test_declared_source_rejects_app_link_switches():
    class _AppLink(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_app_switches(link_switches=("-lfoo",))

    target = _FakeTarget(runtime_xml_source=_StubRuntimeXml())
    target.add_dependency(_AppLink())

    with pytest.raises(ValueError, match="hand-maintained"):
        generate_runtime_xml(target, target.profiles[0])


def test_declared_source_allows_runtime_only_switches():
    # Those land in target_options.gpr, which the declared source says nothing
    # about.
    class _RuntimeSwitch(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_runtime_switches(
                common_switches=("-DRUNTIME",),
            )

    target = _FakeTarget(runtime_xml_source=_StubRuntimeXml())
    target.add_dependency(_RuntimeSwitch())

    assert generate_runtime_xml(target, target.profiles[0]) == "<runtime/>"


# --- loaders ------------------------------------------------------------------


def test_declared_loaders_become_the_project_arms():
    target = _FakeTarget(
        loaders=(
            Loader(
                "RAM",
                linker_scripts=(Path("load.ld"),),
                included_scripts=(Path("shared.ld"),),
            ),
            Loader("ROM"),
        )
    )
    xml = generate_runtime_xml(target, _profile(target, "light"))

    assert 'type Loaders is ("RAM", "ROM", "USER");' in xml
    assert 'Loader : Loaders := external("LOADER", "RAM");' in xml
    assert '("-T", "load.ld");' in xml
    assert '"-T", "shared.ld"' not in xml


def test_bare_loader_links_with_nothing_added():
    """USER is the bare loader every bare-metal runtime offers: its arm exists
    so the end user can select it, and adds no script of ours."""
    target = _FakeTarget()
    xml = generate_runtime_xml(target, _profile(target, "light"))

    assert 'when "USER" =>\n      end case;' in xml


def test_loader_link_switches_follow_its_scripts():
    target = _FakeTarget(
        loaders=(
            Loader(
                "RAM",
                linker_scripts=(Path("load.ld"),),
                link_switches=("-Wl,-u_entry",),
            ),
        )
    )
    xml = generate_runtime_xml(target, _profile(target, "light"))

    assert '("-T", "load.ld",\n               "-Wl,-u_entry");' in xml


def test_a_component_declares_loaders_too():
    """Loaders come off the whole tree, not off the target alone."""

    class _Qspi(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_loaders(Loader("QSPI", linker_scripts=(Path("qspi.ld"),)))

    target = _FakeTarget(loaders=(Loader("RAM"),))
    target.add_dependency(_Qspi())

    names = [
        loader.name for loader in runtime_loaders(target, _profile(target, "light"))
    ]

    assert names == ["RAM", "QSPI"]


def test_two_loaders_of_the_same_name_raise():
    class _AnotherRam(SourcesAndFlagsComponent):
        @override
        def contribute(self, target: AbstractTarget, profile: Profile) -> None:
            self.add_loaders(Loader("RAM", linker_scripts=(Path("other.ld"),)))

    target = _FakeTarget(loaders=(Loader("RAM"),))
    target.add_dependency(_AnotherRam())

    with pytest.raises(ValueError, match="more than once"):
        runtime_loaders(target, _profile(target, "light"))


def test_default_loader_names_the_arm_the_project_starts_from():
    target = _FakeTarget(
        loaders=(Loader("FIRST"), Loader("SECOND")), default_loader="SECOND"
    )
    xml = generate_runtime_xml(target, _profile(target, "light"))

    assert 'Loader : Loaders := external("LOADER", "SECOND");' in xml


def test_default_loader_that_nothing_offers_raises():
    with pytest.raises(ValueError, match="is not offered"):
        xmlgen._default_loader(_stub_target("MISSING"), ["RAM", "USER"])


class _LoaderStub:
    """Just what _offered_loaders and _default_loader read off a target."""

    name = "stub"

    def __init__(self, default_loader: str | None, is_os_target: bool) -> None:
        self.default_loader = default_loader
        self.is_os_target = is_os_target


def _stub_target(
    default_loader: str | None = None, is_os_target: bool = False
) -> AbstractTarget:
    return cast(AbstractTarget, _LoaderStub(default_loader, is_os_target))


def test_bare_metal_offers_user_after_the_declared_loaders():
    """USER belongs to bare-metal linking, not to a target, so every bare-metal
    runtime gets it without repeating it."""
    declared = (Loader("ROM", linker_scripts=(Path("rom.ld"),)),)

    offered = xmlgen._offered_loaders(_stub_target(), declared)

    assert [loader.name for loader in offered] == ["ROM", "USER"]


def test_os_target_offers_only_what_it_declares():
    # An OS links with its own tooling: no user-supplied script to allow for.
    offered = xmlgen._offered_loaders(_stub_target(is_os_target=True), ())

    assert offered == ()


def test_target_declaring_user_itself_raises():
    with pytest.raises(Exception, match="USER loader cannot be defined"):
        xmlgen._offered_loaders(_stub_target(), (Loader("USER"),))
