#
# Copyright (C) 2025-2026, AdaCore
#

from collections import Counter
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    GprSwitches,
    Loader,
    Profile,
    SourceFile,
    SourcesAndFlagsComponent,
)
from rts_prebuilder.base_types import GnatOrGnarlType, UnresolvedPath
from rts_prebuilder.end_user_data.logger import get_logger

from .infrastructure_interface import generate_runtime_xml, get_resolver
from .target_checker import check_loader_names
from .runtime_spec import RuntimeSpec

log = get_logger(__name__)


def specialize(target: AbstractTarget, profile: Profile) -> RuntimeSpec:
    """
    Specialize a profile-independent target into one profile's RuntimeSpec.

    All the profile-driven data lives on the ``Profile``: its base profile, the
    system.ads, and the components that make up the runtime. The runtime's
    sources and flags are the target's own always-on tree merged with the
    profile's components (``profile.components``). This resolves both for the
    profile and asks the infrastructure to render the runtime.xml. The result
    is a fully resolved ``RuntimeSpec``, so the writer never needs the target.
    """
    log.info(
        "--> Specializing target '%s' (%s) for profile '%s'",
        target.name,
        target.__class__.__name__,
        profile.name,
    )

    # The target is the always-on root. The profile adds its own components.
    roots = [target, *profile.components]
    _check_unicity(roots, target, profile)

    all_sources: list[SourceFile] = target_data_sources(target, profile)
    profile_sources: list[SourceFile] = []
    runtime_build_switches = GprSwitches()
    runtime_gnarl_switches: list[str] = []
    runtime_debug_switches: list[str] = []
    for root in roots:
        sources = root.get_sources(target, profile)
        if root is not target:
            profile_sources.extend(sources)
        all_sources.extend(sources)
        runtime_build_switches = runtime_build_switches.merge(
            root.get_runtime_switches(target, profile)
        )
        runtime_gnarl_switches += root.get_runtime_gnarl_switches(target, profile)
        runtime_debug_switches += root.get_runtime_debug_switches(target, profile)

    # Search paths: any component may declare where its sources are found.
    search_paths: list[Path] = []
    for root in roots:
        search_paths.extend(root.get_sources_search_paths(target, profile))
    if search_paths:
        log.info(
            "- Adding source search paths for target %s: %s",
            target.name,
            search_paths,
        )
    get_resolver().add_search_paths(*search_paths)

    return RuntimeSpec(
        name=target.runtime_name_generator(profile),
        profile_name=profile.name,
        base_profile_name=profile.base_profile,
        libs_tuple=libs_tuple(profile_sources),
        sources=all_sources,
        runtime_build_switches=runtime_build_switches,
        runtime_gnarl_switches=runtime_gnarl_switches,
        runtime_debug_switches=runtime_debug_switches,
        system_ads_source_file=_build_system_ads_source_file(profile),
        runtime_xml=generate_runtime_xml(target, profile),
        template_config=target.template_config,
        target_triple=target.target,
        is_native=target.is_native,
    )


def libs_tuple(profile_sources: list[SourceFile]) -> tuple[GnatOrGnarlType, ...]:
    """The libraries this runtime provides, from what its profile installs.

    Only the profile's components are looked at. A target carries its BSP
    tasking sources in its always-on tree whatever runtime is being built, so
    their presence says nothing. The profile's components are what a tasking
    runtime and a non-tasking one differ by.
    """
    if any(s.dest_subdir == Path("gnarl") for s in profile_sources):
        return ("gnat", "gnarl")
    return ("gnat",)


def target_data_sources(target: AbstractTarget, profile: Profile) -> list[SourceFile]:
    """Build the sources a target installs from its plain data instead of from
    a component. Those are the rebuild scripts, the readme, the linker spec,
    and the linker scripts its loaders name.

    A loader names its scripts, so each path is written once where the loader
    is declared. Arms name a shared script each, hence the deduplication.
    """
    sources: list[SourceFile] = []

    if target.build_script is not None and target.prebuild_script is not None:
        sources += [
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(target.build_script),
                dest_path=Path("build.py"),
            ),
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(target.prebuild_script),
                dest_path=Path("prepare_build.py"),
            ),
        ]

    if target.readme_file is not None:
        sources.append(
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(target.readme_file),
                dest_path=Path("README"),
            )
        )

    scripts = dict.fromkeys(
        script
        for loader in runtime_loaders(target, profile)
        for script in (*loader.linker_scripts, *loader.included_scripts)
    )
    sources += [
        SourceFile(
            unresolved_src_path=UnresolvedPath(script),
            dest_subdir=Path("ld"),
        )
        for script in scripts
    ]

    link_spec = target.link_spec_file(profile.base_profile)
    if link_spec is not None:
        sources.append(
            SourceFile(
                dest_subdir=None,
                unresolved_src_path=UnresolvedPath(link_spec),
                dest_path=Path(link_spec.name),
            )
        )

    return sources


def runtime_loaders(target: AbstractTarget, profile: Profile) -> tuple[Loader, ...]:
    """Every loader one runtime offers: the ones the target's own tree declares,
    then the ones the profile's components do.

    :param target: the target the runtime is being built for.
    :param profile: the profile the runtime is being built for.
    :returns: the loaders, in offering order.
    :raises ValueError: if two of them share a name. They are the arms of one
        ``case`` statement and the values of one gpr type, so a name has to
        identify exactly one loader.
    """
    loaders: list[Loader] = []
    for root in (target, *profile.components):
        loaders.extend(root.get_loaders(target, profile))
    check_loader_names(target, profile, loaders)
    return tuple(loaders)


def _check_unicity(
    roots: list[SourcesAndFlagsComponent],
    target: AbstractTarget,
    profile: Profile,
) -> None:
    """
    Raise if the same component class appears twice across the runtime's
    components. That covers the target's always-on tree and the profile's own
    components.

    Two instances of one component class carry the same sources, so a duplicate
    is always a selection bug. Without this, it would only surface as duplicate
    installation warnings.
    """
    counts: Counter[type[SourcesAndFlagsComponent]] = Counter()
    stack = list(roots)
    while stack:
        component = stack.pop()
        counts[type(component)] += 1
        stack.extend(component.get_dependencies())

    duplicated = sorted(klass.__name__ for klass, n in counts.items() if n > 1)
    if duplicated:
        raise ValueError(
            f"Component(s) selected more than once for runtime"
            f" {target.name}/{profile.name}: {duplicated}"
        )


def _build_system_ads_source_file(profile: Profile) -> SourceFile:
    """Build the system.ads source file the runtime installs, from the
    profile's ``system_ads``."""

    # By default, system.ads files are searched for in bb-runtimes/src/system.
    # For custom runtimes the path may instead carry a directory component, in
    # which case we look it up as a regular source file.
    if profile.system_ads.parent == Path("."):
        system_ads_path = Path("system") / profile.system_ads
    else:
        system_ads_path = profile.system_ads

    log.info("- system.ads for profile %s: %s", profile.name, system_ads_path)

    return SourceFile(
        # "Cast" to UnresolvedPath to indicate that this path still needs to go
        # through the resolver.
        unresolved_src_path=UnresolvedPath(system_ads_path),
        dest_path=Path("system.ads"),
        dest_subdir=Path("gnat"),
    )
