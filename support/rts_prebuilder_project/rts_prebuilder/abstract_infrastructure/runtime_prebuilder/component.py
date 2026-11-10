#
# Copyright (C) 2025-2026, AdaCore
#

"""
Composable node holding source files and switches (the Composite pattern).

Components are built from each other by inheritance. A subclass keeps what its
bases register and adds its own on top. ``__init__`` and ``contribute`` chain
across the whole class hierarchy, base first. No override calls ``super``:
``_run_hook_chain`` does the chaining, so each override declares only its own
slice and a forgotten call cannot drop a base's entries.

Components also depend on each other. A component registers source files and
switches, and it owns dependencies. A query returns this component's entries
plus every dependency's, recursively. Targets and their shared-source
components are built out of these.

A component has two kinds of state. Each has one place it may be declared in,
and a call from the wrong place raises:

* structure, declared in ``__init__``: the dependency graph. It describes the
  component itself, not the runtime being built. It is fixed once per instance,
  and is the same for every runtime queried.
* entries, declared in ``contribute``: every source file and switch, via the
  ``add_*`` helpers. ``contribute`` runs once per ``(target, profile)``, so
  entries resolve per runtime.

What varies per runtime is what a component contributes, and which components a
profile carries (``Profile.components``). The shape of the graph never varies.

Every aggregated attribute is a channel (see ``_Channel``). A channel says how
to make an empty accumulator, how to combine two values, and in what order to
fold. ``_register`` and ``_collect`` do the work for all of them, and the public
``add_*`` and ``get_*`` methods are thin typed wrappers. Channels differ only in
their ``_Channel`` config, which is what keeps them all behaving alike:

* a channel holds no lasting state. A registration lands in the buffer
  ``contribute`` is filling, so every query recomputes from scratch.
* every ``add_*`` takes ``replace_channel=``. It drops this node's own entries
  for that channel before adding. That is the override knob. A -mcpu variant
  subclass uses it to drop the base's -mcpu. Entries have a single home, so the
  flag reaches everything this node registered, whoever registered it.
* derived views (``get_linker_scripts``, ``get_sources_of_type``) are computed
  from channels. They hold no storage of their own.
"""

import functools
import inspect
import logging
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Any,
    Concatenate,
    ParamSpec,
    TypeVar,
    cast,
    final,
    override,
)

from rts_prebuilder.base_types import UnresolvedPath
from rts_prebuilder.end_user_data.logger import get_logger

from ..common.source_file import (
    GprSwitches,
    Loader,
    SourceFile,
)

if TYPE_CHECKING:
    from .abstract_target import AbstractTarget
    from .runtime_profile import Profile

logger = get_logger(__name__)


class IncompatibleComponent(Exception):
    """Raised by ``contribute`` when a platform-specific component is run for a
    target whose platform it does not support. Such a component is only ever
    selected for its own platform, so this signals a mis-selection."""


def _added_from() -> str:
    """The first stack frame outside this module, which is the ``__init__`` that
    composed the dependency. Reads ``"<file>:<line> (<function>)"``, or
    ``"<unknown>"``."""
    frame = inspect.currentframe()
    here = frame.f_code.co_filename if frame else __file__
    f = frame.f_back if frame else None
    while f is not None:
        if f.f_code.co_filename != here:
            return f"{f.f_code.co_filename}:{f.f_lineno} ({f.f_code.co_name})"
        f = f.f_back
    return "<unknown>"


def _concat(acc: Any, other: Any) -> Any:
    """Combine for list channels: ``acc`` then ``other``, in a fresh list."""
    return acc + other


def _run_hook_chain(obj: Any, hook: str, sentinel: type, *args: Any) -> None:
    """Call every class's own ``hook`` once, base-to-derived across ``obj``'s MRO.

    The lookup reads each class's own ``__dict__``, so an inherited
    implementation never runs twice. ``sentinel``'s own hook is the "you called
    super" tripwire, so it is skipped.
    """
    for klass in reversed(type(obj).__mro__):
        if klass is sentinel:
            continue
        impl = klass.__dict__.get(hook)
        if impl is not None:
            impl(obj, *args)


_P = ParamSpec("_P")
_R = TypeVar("_R")


def _init_only(
    method: "Callable[Concatenate[SourcesAndFlagsComponent, _P], _R]",
) -> "Callable[Concatenate[SourcesAndFlagsComponent, _P], _R]":
    """Refuse a structural call made from ``contribute``. The graph the
    component was built from is already fixed by then."""

    @functools.wraps(method)
    def guarded(
        self: "SourcesAndFlagsComponent", *args: _P.args, **kwargs: _P.kwargs
    ) -> _R:
        if self._buffers is not None:
            raise TypeError(
                f"{type(self).__name__}.{method.__name__}() is structural and "
                f"cannot be called from here — move it to __init__"
            )
        return method(self, *args, **kwargs)

    return cast("Callable[Concatenate[SourcesAndFlagsComponent, _P], _R]", guarded)


def _prepend(acc: Any, other: Any) -> Any:
    """Combine for the search-path channel: ``other`` first, so a later
    registration takes precedence."""
    return other + acc


def _merge_switches(acc: Any, other: Any) -> Any:
    """Combine for the two ``GprSwitches`` channels: ``acc``'s switches first
    within each field."""
    return acc.merge(other)


@dataclass(frozen=True)
class _Channel:
    """Config for one tree-aggregated attribute."""

    empty: Callable[[], Any]
    """Makes a fresh accumulator."""

    combine: Callable[[Any, Any], Any]
    """Merges two values, both to fold one registered contribution in and to
    fold a node's own value with its dependencies'."""

    dependencies_first: bool = False
    """Fold the dependencies before this node's own value, so defaults lead.
    Otherwise own leads."""


class SourcesAndFlagsComponent:
    """
    A node of the sources/switches Composite tree.

    It registers SourceFile and GprSwitches objects. It also owns dependencies,
    composed in with ``add_dependency``. A query returns this node's own entries
    merged with every dependency's, recursively.

    The two halves live in different places, and each rejects the other's:

    * structure, in ``__init__``: ``add_dependency`` and friends refuse a call
      from ``contribute``. They describe the component, so they are fixed once
      per instance and the same for every runtime queried.
    * entries, in ``contribute``: every ``add_*`` refuses a call from anywhere
      else. It is re-run per (target, profile), so a component's sources and
      switches may vary by runtime.

    Composition is imperative and additive: a component adds to whatever its
    base classes composed, and nothing can take a dependency back out. A base
    that some subclass does not want a dependency from must not compose it at
    all. The subclasses that do want it compose it themselves.

    So a component that applies to only some runtimes is not gated by pruning
    the graph. Either the profiles that want it carry it
    (``Profile.components``), or it gates itself in its own ``contribute``.

    Two things hold for every method here, so neither is repeated below:

    * a query takes the ``(target, profile)`` of the runtime being built. It
      passes them to each node's ``contribute``, so a component can answer
      differently per runtime.
    * a registration takes ``replace_channel``. It drops what this node had put
      in that channel before adding, base classes included.
    """

    # The aggregated attributes, each a channel. Adding an attribute = adding an
    # entry here plus a typed add_*/get_* wrapper. The mechanism is shared.
    #   - list channels (sources, gpr_externals, loaders): concat, own-first.
    #   - sources_search_paths: concat too, but the other way round. A search
    #     path is a precedence, so a later registration must win.
    #   - app_switches and runtime_build_switches: the two builds, holding the same
    #     GprSwitches type since a gpr project splits switches the same way on
    #     either side. The runtime one folds dependencies-first so the
    #     infrastructure defaults (a dependency) lead every field.
    #   - runtime_gnarl_switches and runtime_debug_switches: flat lists, because the
    #     axis is not the language. Gnarl is a source scope and debug a build
    #     variant, and target_options.gpr applies each across every language.
    #     They become GprSwitches the day one of them needs a language split,
    #     which means giving the template the slots first.
    # linker_scripts is not a channel: it derives from the sources one.
    _CHANNELS: dict[str, _Channel] = {
        "sources": _Channel(list, _concat),
        "gpr_externals": _Channel(list, _concat),
        "loaders": _Channel(list, _concat),
        "sources_search_paths": _Channel(list, _prepend),
        "app_switches": _Channel(GprSwitches, _merge_switches),
        "runtime_build_switches": _Channel(
            GprSwitches, _merge_switches, dependencies_first=True
        ),
        "runtime_gnarl_switches": _Channel(list, _concat, dependencies_first=True),
        "runtime_debug_switches": _Channel(list, _concat, dependencies_first=True),
    }

    _buffers: dict[str, Any] | None
    """Per-channel accumulators, live only while ``contribute`` runs. Channels
    have no other storage, so this being ``None`` is exactly what "no contribute
    run is active" means — and what makes an ``add_*`` from the wrong place an
    error rather than a write nobody reads."""

    _dependencies: list["SourcesAndFlagsComponent"]
    """Dependencies composed in ``__init__``. Their entries merge into this
    component's when queried (recursively). Fixed for the instance's lifetime —
    composing from ``contribute`` is refused."""

    def __init__(self) -> None:
        """Initialize the structural state. Channels get no storage here. They
        live only for the duration of a ``contribute`` run."""
        self._buffers = None
        self._dependencies = []

    @override
    def __init_subclass__(cls, **kwargs: Any) -> None:
        """Refuse a component deriving from another: reuse is composition, and a
        subclass cannot drop what its base composed. Targets are the exception,
        their hierarchy carrying properties nothing can compose.

        :raises TypeError: if a base is a component other than this class or a
            target.
        """
        super().__init_subclass__(**kwargs)

        # Local, and still absent while abstract_target is importing: that is
        # the module defining the one class this allows deriving from.
        try:
            from .abstract_target import AbstractTarget
        except ImportError:
            return

        for base in cls.__bases__:
            if not issubclass(base, SourcesAndFlagsComponent):
                continue
            if base is SourcesAndFlagsComponent or issubclass(base, AbstractTarget):
                continue
            raise TypeError(
                f"{cls.__name__} derives from the component {base.__name__};"
                f" compose it with add_dependency instead, since a subclass"
                f" cannot drop what its base composes or contributes"
            )

    @final
    def _log_dependency_added(self, type_name: str) -> None:
        """Report a composition and the ``__init__`` it was made from."""
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(
                "dependency added: %s on %s (from %s)",
                type_name,
                type(self).__name__,
                _added_from(),
            )

    @_init_only
    def add_dependency(self, *components: "SourcesAndFlagsComponent") -> None:
        """Append dependencies, whose entries merge into this component's when
        queried, recursively.

        Compose only what holds for every user of this component. The graph is
        fixed once, in ``__init__``. Anything that applies to some runtimes only
        has to be decided where the runtime is. Either the profile carries the
        component, or the component gates itself in its own ``contribute``.

        At most one dependency of a given type is allowed. Two instances of one
        component class carry the same sources, so a duplicate is a selection
        bug.

        :raises TypeError: if called from ``contribute``, if a dependency is not
            a component instance (passing the class is the usual slip), or if it
            is a target.
        :raises ValueError: if a dependency of the same type is already there.
        """
        # Local: a target is a component, so abstract_target imports us.
        from .abstract_target import AbstractTarget

        for component in components:
            if not isinstance(component, SourcesAndFlagsComponent):
                raise TypeError(
                    f"dependency {component!r} must be an instance (not a class)"
                    f" of SourcesAndFlagsComponent, not {type(component)}"
                )
            if isinstance(component, AbstractTarget):
                raise TypeError(
                    f"a target ({type(component).__name__}) is the root of its "
                    f"own tree and cannot be a dependency"
                )
            if any(type(c) is type(component) for c in self._dependencies):
                raise ValueError(
                    f"a dependency of type {type(component).__name__} is "
                    f"already registered"
                )
            self._dependencies.append(component)
            self._log_dependency_added(type(component).__name__)

    @final
    def get_dependencies(self) -> list["SourcesAndFlagsComponent"]:
        """This component's direct dependencies, in a fresh list the caller may
        keep. It takes no ``(target, profile)`` because only what a dependency
        contributes varies per runtime, never whether it is there."""
        return list(self._dependencies)

    # --- generic channel mechanism ------------------------------------------

    @final
    def _register(
        self, channel: str, contribution: Any, replace_channel: bool = False
    ) -> None:
        """Fold one contribution, shaped like the channel's accumulator, into
        the buffer the active ``contribute`` run is filling.

        Every ``add_*`` goes through here, which is what keeps entries to their
        single home. Outside a run there is no buffer to write to. Raising beats
        dropping the entry in silence.
        """
        store = self._buffers
        if store is None:
            raise TypeError(
                f"{type(self).__name__}: cannot register {channel} outside "
                f"contribute — entries are declared in contribute(target, "
                f"profile), so every add_* helper must be called from there"
            )
        if replace_channel:
            store[channel] = self._CHANNELS[channel].empty()
        store[channel] = self._CHANNELS[channel].combine(store[channel], contribution)

    @final
    def _run_contribute(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> dict[str, Any]:
        """Run every class's ``contribute`` once, base-to-derived across the MRO,
        into fresh per-channel buffers. Nothing outlives the call, so one
        instance is safely reused across runtimes."""
        self._buffers = {
            name: channel.empty() for name, channel in self._CHANNELS.items()
        }
        try:
            _run_hook_chain(
                self, "contribute", SourcesAndFlagsComponent, target, profile
            )
            return self._buffers
        finally:
            self._buffers = None

    @final
    def _collect(
        self, channel: str, target: "AbstractTarget", profile: "Profile"
    ) -> Any:
        """This node's value for ``channel`` merged with every dependency's,
        recursively, using the channel's combine and fold order."""
        spec = self._CHANNELS[channel]
        own = self._run_contribute(target, profile)[channel]
        dependencies = spec.empty()
        for component in self._dependencies:
            dependencies = spec.combine(
                dependencies, component._collect(channel, target, profile)
            )
        # combine is associative, so folding the dependencies together first and
        # then around ``own`` is the same as folding them in one at a time.
        if spec.dependencies_first:
            return spec.combine(dependencies, own)
        return spec.combine(own, dependencies)

    def contribute(self, target: "AbstractTarget", profile: "Profile") -> None:
        """Override to declare the entries this class contributes, with the
        ``self.add_*`` helpers. This is the only place they may be registered.
        The helpers raise anywhere else. Put them all here, whether or not
        they depend on the ``(target, profile)``. Structure does not belong
        here. Compose dependencies in ``__init__``.

        Do not call ``super``: every class's ``contribute`` is chained for you,
        base-to-derived. Do not stash ``target`` or ``profile`` on ``self``
        either, since one instance may be queried for several runtimes.

        A component that overrides nothing contributes nothing. The driver skips
        this body, which exists only to catch the ``super()`` call.

        :raises TypeError: always. Reaching it means a direct subclass called
            ``super().contribute(...)``. The same call deeper in a chain lands
            on an intermediate class's override instead, and silently registers
            that class's entries twice.
        """
        raise TypeError(
            "do not call super().contribute(): every class's contribute is "
            "chained for you, base-to-derived"
        )

    # --- registration --------------------------------------------------------

    @final
    def append_sources(
        self, *sources: SourceFile, replace_channel: bool = False
    ) -> None:
        """Register source files. ``replace_channel`` drops the whole
        ``sources`` channel. That covers gnat, gnarl, linker scripts and root
        files, not only the kind being added."""
        self._register("sources", list(sources), replace_channel)

    @final
    def add_app_switches(
        self,
        *,
        common_switches: Sequence[str] = (),
        c_switches: Sequence[str] = (),
        asm_switches: Sequence[str] = (),
        link_switches: Sequence[str] = (),
        replace_channel: bool = False,
    ) -> None:
        """Register switches for the end user's build against the runtime, one
        keyword per ``GprSwitches`` field. They land in the generated
        ``runtime.xml``. The link ones apply whichever loader the program
        picks. For what a single loader adds, see ``Loader.link_switches``.

        :raises ValueError: if no switch is given.
        """
        self._register_switches(
            ["app_switches"],
            GprSwitches(
                common=tuple(common_switches),
                c=tuple(c_switches),
                asm=tuple(asm_switches),
                link=tuple(link_switches),
            ),
            replace_channel,
        )

    @final
    def add_runtime_switches(
        self,
        *,
        common_switches: Sequence[str] = (),
        c_switches: Sequence[str] = (),
        asm_switches: Sequence[str] = (),
        link_switches: Sequence[str] = (),
        gnarl_switches: Sequence[str] = (),
        debug_switches: Sequence[str] = (),
        replace_channel: bool = False,
    ) -> None:
        """Register switches for the runtime's own build, one keyword per
        ``GprSwitches`` field. They land in the generated
        ``target_options.gpr``, which ``runtime_build.gpr`` and
        ``ravenscar_build.gpr`` read. The link ones apply only when the runtime
        is built as a shared library (``LIBRARY_TYPE`` dynamic).

        Gnarl and debug are not fields of the set. They narrow on something
        other than the language, so the project applies each across every
        language. Gnarl reaches the tasking half alone (libgnarl). Debug reaches
        the Debug and Gnatcov builds alone.

        :raises ValueError: if no switch is given.
        """
        self._register_switches(
            ["runtime_build_switches"],
            GprSwitches(
                common=tuple(common_switches),
                c=tuple(c_switches),
                asm=tuple(asm_switches),
                link=tuple(link_switches),
            ),
            replace_channel,
            allow_empty=bool(gnarl_switches or debug_switches),
        )
        if gnarl_switches:
            self._register(
                "runtime_gnarl_switches", list(gnarl_switches), replace_channel
            )
        if debug_switches:
            self._register(
                "runtime_debug_switches", list(debug_switches), replace_channel
            )

    @final
    def add_app_and_runtime_switches(
        self,
        *,
        common_switches: Sequence[str] = (),
        c_switches: Sequence[str] = (),
        asm_switches: Sequence[str] = (),
        replace_channel: bool = False,
    ) -> None:
        """Register switches for both builds at once, which is why a switch both
        builds need is written here once rather than twice. The target's arch
        identity (-mcpu/-march/-mabi) is the usual case.

        The link is left out: the two builds link different things, so a switch
        for one is rarely right for the other. Say which one you mean with
        ``add_app_switches`` or ``add_runtime_switches``.

        :param replace_channel: clears both destinations whole, every field, not
            only the ones this call names.
        :raises ValueError: if no switch is given.
        """
        self._register_switches(
            ["app_switches", "runtime_build_switches"],
            GprSwitches(
                common=tuple(common_switches),
                c=tuple(c_switches),
                asm=tuple(asm_switches),
            ),
            replace_channel,
        )

    @final
    def _register_switches(
        self,
        channels: list[str],
        switches: GprSwitches,
        replace_channel: bool,
        allow_empty: bool = False,
    ) -> None:
        """Land one ``add_*_switches`` call on the channels of the builds it is
        for. ``allow_empty`` says the call carried switches this set does not
        hold, so an empty set is not an empty call."""
        if not switches:
            if allow_empty:
                return
            raise ValueError("no switch given")
        for channel in channels:
            self._register(channel, switches, replace_channel)

    @final
    def add_gpr_external(
        self, gpr_name: str, env_name: str, replace_channel: bool = False
    ) -> None:
        """Declare ``gpr_name := external ("env_name");`` in the generated
        project. A switch can then read an environment value, instead of the
        target patching generated text. The generator emits it once."""
        self._register("gpr_externals", [(gpr_name, env_name)], replace_channel)

    @final
    def add_loaders(self, *loaders: Loader, replace_channel: bool = False) -> None:
        """Declare link configurations, in offering order, that the end user
        chooses between with ``-XLOADER``. Each becomes one arm of the generated
        project's ``case``."""
        self._register("loaders", list(loaders), replace_channel)

    @final
    def prepend_sources_search_paths(self, *paths: Path) -> None:
        """Prepend search paths for this component's sources, taking precedence
        over what is already there. Its channel combines the other way round
        from the rest, which is why there is no ``replace_channel``."""
        self._register("sources_search_paths", list(paths))

    # --- source-registration sugar (all forward replace_channel) -------------

    @final
    def append_sources_for_dir(
        self, subdir: Path, *sources: Path, replace_channel: bool = False
    ) -> None:
        """Append sources that all install into ``subdir``, marking each path
        unresolved so the caller does not have to."""
        unresolved_paths_list = [UnresolvedPath(s) for s in sources]
        self.append_sources(
            *[
                SourceFile(dest_subdir=subdir, unresolved_src_path=src)
                for src in unresolved_paths_list
            ],
            replace_channel=replace_channel,
        )

    @final
    def add_gnarl_sources(self, *sources: str, replace_channel: bool = False) -> None:
        """Register libgnarl source files, which install under ``gnarl``."""
        self.append_sources_for_dir(
            Path("gnarl"), *[Path(s) for s in sources], replace_channel=replace_channel
        )

    @final
    def add_gnat_sources(self, *sources: str, replace_channel: bool = False) -> None:
        """Register libgnat source files, which install under ``gnat``."""
        self.append_sources_for_dir(
            Path("gnat"), *[Path(s) for s in sources], replace_channel=replace_channel
        )

    @final
    def _add_source_to_dir(
        self,
        subdir: Path,
        source: str,
        dest_name: str | None,
        replace_channel: bool = False,
    ) -> None:
        """Register a single source into ``subdir``, under ``dest_name`` or
        under its own name."""
        self.append_sources(
            SourceFile(
                dest_subdir=subdir,
                unresolved_src_path=UnresolvedPath(Path(source)),
                dest_path=Path(dest_name) if dest_name is not None else None,
            ),
            replace_channel=replace_channel,
        )

    @final
    def add_gnat_source(
        self, source: str, dest_name: str | None = None, replace_channel: bool = False
    ) -> None:
        """Register one libgnat source, installed under ``dest_name`` or under
        its own name."""
        self._add_source_to_dir(Path("gnat"), source, dest_name, replace_channel)

    @final
    def add_gnarl_source(
        self, source: str, dest_name: str | None = None, replace_channel: bool = False
    ) -> None:
        """Register one libgnarl source, installed under ``dest_name`` or under
        its own name."""
        self._add_source_to_dir(Path("gnarl"), source, dest_name, replace_channel)

    # --- typed queries (thin wrappers over _collect) ------------------------

    @final
    def get_sources(
        self,
        target: "AbstractTarget",
        profile: "Profile",
        dest_subdir: Path | None = None,
        exclude_subdirs: list[Path] | None = None,
    ) -> list[SourceFile]:
        """This node's sources merged with every dependency's, recursively,
        keeping only ``dest_subdir`` and dropping ``exclude_subdirs`` when
        either is given."""
        return [
            s
            for s in self._collect("sources", target, profile)
            if not (exclude_subdirs and s.dest_subdir in exclude_subdirs)
            and (dest_subdir is None or s.dest_subdir == dest_subdir)
        ]

    @final
    def get_sources_of_type(
        self,
        target: "AbstractTarget",
        profile: "Profile",
        filter_type: type[SourceFile],
    ) -> list[SourceFile]:
        """The sources across the subtree that are instances of
        ``filter_type``."""
        return [
            s for s in self.get_sources(target, profile) if isinstance(s, filter_type)
        ]

    @final
    def get_app_switches(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> GprSwitches:
        """This node's switches for the end user's build merged with every
        dependency's. They go into runtime.xml. They apply when the end user
        compiles and links against the runtime, never when it is built."""
        return cast(GprSwitches, self._collect("app_switches", target, profile))

    @final
    def get_runtime_switches(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> GprSwitches:
        """This node's switches for the runtime's own build, merged with every
        dependency's. They go into target_options.gpr. Dependencies come first
        in every field, so the infrastructure defaults lead and a target's own
        additions follow."""
        return cast(
            GprSwitches, self._collect("runtime_build_switches", target, profile)
        )

    @final
    def get_runtime_gnarl_switches(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> list[str]:
        """This node's gnarl switches merged with every dependency's: the ones
        target_options.gpr applies to the tasking half alone, whatever the
        language."""
        return cast(list[str], self._collect("runtime_gnarl_switches", target, profile))

    @final
    def get_runtime_debug_switches(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> list[str]:
        """This node's debug switches merged with every dependency's: the ones
        target_options.gpr applies under ``BUILD`` Debug or Gnatcov alone,
        whatever the language."""
        return cast(list[str], self._collect("runtime_debug_switches", target, profile))

    @final
    def get_gpr_externals(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> list[tuple[str, str]]:
        """This node's gpr external variables merged with every dependency's,
        each a ``(gpr_name, env_name)`` pair the generated project declares
        before the switches that read it."""
        return cast(
            list[tuple[str, str]], self._collect("gpr_externals", target, profile)
        )

    @final
    def get_loaders(self, target: "AbstractTarget", profile: "Profile") -> list[Loader]:
        """This node's loaders merged with every dependency's, in offering
        order. They are not checked here. Only the engine sees every node, so
        only it can say they make a usable set."""
        return cast(list[Loader], self._collect("loaders", target, profile))

    @final
    def get_sources_search_paths(
        self, target: "AbstractTarget", profile: "Profile"
    ) -> list[Path]:
        """This node's source search paths merged with every dependency's."""
        return cast(list[Path], self._collect("sources_search_paths", target, profile))
