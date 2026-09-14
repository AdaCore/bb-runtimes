.. Copyright (C) 2025-2026, AdaCore

===============================
Extending with Your Own Target
===============================

This guide explains how to extend target definitions for your board, CPU or
platform, reusing an infrastructure's components and source repositories.
Keep your extension's target definitions and sources in your own project.
The guide is self-contained. For the architecture of the repository and both
Python packages, see :doc:`../overview/3_architecture`.

Two packages are in play, and the difference matters on every page here.
``rts_prebuilder`` gives you the base classes and the registration verbs. Your
infrastructure gives you a pre-filled target base, the profile factories, and the
component catalog to compose from. The
:doc:`rts_prebuilder API reference <../api/extenders_api>` documents the base
contracts. Consult your infrastructure's documentation for its concrete
classes and factories.

This guide carries what a component may register, what a target declares and
what a loader owns, and says what each call is for. The API pages carry the
signature, the arguments and what each one raises:

* :doc:`../api/concrete_infrastructure_extenders/components` — the
  registrations, on
  :class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`.
* :doc:`../api/concrete_infrastructure_extenders/target_definition` — the
  properties a target declares, on
  :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`.
  ``AbstractTarget`` derives from
  :class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`, so
  a target is also a component: it depends on other components and registers
  its own sources and switches, with the same calls as the bullet above.
* :doc:`../api/concrete_infrastructure_extenders/source_files` — the value types
  those calls take and return:
  :class:`~rts_prebuilder.abstract_infrastructure.SourceFile`,
  :class:`~rts_prebuilder.abstract_infrastructure.Loader`,
  :class:`~rts_prebuilder.abstract_infrastructure.GprSwitches`.
* :doc:`../api/concrete_infrastructure_extenders/profiles` — the
  :class:`~rts_prebuilder.abstract_infrastructure.Profile` a target offers.
* :doc:`../api/base_types` — the shared aliases and value sets the signatures
  use.

.. admonition:: Other guides
   :class: routing-box

   * Building a new infrastructure from scratch, with no
     ``EngineToInfrastructureInterface`` for your repo layout:
     :doc:`concrete_infrastructure_builders`.
   * Only running an existing infrastructure's entry point to generate a
     buildable runtime: :doc:`end_users_guide`.

Prerequisites
=============

* Source files for your target (board support packages, linker scripts, …) to
  integrate.
* An existing infrastructure for your repository structure, with its
  ``EngineToInfrastructureInterface`` (see
  :doc:`concrete_infrastructure_builders` if you need to create one).

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   Your extension can reuse the ``bb_runtimes_targets_gen`` engine interface,
   target bases, profile factories and components. Supply your own source directories with
   ``--source-search-path`` and register your target instances from your own
   entry point, as described in :ref:`step 4 <extenders_step_use>`.

What you will write
===================

You write two kinds of object, in this order:

#. Components (:ref:`step 1 <extenders_step_components>`), the groups of sources
   and flags. Nearly all the content lives here, which is why it comes first: a
   target is little more than an assembly of components plus a handful of
   hardware facts.
#. A target class (:ref:`step 2 <extenders_step_target>`), the board's identity
   and hardware traits, composing those components in.

Then you declare the profiles the target offers
(:ref:`step 3 <extenders_step_profiles>`) and plug the target into the engine
(:ref:`step 4 <extenders_step_use>`).

A component reaches a runtime by one of two routes, and choosing between them is
most of the design work:

* The target's own tree. The target composes it with ``add_dependency`` in
  ``__init__`` (:ref:`step 2 <extenders_step_target>`). It is resolved for every
  profile the target offers, so it carries what holds for every runtime of this
  board.
* A profile's ``components`` list (:ref:`step 3 <extenders_step_profiles>`). It
  is read for that one runtime, so it carries what varies from profile to
  profile.

The runtime is the target tree merged with the profile's components. The two are
siblings in that merge: neither is a dependency of the other, and a component
listed in both is a duplicate the engine rejects. A component carried by either
route may still opt out of a given runtime from its own ``contribute``.

.. _extenders_step_components:

Step 1: Write your components
=============================

A :class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent` is
one coherent group of sources and switches: a feature, a per-CPU layer, one
variant of an implementation, or simply everything this board adds. Every source
file and every switch that ends up in a runtime is carried by one.

Before you write one, look at what you already get. The profile factories fill a
component set for each base profile, so a large part of the runtime is there
before your target adds anything. Read the targets your infrastructure ships for
boards close to yours, and the components they compose: an arch layer, a
tasking layer or a driver you need is often already written. Write a new
component for what none of them covers.

A component has two kinds of state, and each has one place it may be declared in.
Both are enforced, so a call in the wrong place raises rather than quietly doing
the wrong thing:

.. list-table::
   :header-rows: 1
   :widths: 20 34 46

   * - Kind
     - Home
     - What belongs there
   * - Entries
     - ``contribute(target, profile)``
     - Every source file and switch, via the registration helpers, whether or
       not it varies by runtime.
   * - Structure
     - ``__init__``
     - The dependency graph (``add_dependency`` and friends).

.. code-block:: python

   from rts_prebuilder.abstract_infrastructure import SourcesAndFlagsComponent


   class MyBoardSupport(SourcesAndFlagsComponent):
       def __init__(self) -> None:
           super().__init__()
           self.add_dependency(MyArchSupport(), MyBoardClock())

       def contribute(self, target, profile):
           self.add_gnat_sources(
               "my_board/start.S",
               "my_board/s-textio__my_board.adb",
           )

The two sections below take these two halves in turn.

Registering entries — ``contribute(target, profile)``
-----------------------------------------------------

:meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.contribute`
receives the ``(target, profile)`` the runtime is being built for and is the only
place :ref:`the registration helpers <extenders_registrations>` may be called.
It runs once per runtime, so the component decides its contents at query time
rather than freezing them at construction. ``add_dependency`` is not one of them:
it shares the prefix but belongs in ``__init__``.

``MyBoardSupport`` above registers the same two sources for every runtime it is
reached for. Varying by runtime is a branch in the same body:

.. code-block:: python

   class SecondaryStack(SourcesAndFlagsComponent):
       def contribute(self, target, profile):
           self.add_gnat_sources("s-secsta.ads", "s-secsta.adb")
           if target.is_64bit:
               self.add_gnat_sources("s-secsta__64.adb")

Rules for the body:

* Never call ``super().contribute(...)``. Every class's ``contribute`` is chained
  for you, base to derived, so each class declares only its own slice.
* Branch only on what the abstract contract exposes: the ``AbstractTarget``
  members (``platform``, ``platform_variant``, ``is_64bit``, ``has_cheri``,
  ``has_libc(base)``, …) and the ``Profile`` fields (``base_profile``, …).
* Treat ``target`` and ``profile`` as call-scoped. Do not stash them on ``self``:
  one instance is queried for several runtimes.

Most bodies never use either argument, and a class with no ``contribute`` at all
adds nothing, which is right for a component that only composes dependencies.
Entries live here whether they vary by runtime or not, so ``replace_channel`` has
one meaning and there is one place to look for what a component registers.

.. _extenders_registrations:

What a component can carry
--------------------------

The full set of registrations, every one of them called from ``contribute`` and
accepting ``replace_channel=True`` (see below) unless noted. The structural
verbs are the exception and belong in ``__init__``: they are under
:ref:`extenders_composing`.

Sources
~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 34 66

   * - Call
     - What it registers, and where it lands
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_gnat_sources`
       / :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_gnat_source`
     - libgnat source files, installed under ``gnat/``. The singular form can
       rename the installed file.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_gnarl_sources`
       / :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_gnarl_source`
     - libgnarl (tasking) source files, installed under ``gnarl/``. Same renaming
       option.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.append_sources_for_dir`
     - Lower level: sources installed under an arbitrary ``subdir``.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.append_sources`
     - Lowest level: pre-built
       :class:`~rts_prebuilder.abstract_infrastructure.SourceFile` objects, for
       full control over destination and name.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.prepend_sources_search_paths`
     - Extra directories to look this component's sources up in. Its channel
       combines the other way round from the rest, so a later registration takes
       precedence, hence no ``replace_channel``.

Switches
~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 34 66

   * - Call
     - What it registers, and where it lands
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_app_switches`
     - Switches for the end user's build against the runtime. They are written
       into the generated ``runtime.xml``. ``link_switches`` applies whichever
       loader the program selects; the switches one loader adds live on that
       ``Loader``. See :ref:`extenders_loaders_ref`.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_runtime_switches`
     - Switches for the runtime's own build. They are written into the generated
       ``target_options.gpr``. ``gnarl_switches`` and ``debug_switches`` exist
       for this build only: the tasking half, and the Debug and Gnatcov build
       variants.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_app_and_runtime_switches`
     - Both at once, which is why a switch both builds need is written once
       rather than twice. The link is left out: the two builds link different
       things, so say which one you mean.
   * - :meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.add_gpr_external`
     - Declares ``gpr_name := external ("env_name");`` in the generated project,
       so a switch can reference an environment value (``"-L" & VSB_Dir &
       "/lib"``) instead of the target patching generated text.

In all three switch calls the keyword says what kind of switch it is:
``common_switches`` every language, ``c_switches`` the C family (C, C++ and Asm,
which the C driver preprocesses), ``asm_switches`` Asm alone, ``link_switches``
the link. Both builds are described with the same
:class:`~rts_prebuilder.abstract_infrastructure.GprSwitches`, because a gpr
project splits switches the same way on either side. What differs is what the
set applies to: the runtime's own library, or the end user's program.

.. _extenders_replace_channel:

Overriding an entry — ``replace_channel``
-----------------------------------------

There is no verb to edit an already-registered source or switch. Instead,
``replace_channel=True`` drops this node's own entries for that channel before
adding, which is how a subclass replaces what its base registered. Dependencies
still merge in as usual.

Because entries have one home, this reaches everything the node registered,
including what a base class contributed earlier in the chain:

.. code-block:: python

   class FeatureSupport(SourcesAndFlagsComponent):
       """Generic implementation of a feature."""

       def contribute(self, target, profile):
           self.add_gnat_sources("shared/s-feature.ads", "shared/s-feature.adb")
           self.add_runtime_switches(common_switches=("-DFEATURE_GENERIC",))


   class FeatureSupportTuned(FeatureSupport):
       """Same feature, built in tuned mode."""

       def contribute(self, target, profile):
           # The base put -DFEATURE_GENERIC in COMMON earlier in the chain;
           # replace it instead of ending up with both defines.
           self.add_runtime_switches(
               common_switches=("-DFEATURE_TUNED",), replace_channel=True
           )

The two sources still come from the base: they live in a different channel, which
``replace_channel`` on the switch channel does not touch.

Note the granularity: the flag clears the whole channel on that node, not just a
matching entry. For sources this means all of the node's sources, gnat, gnarl and
linker scripts alike. For the runtime-build switches it means every group, so
the example above also drops anything the base had put in the C or Asm groups.

``replace_channel`` stops at the node: it cannot reach what a dependency
contributes, and no knob can. If a variant needs a different piece, it composes a
different piece.

.. _extenders_composing:

Composing components together
-----------------------------

A component holds dependency components, and every query merges the node with all
of them, recursively: the
`Composite pattern <https://en.wikipedia.org/wiki/Composite_pattern>`_, drawn out
in :doc:`../overview/3_architecture`. Composition is imperative, in ``__init__``,
with one verb, ``add_dependency``, as ``MyBoardSupport`` pulls in its arch layer
and its clock above. A component may be nothing but an assembly:

.. code-block:: python

   class MathSupport(SourcesAndFlagsComponent):
       """Everything the runtime's maths needs, assembled from smaller pieces."""

       def __init__(self) -> None:
           super().__init__()
           self.add_dependency(GenericSqrt(), Exponent())

At most one dependency of a given type may be registered. Adding a second raises,
because two instances of one component class carry the same sources, so a
duplicate is always a selection bug.

Compose only what is universal. A component's graph is fixed once, so anything
not wanted by every user of that component does not belong in it.

Deriving from a component raises ``TypeError``: reuse is composition. Only a
target may be subclassed, since its hierarchy carries the properties describing
the hardware and nothing composes a property.

``add_dependency`` is the only verb. Nothing removes or replaces what a base
composed. That is left out on purpose, to keep targets simple to read: a target
declares what it is made of in one place, and you never have to walk the class
hierarchy to see what it really holds. A base that some subclass does not want a
piece from must not compose that piece at all. The subclasses that want it
compose it themselves.

The matching ``get_*`` queries
(:meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.get_sources`,
:meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.get_app_switches`,
:meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.get_runtime_switches`,
:meth:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent.get_gpr_externals`,
…) are what the engine calls. Each returns this node's entries merged with every
dependency's, recursively. An extender rarely calls them directly. They are
listed in full on
:doc:`../api/concrete_infrastructure_extenders/components`.

.. attention::

   ``add_dependency`` raises ``TypeError`` when called from ``contribute``. The
   graph is a property of the component, not of the runtime being built: it is
   fixed once per instance, and one instance is queried for many runtimes.

   So a component that applies to some runtimes only is never gated by pruning the
   graph. It is either carried by those profiles alone, or it self-gates in its
   own ``contribute`` by branching on the ``(target, profile)``.

   A platform-specific component that is reached for a platform it does not
   support raises
   :class:`~rts_prebuilder.abstract_infrastructure.IncompatibleComponent`. That
   reports a selection bug and stops the run. It is not a way to opt out.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   An arch-level component shared by every aarch64 target of that
   infrastructure, and the Morello variant of it:

   .. code-block:: python

      class Aarch64Arch(SourcesAndFlagsComponent):
          @override
          def contribute(self, target: AbstractTarget, profile: Profile) -> None:
              self.add_gnat_sources(
                  "shared/i-aarch64.ads",
                  "shared/i-aarch64.adb",
                  "shared/i-cache.ads",
                  "shared/i-cache__aarch64.adb",
              )
              self.add_gnarl_sources(
                  "shared/s-bbcpsp__aarch64.ads",
                  "shared/s-bbcppr__new.ads",
                  "shared/s-bbcppr__aarch64.adb",
                  "aarch64/context_switch.S",
              )


      class MorelloArch(SourcesAndFlagsComponent):
          @override
          def contribute(self, target: AbstractTarget, profile: Profile) -> None:
              self.add_gnarl_sources(
                  "shared/s-bbcpsp__morello.ads",
                  "shared/s-bbcppr__new.ads",
                  "shared/s-bbcppr__morello.adb",
                  "aarch64/morello/context_switch.S",
              )

   The paths are that repository's layout. Only the class, the hook and the
   registration calls come from ``rts_prebuilder``.

.. _extenders_step_target:

Step 2: Define your target class
================================

With the components written, the target is thin: an identity, some hardware
facts, and the composition of those components.

Inherit from :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`, or,
much more commonly, from a pre-filled subclass your infrastructure ships. The
examples below call that base ``MyInfraTarget``.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   That infrastructure ships two bases. ``Target`` holds what every bb-runtimes
   target shares: the build and prebuild scripts, the link spec file, the default
   switches component. ``DFBBTarget`` derives from it for bare-metal boards with
   a single and double precision FPU, so it also fills ``platform`` and the FPU
   properties. OS targets start from ``Target``, boards from ``DFBBTarget``.

The abstract members
--------------------

A target is attributes and components only. It describes hardware and composes
sources; it holds no generation or rendering logic. ``AbstractTarget`` has four
abstract members:

* ``name`` — board name used to build the runtime directory
  (``<profile>-<name>``, for example ``light-myboard``). See also ``cli_name``,
  which is not abstract and defaults to ``name``, when the CLI selector must
  differ from the runtime-directory name.
* ``target`` — gprbuild target triplet, for example ``aarch64-elf``.
* ``platform`` — :data:`~rts_prebuilder.base_types.PlatformIdType`, for example
  ``"bb"``, ``"pikeos"`` or ``"linux"``. Usually filled by the infrastructure's
  pre-filled subclass. ``platform_variant`` narrows it when one platform ships
  several.
* :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profiles` — the
  runtimes the target offers (:ref:`step 3 <extenders_step_profiles>`).

When extending a pre-filled subclass you typically fill only ``name``, ``target``
and ``profiles``, plus ``loaders`` when the runtime offers a link choice
(:ref:`below <extenders_loaders_ref>`). The non-abstract properties (``has_fpu``,
``is_64bit``, ``has_huge_memory``, ``has_timer_64``, ``supports_gc_sections``,
``uses_eh_frame_unwinding``, ``use_certifiable_packages``, ``has_cheri``,
``has_libc(base_profile)``, …) are overridden as needed to describe the hardware.
They are plain declarations: the mechanism that acts on them lives once in the
engine or the generators, never in the target. Two more non-abstract properties
are not hardware facts at all: ``prepare_build_script`` and ``build_script`` name the
scripts the build runs around gprbuild. The full set, with each default, is on
:doc:`../api/concrete_infrastructure_extenders/target_definition`.

Composing the components in
---------------------------

A target is a
:class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`, the
root one of its runtime. Everything from Step 1 is available on it, and the same
rule applies: family components are composed in ``__init__``, while the target's
own sources and switches are registered from ``contribute``:

.. code-block:: python

   class MyTarget(MyInfraTarget):
       def __init__(self) -> None:
           super().__init__()
           self.add_dependency(MyBoardSupport(), MathSupport())

       def contribute(self, target, profile) -> None:
           # Only what is this board's alone; anything shared sits in a component.
           self.add_gnat_sources("my_board/start.S")

Same hook, same signature, same base-to-derived chaining across the MRO, so again
never call ``super().contribute(...)``.

Being the root, the target is also the ``target`` every node in the tree is
queried with, so here ``self`` and ``target`` are the same object. Register on
``self``, but read the properties above off ``target``: that spelling still holds
if the code later moves into a shared component. A property the board class
declares for itself stays on ``self``, since it does not exist on
``AbstractTarget``.

Sources shared by a family of targets belong in their own component, composed in,
never hoisted into a target base class.

.. _extenders_loaders_ref:

The loaders a runtime offers
----------------------------

A :class:`~rts_prebuilder.abstract_infrastructure.Loader` is one link
configuration of the end user's program, selected with ``-XLOADER``: ``RAM``,
``ROM``, ``QSPI``, a partial or a full link. They are registered from
``contribute`` like sources and switches, with ``add_loaders``, and the runtime
offers every loader its tree declares.

.. code-block:: python

   @override
   def contribute(self, target, profile) -> None:
       self.add_loaders(
           Loader("RAM", linker_scripts=(Path("arm/board/ram.ld"),)),
           Loader("ROM", linker_scripts=(Path("arm/board/rom.ld"),)),
           Loader("PARTIAL_LINK"),
       )

Each ``Loader`` owns everything its arm of the generated ``case`` statement
applies: its ``-XLOADER`` name, the scripts it passes to ``-T``, the scripts
installed alongside them without being passed, and the switches applied after
them. The fields are documented on
:class:`~rts_prebuilder.abstract_infrastructure.Loader`.

The infrastructure installs what a loader names, so a script is written once,
where it is used. A script several arms link is named by each of them, since
``-T`` order is part of the link.

A loader is a whole value, so unlike sources and switches two of them never
merge. What follows from that:

* Two loaders cannot share a name, whichever components declared them. A name is
  one arm of the generated ``case`` statement and one value of the ``Loaders``
  gpr type.
* A component cannot put a script into a loader another component declared. It
  declares its own loader, or the component that owns the loader names the
  script.
* The generated project defaults to the first loader offered. Set
  :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.default_loader`
  on the target when the right one is not first, so the default does not depend
  on where in the tree a loader is declared.
* A loader with neither scripts nor switches is bare: selecting it links with
  nothing added. Nothing can be attached to it afterwards, so an empty loader says
  the link takes nothing rather than that entries are still missing.
* An infrastructure may add loaders of its own on top of the declared ones. A
  component that declares one of those itself is an error.

When two variants of a target ship different scripts that must be linked under
one name, give the odd one a ``__variant`` suffix. A source file installs
without that suffix, so both variants install as the same name and the ``-T``
does not change:

.. code-block:: python

   # src/board/ram.ld and src/board/ram__smp.ld both install as ram.ld
   Loader(
       "RAM",
       linker_scripts=(
           Path("board/ram__smp.ld") if self.smp else Path("board/ram.ld"),
       ),
   )

Switches and ``runtime.xml``
----------------------------

A target registers switches from ``contribute``, with the component calls of
:ref:`extenders_registrations`: ``add_app_switches`` for the end user's build,
``add_runtime_switches`` for the runtime's own build,
``add_app_and_runtime_switches`` for both. None of them is mandatory. A target
that adds nothing to what its components already give registers no switches at
all.

``runtime.xml`` is generated by the infrastructure, from the target's
attributes and components, through the engine interface. A target that ships a
hand-maintained one declares it as data, through whatever attribute the
infrastructure exposes for it.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   A family target, which composes the arch component, states the hardware facts
   and declares its profiles:

   .. code-block:: python

      class Aarch64Target(DFBBTarget):
          def __init__(self) -> None:
              super().__init__()
              self.add_dependency(Aarch64Arch())

          @property
          def target(self) -> str:
              return "aarch64-elf"

          @property
          def is_64bit(self) -> bool:
              return True

          @property
          def profiles(self):
              return [
                  light_profile(self, Path("system-xi-arm.ads")),
                  light_tasking_profile(self, Path("system-xi-arm-sfp.ads")),
                  embedded_profile(self, Path("system-xi-arm-full.ads")),
              ]

   A board on top of it: the loaders it offers, an ``-mcpu``, its own sources:

   .. code-block:: python

      class ZynqMP(Aarch64Target):
          @property
          def name(self):
              return "zynqmp"

          @override
          def contribute(self, target: AbstractTarget, profile: Profile) -> None:
              self.add_app_switches(common_switches=("-mcpu=cortex-a53",))
              common = Path("aarch64/zynqmp/common.ld")
              self.add_loaders(
                  *(
                      Loader(
                          name,
                          linker_scripts=(Path(f"aarch64/zynqmp/{script}"),),
                          included_scripts=(common,),
                      )
                      for name, script in (
                          ("RAM", "ram.ld"),
                          ("QSPI", "qspi.ld"),
                          ("HELIX", "helix.ld"),
                      )
                  )
              )
              self.add_gnat_sources(
                  "aarch64/zynqmp/start.S",
                  "shared/s-textio__zynqmp.adb",
              )


      TARGETS = (ZynqMP(),)

   ``common.ld`` is included by each of the three scripts rather than linked, so
   it is named in ``included_scripts`` and never reaches ``-T``.

.. _extenders_step_profiles:

Step 3: Declare the profiles the target offers
==============================================

:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profiles` returns
one :class:`~rts_prebuilder.abstract_infrastructure.Profile` per runtime the
target offers. A ``Profile`` is pure data: its commercial name, the base profile
it maps to, its ``system.ads`` and the ``components`` that make up that
runtime. Names must be unique within a target, since they drive
the runtime fan-out.

The package defines the dataclass and nothing more. Filling ``components`` is the
infrastructure's job, and infrastructures normally expose a factory per base
profile so a target never lists components by hand:

.. code-block:: python

   @property
   def profiles(self):
       return [
           light_profile(self, Path("system-myarch.ads")),
           light_tasking_profile(self, Path("system-myarch-sfp.ads")),
           embedded_profile(self, Path("system-myarch-full.ads")),
       ]

Those factories derive the component set from the target's properties, so most
targets never touch it. Describe the hardware and the factories do the rest. To
see how one builds its list, read
:doc:`../api/concrete_infrastructure_builders/components_selection`.

The platform tasking layer varies with something ``platform`` alone does not say:
the OS version the board runs, or the kernel-versus-user-space variant. Declare
that as ``platform_variant`` and let the platform's own component read it, so the
target still only describes itself:

.. code-block:: python

   class MyPlatformTarget(MyInfraTarget):
       @property
       def platform_variant(self):
           return "rtp" if self.is_rtp else "kernel"

       @property
       def profiles(self):
           return [
               light_profile(self, Path("system-myplatform.ads")),
               light_tasking_profile(self, Path("system-myplatform-sfp.ads")),
           ]

A profile that needs a genuinely different source set skips the factories. Build a
``Profile`` directly, typically on the ``none`` base, with a hand-picked
``components`` list:

.. code-block:: python

   from rts_prebuilder.abstract_infrastructure import Profile

   Profile(
       name="my-custom-rts",
       base_profile="none",
       system_ads=Path("system-mycustom.ads"),
       components=[Common(), MyBoardMath(), MyBoardIO()],
   )

.. _extenders_step_use:

Step 4: Use your target
=======================

Register your target instances with
:class:`~rts_prebuilder.engine.RuntimePrebuilderCLI` and run it. For scripted
use, instantiate :class:`~rts_prebuilder.engine.RuntimePrebuildDriver` directly.
Either way, the infrastructure's engine interface must have been registered
first. See :ref:`infrastructure_registration`.

Your extension's entry point registers the target instances it exposes.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   The shipped command-line entry points expose the built-in
   ``bb_runtimes_targets_gen`` targets.
   Each architecture subpackage ends its ``__init__.py`` with the instances it
   offers:

   .. code-block:: python

      # bb_runtimes_targets_gen/targets/aarch64/__init__.py
      TARGETS = (
          Morello(uart_io=False), Morello(uart_io=True), ZynqMP(), Rpi3(), Rpi3Mc()
      )

   ``targets/__init__.py`` concatenates those lists into one ``TARGETS``, and
   ``targets/__main__.py`` registers the engine interface and hands that
   ``TARGETS`` to the CLI. It is what the ``prebuild_rts`` console script of
   ``pyproject.toml`` runs. Each architecture subpackage carries a copy of it,
   so ``python -m bb_runtimes_targets_gen.targets.aarch64`` offers that
   architecture's targets alone.

   For your extension, create an entry point that reuses the engine interface
   and registers your target instances. Replace ``my_targets`` with the module
   that defines your ``MyTarget`` class:

   .. code-block:: python

      from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
          engine_interface,
      )
      from rts_prebuilder.engine import RuntimePrebuilderCLI

      from my_targets import MyTarget

      engine_interface.self_register()
      RuntimePrebuilderCLI.register_targets(MyTarget()).run()

.. _extenders_customization:

Design rules to remember
========================

A run installs the sources and writes the runtime's build files: the ``.gpr``
projects, ``runtime.xml``, the path files. Those written files are never
patched. Rendering the default ``runtime.xml`` and then ``.replace()``-ing its
text is what :ref:`rts_prebuilder requirement R5, every customization goes through
the API <requirements_table>` forbids. Wanting to string-replace generated text means
the API is missing an input: give the generator a property or a component
instead, or declare a hand-maintained ``runtime.xml`` as data.

So everything your target needs is said in the target definition, and it goes in
one of three places. Pick the most declarative one that fits.

#. A first-class :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`
   property. Use one when the thing is a fact about the target that the engine
   or the generators already branch on, or that more than one decision derives
   from: hardware traits (``has_fpu``, ``is_64bit``), behavioural facts
   (``supports_gc_sections``, ``uses_eh_frame_unwinding``), or a declaration
   such as which hand-maintained ``runtime.xml`` to use. A property states one
   thing, and the mechanism that consumes it lives once, in the engine or the
   generator. Promote a value to a property as soon as a generator or a default
   selection needs to branch on it, rather than teaching each target to emit the
   resulting switches itself. Default it on ``AbstractTarget`` or on the
   pre-filled base so existing targets are unaffected, and override it on the
   targets that differ. The full list is on
   :doc:`../api/concrete_infrastructure_extenders/target_definition`.

#. A component registration (:ref:`step 1 <extenders_step_components>`). Use one
   for concrete sources, build flags, linker switches and scripts, and gpr
   externals. All of them go in a ``contribute``: the target's own for
   board-specific ones, a component's for family-shared ones, composed in. To
   change something a base composed, override the choice that picked it, or use
   ``replace_channel=True`` for the base's own entries in one channel.

#. A profile factory (:ref:`step 3 <extenders_step_profiles>`). Which component
   families make up a given profile is decided there. The usual customization is
   passing the platform tasking layer to a factory. Stay on the factories for as
   long as your target is a variation on hardware the infrastructure already
   knows: they cover that case and they keep the target thin. They stop paying
   off once the runtime stops looking like the ones they were written for: a
   runtime whose libgnat subset is chosen by a certification
   argument, a platform whose tasking layer shares nothing with the bareboard
   one, a board with no relation to the families in the repository. Bending the
   shared components into that shape means gating them on facts that only ever
   hold for this one target, and every other user of those components pays for
   it. Write components of your own for the parts that differ, and hand-build
   the :class:`~rts_prebuilder.abstract_infrastructure.Profile` instead of
   widening the shared selection. A bespoke profile states the runtime's
   contents in one readable place, and it costs little: a ``Profile`` is pure
   data, and the components it does share are composed in like any other.

In short: describe the target with properties, contribute sources and flags with
components, and pick families in the profile's selection. Reach for a new property
the moment a generator would otherwise have to be told the same fact by every
target.

.. _extenders_target_families:

Generating a family of variants
-------------------------------

Some families have too many variants to write one class each, and the variants
differ only in data: a flash size, a RAM size, a clock source. Subclass
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTargetGenerator`, which
yields the variant names and instantiates a target for one of them, and register
it with the CLI alongside plain targets. The members to fill are on
:doc:`../api/concrete_infrastructure_extenders/target_generators`.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   ``targets/arm/generators.py`` holds the STM32, SAM and RP2040 generators.

Next steps
==========

Generate a runtime with the target you wrote, then build the tree it produces:
:doc:`end_users_guide`.
