.. Copyright (C) 2025-2026, AdaCore

============
Architecture
============

This documentation explains how the bb-runtimes repository generates
restricted Ada runtimes. The repository has three parts:

* ``support/rts_prebuilder_project/`` contains ``rts_prebuilder``, the generation
  engine and the contracts for targets, components and profiles.
* ``support/bb_runtimes_targets_gen/`` contains ``bb_runtimes_targets_gen``, the
  target definitions and components that configure the engine for AdaCore's
  runtimes.
* ``src/`` contains runtime sources, linker scripts and project fragments that
  the target definitions select during generation.

These pages cover both Python packages and how they work together.
Green boxes labelled "Specificity of ``bb_runtimes_targets_gen``" contain
details of its targets, components, source selection and build conventions,
as in this example:

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   ARM target definitions live in
   ``support/bb_runtimes_targets_gen/bb_runtimes_targets_gen/targets/arm/``.
   ARM-specific runtime sources live in ``src/arm/``.

This chapter explains the building blocks, the runtime generation process and
the engine's separation from the target definitions.

The engine's design follows the :doc:`rts_prebuilder requirements <2_requirements>`,
identified as R1 to R5 below. Examples here are kept small. The
``rts_prebuilder`` API reference and working patterns are linked from the guides:
:doc:`../user_guides/extending_with_your_own_target`
(extending target definitions) and :doc:`../user_guides/concrete_infrastructure_builders`
(building an infrastructure from scratch).

The building blocks
===================

``SourcesAndFlagsComponent`` — a group of sources and flags
-----------------------------------------------------------

The smallest block. A
:class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent` is one
coherent group of source files and switches: a feature, a per-CPU layer, one
variant of an implementation (R4).

A component declares what it depends on in ``__init__`` and registers its own
sources and switches in ``contribute``:

.. code-block:: python

   class CacheDefinitions(SourcesAndFlagsComponent):
       @override
       def contribute(self, target: AbstractTarget, profile: Profile) -> None:
           self.add_gnat_sources("i-cache_defs.ads")


   class CacheSupport(SourcesAndFlagsComponent):
       def __init__(self) -> None:
           super().__init__()
           self.add_dependency(CacheDefinitions(), CacheMaintenance())

       @override
       def contribute(self, target: AbstractTarget, profile: Profile) -> None:
           self.add_gnat_sources("i-cache.ads", "i-cache.adb")

           if profile.base_profile == "embedded":
               self.add_runtime_switches(
                   common_switches=("-fno-omit-frame-pointer",),
               )

Every query (``get_sources``, ``get_app_switches``, ``get_runtime_switches``, …)
merges a component's own entries with every dependency's, recursively.

One component and a whole subtree answer such a query the same way, so a caller
never needs to know the shape of the tree behind it. The engine asks one
component for ``get_sources``, and that call may return three files or three
hundred gathered from twenty nodes below.

A profile that carries ``CacheSupport`` gets the whole tree below it. Neither
``CacheDefinitions`` nor ``CacheMaintenance`` is named by the caller:

.. mermaid::

   flowchart TB
      Engine["<b>Engine</b><br/>get_sources()"]
      CacheSupport["<b>CacheSupport</b><br/>i-cache.ads<br/>i-cache.adb<br/>switch -fno-omit-frame-pointer"]
      CacheDefinitions["<b>CacheDefinitions</b><br/>i-cache_defs.ads"]
      CacheMaintenance["<b>CacheMaintenance</b><br/>i-cache_ops.adb"]

      Engine -.->|"get_sources(target=my_board, profile=embedded)"| CacheSupport
      CacheSupport -->|"composed of, with add_dependency"| CacheDefinitions
      CacheSupport -->|"composed of, with add_dependency"| CacheMaintenance

The single ``get_sources`` call returns all four files. One class covers both
cases, since a component with no dependency answers with its own entries alone.
A family component can therefore gain sub-components later without a single
caller changing.

.. important::

   The graph has to stay an acyclic tree. A cycle is not supported, and neither
   is one component reached through two branches. Keep it shallow enough to see
   that by reading it.

.. seealso::

   This is the Composite pattern, where a component with no dependency is the
   leaf and one with dependencies is the composite. Background on the pattern
   itself:

   * `Composite pattern — Wikipedia <https://en.wikipedia.org/wiki/Composite_pattern>`_
   * `Composite — Refactoring Guru <https://refactoring.guru/design-patterns/composite>`_
     (worked examples, in many languages)
   * *Design Patterns: Elements of Reusable Object-Oriented Software*,
     Gamma, Helm, Johnson & Vlissides, the original description.

Each half refuses the calls belonging to the other. Registering entries from
``contribute(target, profile)`` lets a group decide its contents at query time
instead of at construction (R4). Composing dependencies in ``__init__`` keeps
the graph a property of the component and not of any one runtime.

Together they meet R5, since no verb edits a graph or an entry already
registered. To change what a runtime is made of, change which components the
profile carries, gate inside a component's own ``contribute``, or feed the
generator data.

How to write one is the extender's job, and the rules live with it:
:doc:`../user_guides/extending_with_your_own_target`.

``AbstractTarget`` — the target description
-------------------------------------------

A target is attributes and one component tree, with no generation logic. One
subclass of :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`
describes it for every profile it offers (R3): its ``name``, the gprbuild
``target`` triplet, its ``platform``, the ``profiles``, and hardware traits
(``has_fpu``, ``is_64bit``, ``uses_eh_frame_unwinding``, …). The engine and the
infrastructure's generators read the target. It renders nothing itself.

A target also names files: its build scripts, its readme, its linker spec, and
the scripts its loaders name. The engine installs those by reading the
properties and the loaders channel, so a target registers no source for them.

The target is also the root component of its runtime, the always-on part
resolved for every profile. It therefore follows every rule above, and is the
``target`` the whole tree is queried with.

.. code-block:: python

   class MyArchTarget(MyInfraTarget):
       def __init__(self):
           super().__init__()
           self.add_dependency(MyArch())   # family sources

       @property
       def target(self):
           return "myarch-elf"

       @property
       def is_64bit(self):
           return True

       @property
       def profiles(self):
           return [
               light_profile(self, Path("system-myarch.ads")),
               light_tasking_profile(self, Path("system-myarch-sfp.ads")),
               embedded_profile(self, Path("system-myarch-full.ads")),
           ]

Concrete targets then subclass this and add only what differs: a ``name``,
linker scripts, an ``-mcpu``, any components of their own.

Only ``AbstractTarget`` comes from the ``rts_prebuilder`` package. The
pre-filled base ``MyInfraTarget`` and the profile factories above come from a
concrete infrastructure. ``rts_prebuilder`` neither defines nor requires them.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   That infrastructure supplies a ``Target`` base and its own profile factories.
   What the base fills in is described in
   :doc:`../user_guides/extending_with_your_own_target`.

The ``stm32f469disco`` board of that infrastructure shows the whole chain. Five
classes stand between the board and the component base, and each one contributes
its own entries:

.. mermaid::

   flowchart TB
      SFC["<b>SourcesAndFlagsComponent</b><br/>contribute()<br/>add_dependency()<br/>add_gnat_sources()<br/>add_runtime_switches()<br/>get_sources()"]
      AT["<b>AbstractTarget</b><br/>name<br/>profiles"]
      T["<b>Target</b><br/>build_script<br/>link_spec_file()"]
      A6["<b>ArmV6MTarget</b><br/>s-bbarat.ads<br/>s-bbarat.adb"]
      A7["<b>ArmV7MTarget</b><br/>has_single_precision_fpu<br/>profiles"]
      S["<b>Stm32</b><br/>s-bbmcpa.ads<br/>svd/i-stm32-rcc.ads<br/>switch -mcpu=cortex-m4"]

      SFC -->|inherited by| AT
      AT -->|inherited by| T
      T -->|inherited by| A6
      A6 -->|inherited by| A7
      A7 -->|inherited by| S

A query on ``Stm32`` returns ``ArmV6MTarget``'s two files first, then
``Stm32``'s own entries.

The same board again, with the components each class composes:

.. mermaid::

   flowchart TB
      Engine["<b>Engine</b><br/>get_sources()"]
      SFC["<b>SourcesAndFlagsComponent</b>"]
      AT["<b>AbstractTarget</b>"]
      T["<b>Target</b>"]
      A6["<b>ArmV6MTarget</b><br/>s-bbarat.ads"]
      A7["<b>ArmV7MTarget</b><br/>profiles"]
      S["<b>Stm32</b><br/>s-bbmcpa.ads<br/>switch -mcpu=cortex-m4"]
      TDS["<b>TargetDefaultSwitchesComponent</b><br/>switch -g<br/>switch -DIN_RTS"]
      A7A["<b>ArmV7MArch</b><br/>s-bbbosu__armv7m.adb"]
      CMA["<b>CortexMArch</b><br/>s-bbcpsp__cortexm.ads"]

      SFC -->|inherited by| AT
      AT -->|inherited by| T
      T -->|inherited by| A6
      A6 -->|inherited by| A7
      A7 -->|inherited by| S

      T -->|"composed of, with add_dependency"| TDS
      S -->|"composed of, with add_dependency"| A7A
      A7A -->|"composed of, with add_dependency"| CMA

      Engine -.->|"get_sources(target=stm32f469disco, profile=light)"| S

One query walks both halves: the class chain and the composed components.

Inheritance and composition
---------------------------

Composition adds a piece to a runtime. Inheritance says a class is a kind of
another one and shares its declarations. Every ``contribute`` in the hierarchy
runs, so a subclass adds to what its bases declared instead of overriding it.

Reach for composition first. A class inherits from one chain of bases but
composes as many components as it likes, so a group of sources put in a base
class is reusable only by the classes that inherit from it.

Neither mechanism can be undone. A dependency a base composes reaches every
subclass and no verb removes it, so a base must compose only what every
subclass wants. Entries a base contributes reach every subclass too, and the
only way out is ``replace_channel``, which clears that whole channel for the
node rather than the one entry the subclass disagreed with.

Deriving from a component is forbidden.
:class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`
rejects it in ``__init_subclass__``, at class-definition time. Only a target
may be subclassed, because a target answers the properties describing the
hardware and no component can supply those. The properties a target shares with
a family live in a base class. When a base cannot answer for all of its
subclasses, split the base instead of correcting it lower down.

The order entries come back in
------------------------------

A node's own entries come from every ``contribute`` in its class hierarchy,
base class before subclass. A query merges those first, then each dependency's,
depth first in composition order. The ``add_dependency`` order decides between
siblings, and the engine asks the target's tree before the profile's
``components``.

Two kinds of channel break that order. The runtime-switch channels
(``runtime_build_switches``, ``runtime_gnarl_switches``,
``runtime_debug_switches``) merge the dependencies first, so the infrastructure
defaults lead every group. ``sources_search_paths`` puts each new registration
in front, so a path added later is searched first.

Take a target class ``ChildTarget`` deriving from ``BaseTarget``, which
composes one ``DependencyComponent``, which composes one
``DeepDependencyComponent``:

.. code-block:: python

   class DeepDependencyComponent(SourcesAndFlagsComponent):
       def contribute(self, target, profile):
           self.add_gnat_sources("deep_dependency.adb")

   class DependencyComponent(SourcesAndFlagsComponent):
       def __init__(self):
           super().__init__()
           self.add_dependency(DeepDependencyComponent())

       def contribute(self, target, profile):
           self.add_gnat_sources("dependency.adb")

   class BaseTarget(AbstractTarget):
       def __init__(self):
           super().__init__()
           self.add_dependency(DependencyComponent())

       def contribute(self, target, profile):
           self.add_gnat_sources("base.ads")

   class ChildTarget(BaseTarget):
       def contribute(self, target, profile):
           self.add_gnat_sources("child.ads")

The profile carries one component contributing ``profile.adb``. The sources of
that runtime come back in this order:

.. code-block:: text

   base.ads              BaseTarget.contribute, the base class, runs first
   child.ads             ChildTarget.contribute, the subclass, runs second
   dependency.adb        the dependency, own entries before its own subtree
   deep_dependency.adb   what DependencyComponent composes
   profile.adb           the profile's components, after the target tree

On the runtime-switch channels only the step between a node and its
dependencies flips. The target's own switches land last, after both components',
which is what puts the infrastructure defaults a target depends on in front of
its own switches.

Nothing is deduplicated, and this order is what a reader sees in the generated
files: a loader's ``-T`` sequence, the switch order in ``target_options.gpr``,
the arms of the ``Loaders`` case. Where the order carries meaning, state it as
data instead of relying on position, as
:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.default_loader`
does.

``AbstractTargetGenerator`` — a family of target variants
---------------------------------------------------------

Some families have too many variants to write one class each, but the variants
differ only in data (a flash size, a clock source). A
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTargetGenerator`
enumerates the variant names lazily and instantiates an ``AbstractTarget`` for
one name on demand. The CLI registers generators next to plain targets, so a
variant can be selected before any object for it exists.

``Profile`` and the selection function — what a runtime is made of
------------------------------------------------------------------

Each :class:`~rts_prebuilder.abstract_infrastructure.Profile` is pure data: its
commercial name, the base profile it maps to, its ``system.ads`` and the
``components`` that make up that runtime. It carries no selection behaviour, so
one target instance can drive many profiles (R3).

The ``rts_prebuilder`` package does not decide which components a profile's
runtime is made of. A concrete infrastructure decides that and stores the result
on ``Profile.components``.

Ordinary components therefore stay profile-unaware: a component contributes its
own sources, and the per-profile policy sits in the selection code instead of
being spread across the catalog (R4). A custom runtime may hand-pick
``Profile.components`` directly.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   One plain function per base profile, all of them in one module, each building
   the ``Profile`` for that runtime:

   .. code-block:: python

      def light_profile(target, system_ads: Path, *, name="light"):
          return Profile(
              name=name, base_profile="light", system_ads=system_ads,
              components=light_components(target),
          )

``Loader`` — one link configuration
------------------------------------

A :class:`~rts_prebuilder.abstract_infrastructure.Loader` is one way of linking
the end user's program: RAM, ROM, QSPI, a partial link. Any component declares
the ones it offers, on the ``loaders`` channel, and each owns the linker scripts
it passes to ``-T`` and any extra link switches. A loader is a whole value: two
never merge, so their names must stay distinct across a runtime, and the end
user picks exactly one per link with ``-XLOADER``.

``SourcePathResolver`` — finding the sources on disk
----------------------------------------------------

Components name sources by unresolved relative path, and the file itself lives
in some external repository. A
:class:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver` (default:
:class:`~rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver`)
resolves each path against search paths fed in at run time, and records what it
found so the install can ship an auditable record (R2).

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   Supply source directories with ``--source-search-path``. A component that
   registers ``"s-atopri.adb"`` finds it directly under a search root.
   A component that registers ``"arm/zynq/start-ram.S"`` finds it under
   ``src/arm/zynq/start-ram.S`` when ``src`` is a search root.

   Sources that declare a relative directory require that directory under a
   search root. Components retain these paths when different implementations
   share a filename, so the resolver selects the intended source.

From a target to installed runtimes
===================================

R1 (produce the trees) and R3 (one target, many profiles) together give the
pipeline. The target is profile-independent, so a policy step specializes it per
profile and a mechanism step writes each specialization to disk. They are kept
apart: the policy makes every profile-driven decision, the mechanism makes none.

.. mermaid::

   %%{init: {"flowchart": {"wrappingWidth": 560, "padding": 25}}}%%
   flowchart TB
      %% conf.py caps labels at 260px for the routing diagram's long questions.
      %% The lines here are already split by hand and must not wrap again.
      subgraph Policy["<b>Policy</b>"]
         direction LR
         T["<b>Target</b> (AbstractTarget)<br/>profile-independent:<br/>name · triplet · profiles (list[Profile])<br/>own component tree"]
         PS["<b>specialize</b>(target, profile)"]
         SPEC["<b>RuntimeSpec</b> (one per profile)<br/>resolved sources · flags · config"]

         T --> PS
         PS --> SPEC
      end

      subgraph Mechanism["<b>Mechanism</b>"]
         direction LR
         GEN["<b>RuntimeWriter</b>(spec)"]
         DISK["Installed runtime directory,<br/>one per profile"]

         GEN -->|install| DISK
      end

      SPEC --> GEN
      style Policy fill:#f0f8ff,stroke:#0066cc,stroke-width:3px
      style Mechanism fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px

:class:`~rts_prebuilder.engine.RuntimePrebuildDriver` drives it: it validates the
target, then loops the target's profiles, running the specializer and the writer
for each.

#. ``specialize()``, the policy. For one profile, it reads the profile-driven
   data off the ``Profile`` (``base_profile``, ``system_ads``), asks the
   infrastructure to render the ``runtime.xml``, and resolves the runtime's
   components (the target's always-on tree merged with ``profile.components``)
   into a flat source list and the merged runtime-build switches.

#. ``RuntimeSpec``, the complete profile-specific description of one runtime. It
   holds no reference to the target and no component tree, just the
   already-resolved values the next step needs. This is the seam that keeps
   policy and mechanism apart.

#. ``RuntimeWriter``, the mechanism. Output only: it turns the spec's source
   list into file pairs and writes them alongside the generated build files
   (``.gpr`` projects, ``runtime.xml``, path files, scaffolding). It needs no
   target and makes no profile decision.

#. Install. ``RuntimePrebuildDriver.install_runtimes(destination)`` installs each
   runtime under ``destination/<name>/`` (copying, or symlinking with
   ``link=True``) and writes the resolution record to the metadata directory
   (R2).

The two namespaces
==================

R2 forces a hard split, and that split is the top-level shape of the
``rts_prebuilder`` package:

* Abstract Infrastructure: the contracts. ABCs and dataclasses that only capture
  a concrete infrastructure's data. They do no processing and import nothing
  external. This is what a concrete infrastructure (such as
  ``bb_runtimes_targets_gen``) fills in. They are also the argument and return
  types of the whole API, so a target or a component that fills a contract wrong
  is caught by a static type checker before a run starts.
* Engine: the mechanism. All processing lives here, written purely against the
  abstract contracts. It never names a source file, a target, or a component
  class.

The engine reaches the concrete data through one door: a concrete
infrastructure builds an
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
and calls ``self_register()``, and the engine pulls it back from a singleton.
With no interface registered, the engine raises (see
:ref:`infrastructure_registration`).

.. mermaid::

   %%{init: {"flowchart": {"wrappingWidth": 900, "padding": 25}}}%%
   flowchart TB
      User["<b>End-user code / CLI</b>"]

      subgraph Package["<b>rts_prebuilder</b>"]
         direction TB
         subgraph EnginePackage["<b>Engine</b>"]
            EngineBox["All the processing<br/>RuntimePrebuildDriver · specialize<br/>RuntimeWriter · CLI"]
         end
         subgraph Contracts["<b>Abstract Infrastructure</b> (contracts)"]
            direction LR
            subgraph Extenders["For infrastructure extenders"]
               direction TB
               BaseClasses["Base classes<br/>AbstractTarget<br/>SourcesAndFlagsComponent"]
               DataClasses["Data classes<br/>SourceFile · Loader · Profile"]
               BaseClasses -->|composed of| DataClasses
            end
            subgraph Builders["For infrastructure builders"]
               direction TB
               InterfaceBox["EngineToInfrastructureInterface<br/>a registered singleton"]
               Resolver["SourcePathResolver"]
               InterfaceBox -->|holds| Resolver
            end
         end
      end

      ConcretePackage["<b>Concrete Infrastructure</b><br/>• concrete targets and target generators<br/>• the component catalog they compose<br/>• profile factories, which components each runtime carries<br/>• its EngineToInfrastructureInterface: runtime.xml generator, source path resolver<br/>• an entry point that registers the interface, then the target list"]

      User -->|drives| EngineBox
      EngineBox -->|works only against| Contracts
      ConcretePackage -.->|implements| Contracts

      style Package fill:#f0f8ff,stroke:#0066cc,stroke-width:3px
      style Contracts fill:#fff9e6,stroke:#ff9800,stroke-width:3px
      style Extenders fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
      style Builders fill:#fff3e0,stroke:#f57c00,stroke-width:2px
      style EnginePackage fill:#e1f5ff,stroke:#0288d1,stroke-width:2px
      style ConcretePackage fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style User fill:#e8f5e9,stroke:#388e3c,stroke-width:3px

Every block above is one of two things: a contract an infrastructure fills, or an
engine class that consumes it. The engine classes are the pipeline described
above. The contracts are:

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Contract
     - What an infrastructure puts in it
   * - :class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`
     - One reusable group of sources and switches, and the groups it is composed
       of (R4).
   * - :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`
     - One target, described profile-independently (R3), with the runtimes
       it offers as ``profiles``.
   * - :class:`~rts_prebuilder.abstract_infrastructure.AbstractTargetGenerator`
     - A family's variant names, and one target built per name on demand.
   * - :class:`~rts_prebuilder.abstract_infrastructure.Profile`
     - One runtime the target offers, as pure data: base profile, system.ads,
       and the components that make it up (R4).
   * - :class:`~rts_prebuilder.abstract_infrastructure.Loader`
     - One link configuration of the end user's program, with the scripts it
       links and the switches it adds.
   * - :class:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver`
     - Where a relative source path is looked up, and how each resolution is
       recorded (R2).
   * - :class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
     - The single registered door through which the engine reaches all of the
       above (R2).
