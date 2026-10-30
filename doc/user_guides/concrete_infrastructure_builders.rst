.. Copyright (C) 2025-2026, AdaCore

=====================================
Concrete Infrastructure Builder Guide
=====================================

This guide covers advanced usage: creating your own runtime generator from
scratch with ``rts_prebuilder``.

Before proceeding, reconsider whether extending an existing target definition
meets your needs. See
:doc:`extending_with_your_own_target` for that approach. A new generator requires
you to maintain its component catalog, profile selection and source resolution.

Read :doc:`../overview/3_architecture` alongside this guide to understand the
contracts your infrastructure must implement.

.. admonition:: Other guides
   :class: routing-box

   * Extending target definitions for your board, CPU or platform:
     :doc:`extending_with_your_own_target`.
   * Only running an existing infrastructure's entry point to generate a
     buildable runtime: :doc:`end_users_guide`.

What rts_prebuilder requires of you
=====================================

``rts_prebuilder`` requires exactly two things: an
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
that has been registered, and at least one
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget` handed to the
engine. Everything else below is a convention that has proved to work. Where a
step is a convention rather than a requirement, it says so.

Step 1: Design your source organization
=======================================

Decide how your source files and your target definitions are laid out across
repositories. ``rts_prebuilder`` makes no assumption here: a component names a
source by a relative path, and the resolver turns that into a file at run time. The layout
you pick is what those relative paths are relative to.

Step 2: Customize the path resolver (optional)
==============================================

Most infrastructures reuse
:class:`~rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver`
as-is. It exposes ``add_search_paths()`` for callers to supply source directories
at run time.

Subclass :class:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver` only
when you need a different chain of resolution steps. The base class already
implements ``resolve()``, which dispatches through the steps and records each
resolution, so a subclass's contract is:

* pass a ``list[AbstractResolutionStep]`` to ``super().__init__(steps=...)``;
* override
  :meth:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver.add_search_paths`
  to wire whichever of your steps consumes run-time search paths.

``DefaultSourcePathResolver`` is the canonical pattern to copy.

Step 3: Provide the runtime components
======================================

Write the master inventory of every source group that can appear in any runtime
this infrastructure can produce, as
:class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`
subclasses. One class per coherent group of files: a feature, a per-CPU layer, a
variant of an implementation. A component registers its files from
``contribute(target, profile)`` with ``add_gnat_sources()`` and
``add_gnarl_sources()``. When a group has platform-specific files, branch on the
target's ``platform`` right there.

Keep components profile-unaware. Each registers its files, branching only on
target facts. Which profiles a component belongs to is decided in one place, your
selection layer (Step 4).

.. note::

   A target also calls ``add_gnat_sources`` and ``add_gnarl_sources`` from its own
   ``contribute``, but only for the paths that board alone brings. The components
   here are shared building blocks, picked per profile.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   One class per component, grouped by theme in the source tree:

   .. code-block:: python

      class Fpu(SourcesAndFlagsComponent):
          @override
          def contribute(self, target: AbstractTarget, profile: Profile) -> None:
              self.add_gnat_sources(
                  "s-fatgen.ads",
                  "s-fatgen.adb",
              )


      class GnarlTimer32(SourcesAndFlagsComponent):
          @override
          def contribute(self, target: AbstractTarget, profile: Profile) -> None:
              match target.platform:
                  case "bb":
                      self.add_gnarl_sources(
                          "s-bbbosu__timer-32.ads",
                          "s-bbtime__timer-32.adb",
                      )

Step 4: Decide how a profile picks its components
=================================================

The package does not select components. It reads
:attr:`~rts_prebuilder.abstract_infrastructure.Profile.components` and merges it
with the target's own tree. Filling that list is yours to design.

The shape that has proved to work is one plain function per base profile, each
returning the flat set of components for that runtime, plus one profile factory
per base profile that calls it:

.. code-block:: python

   def light_components(target):
       ret = [Common(), Light(), ...]
       if target.is_64bit:
           ret.append(Common64())
       return ret


   def light_profile(target, system_ads: Path, *, name="light"):
       return Profile(
           name=name, base_profile="light", system_ads=system_ads,
           components=light_components(target),
       )

Targets then call the factories from their ``profiles`` property and never list
components by hand. The per-profile policy sits in one module instead of being
scattered across the catalog.

Leave a way to pass a component in that the function cannot derive from a target
property, such as a platform-specific tasking layer. A keyword argument on the
factory, forwarded into the component set, keeps that out of the target's
component tree.

Step 5: Define the infrastructure interface
===========================================

Build a module-level
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`.
It has two fields:

``path_resolver_instance``
   Optional. Defaults to
   :class:`~rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver`.
   Pass your own subclass only if the default search-path strategy does not fit.

``runtime_xml_generator``
   The callable ``(target, profile) -> str`` the engine uses to render each
   ``runtime.xml``. Targets never generate their own.

.. code-block:: python

   from rts_prebuilder.abstract_infrastructure import EngineToInfrastructureInterface

   from .runtime_xml_gen import generate_runtime_xml

   engine_interface = EngineToInfrastructureInterface(
       runtime_xml_generator=generate_runtime_xml,
   )

Step 6: Register your infrastructure
====================================

Importing the module is not enough. The engine only sees an interface that has
called
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`.
Call it from the entry point that runs the engine:

.. code-block:: python

   from rts_prebuilder.engine import RuntimePrebuilderCLI

   from your_infrastructure_module import engine_interface
   from . import TARGETS

   engine_interface.self_register()


   def main() -> None:
       RuntimePrebuilderCLI.register_targets(*TARGETS).run()

See :ref:`infrastructure_registration` for the singleton semantics.

Search paths are fed to the already-built interface at run time, by whoever drives
the engine:

.. code-block:: python

   engine_interface.path_resolver_instance.add_search_paths(
       Path("/path/to/runtime-sources"),
       Path("/path/to/your-target-sources"),
   )

Step 7: Define base target classes
==================================

Base target classes are recommended, but ``rts_prebuilder`` does not require them.
``AbstractTarget`` has four abstract members an extender would otherwise have
to fill: ``name``, ``platform``,
``target`` and
:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profiles`.
Component selection is an infrastructure-level decision rather than a per-board
one, so a pre-filled base is where you expose the profile factories from Step 4
and fill in whatever is constant across your targets.

Extenders then inherit from your base and supply only ``name``, ``target`` and
``profiles``, plus their board-specific sources.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   Two levels of base class:

   * ``Target(AbstractTarget)``: the generic one. The ``runtime.xml`` generator is
     registered on the engine interface rather than on the target, and profiles
     are declared per board with the factory helpers.
   * ``DFBBTarget(Target)``: the bareboard variant, filling ``platform="bb"`` plus
     FPU and timer defaults.

   The split into two levels, and which defaults sit at which level, are that
   infrastructure's choices.

Step 8: Expose an entry point
=============================

Decide how a user invokes your infrastructure, and hand your targets to
:class:`~rts_prebuilder.engine.RuntimePrebuilderCLI`. ``register_targets`` accepts
both concrete
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget` instances and
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTargetGenerator` instances
for families whose variants are generated rather than written out.

The flags the resulting command accepts come from ``rts_prebuilder``. See
:doc:`end_users_guide` for usage instructions.
