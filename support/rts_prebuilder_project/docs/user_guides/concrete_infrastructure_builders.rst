.. Copyright (C) 2025-2026, AdaCore

============================
Concrete Infrastructure Builder Guide
============================

.. admonition:: Are You In The Right Place?
   :class: routing-box

   * You already have a concrete infrastructure (e.g. ``bb_runtimes_targets_gen``)
     and only want to add a new target (board/CPU/platform) to it →
     :doc:`concrete_infrastructure_extenders`.
   * You only want to *invoke* an existing infrastructure's CLI to generate a
     buildable runtime → :doc:`end_users_guide`.
   * You need to build a brand-new infrastructure from scratch → continue below.

This guide is for developers who need to **build a completely new runtime infrastructure** from scratch.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   ``bb_runtimes_targets_gen`` is a fully-wired infrastructure for embedded
   targets. Per-step admonitions below point at the matching files; full
   snippets in the Reference Implementation section at the bottom.

Creating Your Infrastructure
=============================

Step 1: Design Your Source Organization
----------------------------------------

Decide how your source files / Target definition will be organized in repositories.

Step 2: (Optional) Customize the Path Resolver
-----------------------------------------------

Most infrastructures can reuse
:class:`~rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver`
as-is — it chains the manifest, libs, runtime search-paths, and GCC-dir
resolution steps, and exposes ``add_search_paths()`` for callers to feed
directories at runtime.

Subclass :class:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver`
only when you need a different chain of resolution steps. The base class
already implements ``resolve()`` (which dispatches through the steps and
records each resolution via ``record_resolution_with_metadata()``), so a
subclass's contract is:

* pass a ``list[AbstractResolutionStep]`` to ``super().__init__(steps=...)``;
* override the abstract
  :meth:`~rts_prebuilder.abstract_infrastructure.SourcePathResolver.add_search_paths`
  to wire whichever of your steps consumes runtime search paths.

See ``DefaultSourcePathResolver`` for the canonical pattern.

Step 3: Provide the Sources Database
------------------------------------

Master inventory of every *source family* that can appear in any runtime
this infrastructure can produce. Each family entry groups files plus
metadata about when they apply.

A family entry can carry:

* ``srcs`` — file list always included for this family.
* one key per :data:`~rts_prebuilder.base_types.PlatformSpecificSourcesIdType`
  (e.g. ``"bb"``, ``"pikeos"``) — extra files included only when assembling
  for that platform.
* ``conditions`` — scenario predicates saying which scenario
  configurations this family applies to.
* ``requires`` — other scenario dimensions this family pulls in (used by
  :func:`~rts_prebuilder.engine.common.rule.complete_scenarios_from_deps`
  to fill scenarios the target didn't pin explicitly).

Two engine phases use it:

* **Phase 1 — Assembler**: filters by profile/platform, copies matching
  families into the output tree, emits a JSON descriptor with each
  family's ``conditions``/``requires``.
* **Phase 2 — Targetizer**: reads the descriptor, evaluates the active
  scenario against ``conditions``, picks the subset that becomes the
  per-target runtime.

Hand the engine a :data:`~rts_prebuilder.base_types.RawRtsSourcesDBType`
dict. "Raw" means untyped at the call site; the engine wraps it into the
strongly-typed :data:`~rts_prebuilder.base_types.RtsSourcesDBType` at load
time. Storage format is your choice — inline Python literal, JSON, YAML
loaded at import time. The dict is passed to
``CommonInterface(all_sources_listing=...)`` in Step 5.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   Inlined as Python literals in
   ``bb_runtimes_targets_gen/concrete_infrastructure/common/sources_db.py``.
   Illustrative samples of the three shapes an entry can take:

   .. code-block:: python

      from rts_prebuilder.base_types import RawRtsSourcesDBType

      all_sources_listing: RawRtsSourcesDBType = {
          # 1. Plain srcs — always included.
          "common": {
              "srcs": ["libgnat/ada.ads", "libgnat/a-assert.ads"],
          },

          # 2. srcs + conditions — included only when the scenario matches.
          #    Predicate syntax: "Dimension:value[,value...]" or
          #    "Dimension:!value" for negation; multiple predicates AND.
          "common/64": {
              "conditions": ["Target_Word_Size:64"],
              "srcs": [
                  "libgnat/i-cexten__128.ads",
                  "libgnat/s-casi128.ads",
              ],
          },

          # 3. srcs + conditions + requires — when this family applies,
          #    also pin the listed scenario dimensions. complete_scenarios_from_deps
          #    follows the chain transitively.
          "image/decimal": {
              "conditions": ["Add_Image_Decimal:yes"],
              "srcs": [
                  "libgnat/s-imaged.ads",
                  "libgnat/s-imaged.adb",
              ],
              "requires": ["Add_Image_Util:yes"],
          },
      }

.. warning::

   Not consumed by extenders' ``add_gnat_sources`` / ``add_gnarl_sources``.
   Those calls are imperative and add arbitrary paths to a single target's
   tree — independent of the sources DB.

Step 4: Implement the Scenario Generator
-----------------------------------------

Create a class inheriting from
:class:`~rts_prebuilder.abstract_infrastructure.AbstractProfileToScenarioGenerator`.
The only abstract method is
:meth:`~rts_prebuilder.abstract_infrastructure.AbstractProfileToScenarioGenerator.profile_to_scenarios`,
which takes a :data:`~rts_prebuilder.base_types.ProfileNameType` and returns
a :data:`~rts_prebuilder.base_types.ScenariosConfigurationType` (a dict
keying scenario dimensions defined in your ``all_possible_scenarios`` to one
of their valid values).

The constructor receives the
:class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget` instance, so
your implementation can read target attributes (``has_fpu``, ``is_64bit``, …)
to drive the mapping.

The generator is **not** registered globally. Each target picks one by
overriding
:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profile_to_scenarios_generator`.
Infrastructures typically ship a default subclass and wire it in from their
pre-filled ``Target`` base class (Step 7).

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   ``DefaultProfileToScenariosGenerator``
   (``infrastructure/runtime_targetizer/profiles.py``) dispatches via
   ``match self.target.base_profile(profile)`` to per-profile
   ``light_scenarios()`` / ``embedded_scenarios()`` / … helpers. The
   pre-filled ``Target`` base class returns it from
   ``profile_to_scenarios_generator``.

Step 5: Define the Infrastructure Interface
--------------------------------------------

Build a module-level :class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
holding a :class:`~rts_prebuilder.abstract_infrastructure.CommonInterface`.
``CommonInterface`` takes:

* ``all_sources_listing`` — a :data:`~rts_prebuilder.base_types.RawRtsSourcesDBType`
  dict mapping source-set names to their file lists.
* ``all_possible_scenarios`` — a
  :data:`~rts_prebuilder.base_types.AllPossibleScenarioConfigsType` dict
  enumerating every scenario dimension and its valid values.
* ``path_resolver_instance`` — optional; defaults to
  :class:`~rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver`.
  Pass your own subclass only if the default search-path strategy doesn't fit.

.. code-block:: python

   from rts_prebuilder.abstract_infrastructure import (
       CommonInterface,
       EngineToInfrastructureInterface,
   )

   from .common.sources_db import all_scenarios, all_sources_listing


   engine_interface = EngineToInfrastructureInterface(
       common=CommonInterface(
           all_sources_listing=all_sources_listing,
           all_possible_scenarios=all_scenarios,
       ),
   )

Step 6: Register Your Infrastructure
-------------------------------------

Importing the module is not enough — the engine only sees an interface that
has called
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`.
Call it from the entry point that runs the engine (typically the
``__main__`` of each target subpackage):

.. code-block:: python

   from rts_prebuilder.engine import RuntimeTargetizerCLI
   from your_infrastructure_module import engine_interface
   from . import TARGETS

   engine_interface.self_register()


   def main() -> None:
       RuntimeTargetizerCLI.register_targets(*TARGETS).run()

See :ref:`infrastructure_registration` for the singleton semantics.

Step 7: Define Base Target Classes
----------------------------------

Strongly recommended. ``AbstractTarget`` has six abstract members an
extender would otherwise have to fill: ``name``, ``platform``,
``system_ads``, ``target``,
:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profile_to_scenarios_generator`,
and
:meth:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.dump_runtime_xml`.
The last two are infrastructure-level decisions, not per-board, so a
pre-filled subclass should supply them once.

Extenders then inherit from your pre-filled base and supply only ``name``,
``target``, ``system_ads``, plus board-specific sources.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   * ``Target(AbstractTarget)`` — fills ``profile_to_scenarios_generator``
     (with the default generator from Step 4) and ``dump_runtime_xml``.
   * ``DFBBTarget(Target)`` — bareboard variant: fills ``platform="bb"``,
     plus FPU and timer defaults.
   * ``ArchSupport(AbsractArchSupport)`` — currently a thin pass-through,
     kept as the seam for future arch-level defaults.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen`` Reference Implementation
   :class: tip

   The three files below show the minimal wiring as it lives in
   ``bb_runtimes_targets_gen``.

   **Sources DB and scenario matrix**
   (``infrastructure/common/sources_db.py``):

   .. code-block:: python

      from rts_prebuilder.base_types import (
          AllPossibleScenarioConfigsType,
          RawRtsSourcesDBType,
      )

      all_scenarios: AllPossibleScenarioConfigsType = {
          "RTS_Profile": ["light", "light-tasking", "embedded", "cert"],
          "Compiler_Backend": ["gcc", "llvm"],
          "CPU_Family": ["arm", "aarch64", "powerpc", "x86_64", "riscv64"],
          "Has_FPU": ["no", "yes"],
          "Timer": ["n/a", "timer32", "timer64"],
          # ... more dimensions ...
      }

      all_sources_listing: RawRtsSourcesDBType = {
          "common": {"srcs": ["libgnat/ada.ads", "libgnat/a-assert.ads"]},
          # ... more source sets ...
      }

   **Engine interface module** (``infrastructure/engine_interface.py``):

   .. code-block:: python

      from rts_prebuilder.abstract_infrastructure import (
          CommonInterface,
          EngineToInfrastructureInterface,
      )

      from .common.sources_db import all_scenarios, all_sources_listing


      engine_interface = EngineToInfrastructureInterface(
          common=CommonInterface(
              all_sources_listing=all_sources_listing,
              all_possible_scenarios=all_scenarios,
          ),
      )

   **CLI entry symlinked into each target subpackage**
   (``targets/__main__to__symlink_to.py``):

   .. code-block:: python

      from rts_prebuilder.engine import RuntimeTargetizerCLI

      from . import TARGETS  # provided by each target subpackage's __init__.py
      from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
          engine_interface,
      )

      engine_interface.self_register()


      def main() -> None:
          RuntimeTargetizerCLI.register_targets(*TARGETS).run()


      if __name__ == "__main__":
          main()

   Search paths for source files are added at runtime by the caller, on the
   already-built infrastructure::

      engine_interface.common.path_resolver_instance.add_search_paths(
          Path("/path/to/gnat-sources"),
          Path("/path/to/bb-runtimes/sources"),
      )

