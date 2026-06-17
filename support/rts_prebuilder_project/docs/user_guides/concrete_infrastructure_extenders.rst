.. Copyright (C) 2025-2026, AdaCore

========================================
Concrete Infrastructure Extender Guide
========================================

.. admonition:: Are You In The Right Place?
   :class: routing-box

   * You need to build a brand-new infrastructure from scratch (no existing
     ``EngineToInfrastructureInterface`` for your repo layout) →
     :doc:`concrete_infrastructure_builders`.
   * You only want to *invoke* an existing infrastructure's CLI to generate a
     buildable runtime → :doc:`end_users_guide`.
   * You have an existing infrastructure and want to add a new target to it →
     continue below.

This guide is for developers who want to **extend an existing runtime infrastructure** by defining new targets (boards, CPUs, or platforms).

You'll define new Target classes using the existing source repositories and infrastructure, without building everything from scratch.

Prerequisites
=============

* Source files for your target (board support packages, linker scripts, etc.) that want to integrate
* An existing infrastructure interface for your repository structure (see :doc:`concrete_infrastructure_builders` if you need to create one),
  this is usually provided by the repository of targets that you are using.

Creating a New Target
=====================

Step 1: Set Up Your Target Module
----------------------------------

Create a new Python module for your target family.

Step 2: Define Your Target Class
---------------------------------

Inherit from :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`
— or, much more commonly, from a pre-filled subclass shipped by your
infrastructure.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   Pre-filled bases re-exported from ``bb_runtimes_targets_gen.concrete_infrastructure``:
   ``Target`` (generic) and ``DFBBTarget`` (bareboard, with FPU and timer
   defaults). Extenders typically inherit from ``DFBBTarget``.

``AbstractTarget`` has six abstract members in total:

* ``name`` — board name used to build the runtime directory
  (``<profile>-<name>``, e.g. ``light-zynqmp``). See also ``cli_name``
  (non-abstract, defaults to ``name``) if the CLI selector needs to differ
  from the runtime-directory name.
* ``target`` — gprbuild target triplet (e.g. ``aarch64-elf``).
* ``platform`` — :data:`~rts_prebuilder.base_types.PlatformIdType`
  (e.g. ``"bb"``, ``"pikeos"``, ``"linux"``). Usually filled by the
  infrastructure's pre-filled subclass.
* ``system_ads`` — ``dict[profile_name, system.ads_path]``.
* :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profile_to_scenarios_generator` —
  also usually filled by the infrastructure's pre-filled subclass.
* :meth:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.dump_runtime_xml` —
  same.

When extending a pre-filled subclass, you typically only fill ``name``,
``target``, and ``system_ads``. The non-abstract properties (``has_fpu``,
``is_64bit``, ``has_huge_memory``, ``has_timer_64``, …) are then overridden
as needed to describe the hardware.

Also useful: ``compiler_switches`` / ``c_switches`` for build flags, and
``amend_rts`` for last-mile :class:`~rts_prebuilder.abstract_infrastructure.RuntimeConfig`
tweaks (use sparingly — prefer overriding the scenario generator).

Step 3: Add Board-Specific Sources
-----------------------------------

Use the :class:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin` methods:

* :meth:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin.add_gnat_sources` - Add libgnat source files (high-level wrapper)
* :meth:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin.add_gnarl_sources` - Add libgnarl source files (high-level wrapper)
* :meth:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin.add_linker_script` - Add a linker script (optionally scoped to specific loaders via ``loaders=(...)``)
* :meth:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin.add_linker_switch` - Add linker switches
* :meth:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin.append_sources_for_dir` - Lower-level: add multiple source files for an arbitrary subdir

Step 4: Configure Compiler Flags
---------------------------------

Override the ``compiler_switches`` property to return architecture-specific flags.

Step 5: (Optional) Compose with an Architecture Support
--------------------------------------------------------

Architecture-level state (shared sources, common context-switch assembly,
shared linker switches) lives in an
:class:`~rts_prebuilder.abstract_infrastructure.AbsractArchSupport` subclass
— **not** in a base class of your target. Composition happens through the
``parent`` property on
:class:`~rts_prebuilder.abstract_infrastructure.runtime_targetizer.component_mixin.SourcesAndFlagsComponentMixin`:
your target overrides ``parent`` to return an instance of the arch class,
and the mixin's ``get_sources`` / ``linker_switches`` / search-paths
accessors merge the parent contribution recursively.

This is multiple-composition, not multiple-inheritance — the target inherits
from ``AbstractTarget`` only.

Example::

   class Aarch64Target(DFBBTarget):
       @property
       def parent(self) -> ArchSupport:
           return Aarch64Arch()

Step 6: Use Your Target
------------------------

Expose your target instances as a ``TARGETS`` tuple on the package, then
plug them into the engine. Canonical CLI shape:

.. code-block:: python

   from rts_prebuilder.engine import RuntimeTargetizerCLI

   from . import TARGETS
   from <your_infrastructure>.engine_interface import engine_interface

   engine_interface.self_register()


   def main() -> None:
       RuntimeTargetizerCLI.register_targets(*TARGETS).run()

For scripted (non-CLI) use, instantiate
:class:`~rts_prebuilder.engine.RuntimeTargetizer` directly. Either way, the
infrastructure's ``engine_interface`` must have been registered first — see
:ref:`infrastructure_registration`.

.. admonition:: Real World Example — Extending ``bb_runtimes_targets_gen``
   :class: tip

   Three snippets excerpted from
   ``bb_runtimes_targets_gen/targets/aarch64/__init__.py``, showing the
   typical shape of an extender's contribution.

   **1. Architecture support** — register sources shared by every aarch64
   target:

   .. code-block:: python

      from bb_runtimes_targets_gen.concrete_infrastructure import ArchSupport, DFBBTarget


      class Aarch64Arch(ArchSupport):
          @property
          def name(self):
              return "aarch64"

          def __init__(self) -> None:
              super().__init__()
              self.add_gnat_sources(
                  "shared/i-aarch64.ads",
                  "shared/i-aarch64.adb",
              )
              self.add_gnarl_sources(
                  "shared/s-bbcpsp__aarch64.ads",
                  "shared/s-bbcppr__aarch64.adb",
                  "aarch64/context_switch.S",
              )

   **2. Generic target** — point at the arch parent and select a
   ``system.ads`` per profile:

   .. code-block:: python

      class Aarch64Target(DFBBTarget):
          @property
          def target(self) -> str:
              return "aarch64-elf"

          @property
          def parent(self) -> ArchSupport:
              return Aarch64Arch()

          @property
          def system_ads(self):
              return {
                  "light": "system-xi-arm.ads",
                  "light-tasking": "system-xi-arm-sfp.ads",
                  "embedded": "system-xi-arm-full.ads",
              }

   **3. Board-specific target** — register per-loader linker scripts, then
   expose the package's ``TARGETS`` tuple that the symlinked ``__main__``
   picks up:

   .. code-block:: python

      class ZynqMP(Aarch64Target):
          @property
          def name(self):
              return "zynqmp"

          @property
          def loaders(self):
              return ("RAM", "QSPI", "HELIX")

          def __init__(self) -> None:
              super().__init__()
              self.add_linker_script("aarch64/zynqmp/common.ld")
              self.add_linker_script("aarch64/zynqmp/ram.ld", loaders=("RAM",))
              self.add_linker_script("aarch64/zynqmp/qspi.ld", loaders=("QSPI",))


      TARGETS = (ZynqMP(),)
