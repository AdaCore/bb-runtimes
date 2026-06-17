.. Copyright (C) 2025-2026, AdaCore

===============
End Users Guide
===============

.. admonition:: Are You In The Right Place?
   :class: routing-box

   * You want to add a new target to an existing infrastructure
     (e.g. ``bb_runtimes_targets_gen``) → :doc:`concrete_infrastructure_extenders`.
   * You need to build a brand-new infrastructure from scratch →
     :doc:`concrete_infrastructure_builders`.
   * You just want to generate a runtime against a target that's already
     supported → continue below.

For users who want to generate buildable runtimes against targets already
supported by an existing infrastructure package.

Overview
========

As an end user, you do not invoke :class:`~rts_prebuilder.engine.RuntimeAssembler`
or :class:`~rts_prebuilder.engine.RuntimeTargetizer` directly. The
infrastructure package ships **CLI wrappers** around them — these are your
entry points. Target names, supported flags, and any assembly entry point
are defined by the infrastructure, not by ``rts_prebuilder``.

.. note::

   The target name accepted on the CLI is
   :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.cli_name`,
   which defaults to
   :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.name`.
   An infrastructure or extender may override ``cli_name`` to expose a
   different selector than the runtime-directory name.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   Each target family is its own runnable module::

      python -m bb_runtimes_targets_gen.targets.aarch64 --list-targets
      python -m bb_runtimes_targets_gen.targets.aarch64 zynqmp --output-dir ./install

   Inspect the wrapper modules at
   ``bb_runtimes_targets_gen/targets/<arch>/__main__.py`` to see how the
   CLI is built on top of ``rts_prebuilder``.

Build your own scripts
======================

If the infrastructure's CLIs don't fit your workflow, you can drive the
engine classes yourself:

* :class:`~rts_prebuilder.engine.RuntimeAssembler` — Phase 1 (source assembly).
* :class:`~rts_prebuilder.engine.RuntimeTargetizer` — Phase 2 (targetization).
* :class:`~rts_prebuilder.engine.RuntimeAssemblerCLI` / :class:`~rts_prebuilder.engine.RuntimeTargetizerCLI`
  — argparse helpers wrapping the two phases.

The same caveat applies: the infrastructure must be registered first (its
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
instance must have called
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`).
See :ref:`infrastructure_registration`.

See the :doc:`../api/end_users_api` for the complete API.
