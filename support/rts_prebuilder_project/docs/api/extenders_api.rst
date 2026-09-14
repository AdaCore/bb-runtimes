.. Copyright (C) 2025-2026, AdaCore

========================================================
rts_prebuilder API for concrete infrastructure extenders
========================================================

This section documents the ``rts_prebuilder`` contracts for target definitions:
the component and target base classes, profiles, loaders, and source-file types.
Concrete classes and factories from ``bb_runtimes_targets_gen`` are
infrastructure-specific examples, rather than part of this API. See
:doc:`../user_guides/extending_with_your_own_target` for usage instructions.

.. automodule:: rts_prebuilder.abstract_infrastructure
   :no-members:

.. toctree::
   :maxdepth: 1

   concrete_infrastructure_extenders/components
   concrete_infrastructure_extenders/target_definition
   concrete_infrastructure_extenders/profiles
   concrete_infrastructure_extenders/source_files
   concrete_infrastructure_extenders/target_generators
   base_types
