.. Copyright (C) 2025-2026, AdaCore

.. _infrastructure_registration:

============
Registration
============

The engine reaches the infrastructure through a singleton holder. Registration
is an explicit step: build an :class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
instance, then call its
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`
method before any engine entry point runs.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   .. code-block:: python

      from rts_prebuilder.engine import RuntimeTargetizerCLI

      from . import TARGETS
      from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
          engine_interface,
      )

      engine_interface.self_register()


      def main() -> None:
          RuntimeTargetizerCLI.register_targets(*TARGETS).run()

Only one infrastructure interface can be active at a time. Registering a
different instance over an existing one logs an error (the new instance
still replaces the old one — the log is the only signal).

You can check whether an interface is registered, and retrieve it, with:

.. autofunction:: rts_prebuilder.abstract_infrastructure.get_infrastructure_interface
