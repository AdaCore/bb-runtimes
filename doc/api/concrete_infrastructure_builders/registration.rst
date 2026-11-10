.. Copyright (C) 2025-2026, AdaCore

.. _infrastructure_registration:

============
Registration
============

The engine reaches the infrastructure through a singleton holder. Registration is
an explicit step: build an
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
instance, then call its
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`
method before any engine entry point runs. Importing the module that defines the
instance is not enough.

.. code-block:: python

   from rts_prebuilder.engine import RuntimePrebuilderCLI

   from . import TARGETS
   from your_infrastructure.engine_interface import engine_interface

   engine_interface.self_register()


   def main() -> None:
       RuntimePrebuilderCLI.register_targets(*TARGETS).run()

Only one infrastructure interface can be active at a time. Registering a different
instance over an existing one logs an error. The new instance still replaces the
old one, and the log is the only signal.

With no interface registered, the engine raises when it first needs the
infrastructure.

.. autofunction:: rts_prebuilder.abstract_infrastructure.get_infrastructure_interface
