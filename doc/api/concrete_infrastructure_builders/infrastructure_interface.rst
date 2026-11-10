.. Copyright (C) 2025-2026, AdaCore

==============================
Infrastructure Implementation
==============================

The one door between the engine and a concrete infrastructure. Build one
instance, fill it, and register it (see :ref:`infrastructure_registration`).

EngineToInfrastructureInterface
===============================

.. autoclass:: rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

Reading a source through the resolver
=====================================

An infrastructure's generators sometimes need the contents of a source file,
looked up the same way the engine looks up runtime sources.

.. autofunction:: rts_prebuilder.engine.resolve_and_read_file
