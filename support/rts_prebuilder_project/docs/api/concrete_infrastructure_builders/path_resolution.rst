.. Copyright (C) 2025-2026, AdaCore

===============
Path Resolution
===============

Components name sources by relative path. A resolver turns each one into a file
on disk, and records what it did so the install carries an auditable resolution
record.

SourcePathResolver
==================

.. autoclass:: rts_prebuilder.abstract_infrastructure.SourcePathResolver
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

DefaultSourcePathResolver
=========================

The default resolver looks for files in an ordered list of search paths. The
caller, an end-user script or an infrastructure entry point, feeds those paths in
at run time, reaching the resolver through the registered engine interface.

.. autoclass:: rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

.. code-block:: python

   from pathlib import Path

   engine_interface.path_resolver_instance.add_search_paths(
       Path("/path/to/runtime-sources"),
       Path("/path/to/your-target-sources"),
   )

Resolution steps
================

A resolver is a chain of steps. Subclass ``SourcePathResolver`` with your own
chain only when the default one does not fit.

.. autoclass:: rts_prebuilder.abstract_infrastructure.AbstractResolutionStep
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

.. autodata:: rts_prebuilder.abstract_infrastructure.OutcomeDescription

Resolution records
==================

.. autoclass:: rts_prebuilder.abstract_infrastructure.common.resolver_core.ResolutionRecord
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

.. autoclass:: rts_prebuilder.abstract_infrastructure.common.resolver_core.GitMetadata
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:
