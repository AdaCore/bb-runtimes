.. Copyright (C) 2025-2026, AdaCore

===============
Path Resolution
===============

SourcePathResolver
==========================

.. autoclass:: rts_prebuilder.abstract_infrastructure.SourcePathResolver
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

DefaultSourcePathResolver
==========================

.. autoclass:: rts_prebuilder.abstract_infrastructure.DefaultSourcePathResolver
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

The default resolver looks for files in an ordered list of search paths. The
caller (an end-user script or an infrastructure entry point) feeds those paths
in at runtime, reaching the resolver through the registered engine interface.

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   .. code-block:: python

      from pathlib import Path

      from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
          engine_interface,
      )


      engine_interface.common.path_resolver_instance.add_search_paths(
          Path("/path/to/gnat-sources"),
          Path("/path/to/bb-runtimes/sources"),
      )

ResolutionRecord
================

.. autoclass:: rts_prebuilder.abstract_infrastructure.common.resolver_core.ResolutionRecord
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

GitMetadata
===========

.. autoclass:: rts_prebuilder.abstract_infrastructure.common.resolver_core.GitMetadata
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:
