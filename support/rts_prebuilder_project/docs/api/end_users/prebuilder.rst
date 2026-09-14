.. Copyright (C) 2025-2026, AdaCore

=====================
RuntimePrebuildDriver
=====================

The ``rts_prebuilder`` driver handles one target. Call
``create_runtimes_writers()`` to specialize the target for each of its profiles.
Then call ``install_runtimes(destination)`` to write them out.

.. autoclass:: rts_prebuilder.engine.RuntimePrebuildDriver
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:
