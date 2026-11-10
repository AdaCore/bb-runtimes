.. Copyright (C) 2025-2026, AdaCore

==========
Components
==========

SourcesAndFlagsComponent
========================

.. autoclass:: rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

IncompatibleComponent
=====================

Raised from a component's ``contribute`` when it is run for a target whose
platform it does not support. Such a component is only ever selected for its own
platform, so the exception reports a mis-selection. It is not caught, and it is
not a way to opt a component out of a runtime.

.. autoclass:: rts_prebuilder.abstract_infrastructure.IncompatibleComponent
   :members:
   :show-inheritance:
