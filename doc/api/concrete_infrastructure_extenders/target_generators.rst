.. Copyright (C) 2026, AdaCore

=================
Target Generators
=================

A generator stands in for a family of targets whose variants differ only in data,
so the variants do not have to be written out one class each.
:meth:`~rts_prebuilder.engine.RuntimePrebuilderCLI.register_targets` accepts
generators alongside concrete targets.

.. autoclass:: rts_prebuilder.abstract_infrastructure.AbstractTargetGenerator
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:
