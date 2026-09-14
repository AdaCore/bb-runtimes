.. Copyright (C) 2025-2026, AdaCore

=========================
Source Files and Switches
=========================

The value types a component registers. An extender builds these directly only for
the lower-level registrations; the source and switch helpers build them
otherwise.

SourceFile
==========

.. autoclass:: rts_prebuilder.abstract_infrastructure.SourceFile
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

Loader
======

One link configuration of the end user's program. The rules for declaring them
are in :ref:`extenders_loaders_ref`.

.. autoclass:: rts_prebuilder.abstract_infrastructure.Loader
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

.. autofunction:: rts_prebuilder.abstract_infrastructure.dest_name

GprSwitches
===========

.. autoclass:: rts_prebuilder.abstract_infrastructure.GprSwitches
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:
