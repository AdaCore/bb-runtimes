.. Copyright (C) 2025-2026, AdaCore

=========================
Source Files and Switches
=========================

The value types a component registers. An extender builds these directly only for
the lower-level registrations; the source and switch helpers build them
otherwise.

SourceFile
==========

Files declared with ``SourceFile`` are copied or linked without substitution.

.. autoclass:: rts_prebuilder.abstract_infrastructure.SourceFile
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

TemplateFile
============

``TemplateFile`` inherits path and destination handling from ``SourceFile``.
The source helpers select this type for ``.tmpl`` paths. Templates use Python's
``string.Template`` syntax: ``${name}`` for parameters and ``$$`` for a literal
dollar sign.

.. autoclass:: rts_prebuilder.abstract_infrastructure.TemplateFile
   :members:
   :special-members: __init__
   :show-inheritance:

``TemplateFilePair.install`` uses the same installer as ``FilePair.install``.
It writes rendered content, including when ``--link`` is enabled.

.. autoclass:: rts_prebuilder.engine.template.Template
   :members: parameters, render

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
