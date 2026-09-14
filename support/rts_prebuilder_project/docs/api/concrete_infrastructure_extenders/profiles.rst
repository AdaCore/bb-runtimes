.. Copyright (C) 2026, AdaCore

========
Profiles
========

A target declares the runtimes it offers as a list of ``Profile`` values, on its
:attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profiles` property.
A ``Profile`` is a pure-data value object and holds no selection behaviour.

Filling :attr:`~rts_prebuilder.abstract_infrastructure.Profile.components` is the
concrete infrastructure's job. How a given infrastructure does it is described in
:doc:`../concrete_infrastructure_builders/components_selection`.

.. autoclass:: rts_prebuilder.abstract_infrastructure.Profile
   :members:
