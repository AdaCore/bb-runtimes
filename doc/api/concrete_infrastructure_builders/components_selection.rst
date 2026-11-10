.. Copyright (C) 2026, AdaCore

====================
Components Selection
====================

A target declares which runtimes it offers as a list of
:class:`~rts_prebuilder.abstract_infrastructure.Profile` values. Each ``Profile``
carries the set of components that make up that runtime, in its
:attr:`~rts_prebuilder.abstract_infrastructure.Profile.components`. The class
itself is documented under :doc:`../concrete_infrastructure_extenders/profiles`.

``rts_prebuilder`` does no component selection. It merges ``profile.components``
with the target's own always-on tree and resolves the result. Deciding what goes into that list is
the infrastructure's design, and this page records the shape that has proved to
work.

Selection is a plain function per base profile: given a target, it returns the
flat set of components for that runtime. A profile factory calls the matching
function and stores the result on ``Profile.components``. Components themselves
stay out of it: a component declares its sources in ``contribute(target,
profile)``, branching on target facts such as ``target.platform`` or on the
``Profile``, while which components a runtime includes is the selection
function's answer alone.

There is no profile-selector component on the tree.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   One components factory per base profile: ``no_profile_components``,
   ``light_components``, ``light_tasking_components``, ``embedded_components``,
   ``cert_components``. Each returns the flat set for that runtime:

   .. code-block:: python

      def light_components(target):
          ret = _light_subset_components(target)
          ...
          return ret

   The profile factories call the matching one:

   .. code-block:: python

      def light_profile(target, system_ads: Path, *, name="light"):
          return Profile(
              name=name, base_profile="light", system_ads=system_ads,
              components=light_components(target),
          )

   A platform tasking layer is selected the same way, from ``platform`` and
   ``platform_variant``. One component per platform covers every variant of it,
   and picks its sources in ``contribute``:

   .. code-block:: python

      class GnarlPikeos(SourcesAndFlagsComponent):
          def contribute(self, target, profile):
              if target.platform != "pikeos":
                  raise IncompatibleComponent()
              match target.platform_variant:
                  case "pikeos3":
                      self.add_gnarl_sources("s-osinte__pikeos3.ads", ...)
                  ...

   The factory names and the base-profile split are that infrastructure's
   choices, not part of the ``rts_prebuilder`` API.

A custom runtime can bypass the factories entirely: build a ``Profile``, typically
on the ``none`` base, with an explicit ``components`` list.
