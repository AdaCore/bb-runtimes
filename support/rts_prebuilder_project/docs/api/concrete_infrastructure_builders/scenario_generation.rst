.. Copyright (C) 2025-2026, AdaCore

====================
Scenario Generation
====================

.. autoclass:: rts_prebuilder.abstract_infrastructure.AbstractProfileToScenarioGenerator
   :members:
   :exclude-members: _*
   :special-members: __init__
   :show-inheritance:

.. admonition:: Real World Example — ``bb_runtimes_targets_gen``
   :class: tip

   ``DefaultProfileToScenariosGenerator`` is the shipped concrete subclass.
   Dispatches on the target's base profile (trimmed view):

   .. code-block:: python

      from typing import override

      from rts_prebuilder.abstract_infrastructure import (
          AbstractProfileToScenarioGenerator,
      )
      from rts_prebuilder.base_types import (
          ProfileNameType,
          ScenariosConfigurationType,
      )


      class DefaultProfileToScenariosGenerator(AbstractProfileToScenarioGenerator):
          @override
          def profile_to_scenarios(
              self, profile: ProfileNameType
          ) -> ScenariosConfigurationType:
              base_profile = self.target.base_profile(profile)
              match base_profile:
                  case "light":
                      return self.light_scenarios()
                  case "light-tasking":
                      return self.light_tasking_scenarios()
                  case "embedded":
                      return self.embedded_scenarios()
                  case "cert":
                      return self.cert_scenarios()

   A target hooks this generator in by overriding
   :attr:`~rts_prebuilder.abstract_infrastructure.AbstractTarget.profile_to_scenarios_generator`
   on its base class.

