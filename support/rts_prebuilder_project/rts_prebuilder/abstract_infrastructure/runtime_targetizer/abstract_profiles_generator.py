#
# Copyright (C) 2025-2026, AdaCore
#

from abc import ABC, abstractmethod

from rts_prebuilder.base_types import ScenariosConfigurationType, ProfileNameType

from .abstract_target import AbstractTarget


class AbstractProfileToScenarioGenerator(ABC):

    target: AbstractTarget
    """
    The target instance used to generate the RTS Profile
    """

    def __init__(self, target: AbstractTarget) -> None:
        """
        :type target: AbstractTarget for which the profiles are generated
                      this is used to get target specific attributes
        """
        self.target = target

    @abstractmethod
    def profile_to_scenarios(
        self, profile: ProfileNameType
    ) -> ScenariosConfigurationType:
        """
        Given a profile name, return the scenarios configuration for that profile

        :param profile: The profile name
        :return: The scenarios configuration for the given profile
        """
        ...
