#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass
from typing import ClassVar

from rts_prebuilder.base_types import (
    AllPossibleScenarioConfigsType,
    RawRtsSourcesDBType,
)
from rts_prebuilder.end_user_data.logger import get_logger

from .common.default_resolver import DefaultSourcePathResolver
from .common.resolver_core import SourcePathResolver


log = get_logger(__name__)


@dataclass
class CommonInterface:
    """Common interface used by all features of the engine."""

    all_sources_listing: RawRtsSourcesDBType
    """This raw sources listing that will be processed
       to give a RtsSourcesDBType instance"""

    all_possible_scenarios: "AllPossibleScenarioConfigsType"

    path_resolver_instance: SourcePathResolver = DefaultSourcePathResolver()
    """ An instance of a subclass of SourcePathResolver """


@dataclass
class EngineToInfrastructureInterface:
    """
    Base infrastructure interface for the engine. (Only the engine)

    If targetizer interface is not set, then that feature
    cannot be used.
    """

    common: CommonInterface

    def self_register(self) -> None:
        """Register this interface as the singleton instance."""
        EngineToInfrastructureSingleton.register_interface(self)


class EngineToInfrastructureSingleton:
    """Singleton holder for the EngineToInfrastructureInterface instance."""

    instance: ClassVar[EngineToInfrastructureInterface | None] = None

    @classmethod
    def register_interface(cls, interface: EngineToInfrastructureInterface) -> None:
        if cls.instance is not None and cls.instance is not interface:
            log.error(
                "A different EngineInterfaceSingleton instance is already registered"
                f" (existing: {id(cls.instance)}, new: {id(interface)})"
            )

        cls.instance = interface

    @classmethod
    def get_interface(cls) -> EngineToInfrastructureInterface:
        if cls.instance is None:
            raise Exception("EngineInterfaceSingleton instance not registered")
        return cls.instance

    @classmethod
    def is_registered(cls) -> bool:
        return cls.instance is not None


# Export a typed getter for the singleton instance
def get_infrastructure_interface() -> EngineToInfrastructureInterface:
    return EngineToInfrastructureSingleton.get_interface()
