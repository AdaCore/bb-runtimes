#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass, field
from typing import Callable, ClassVar

from rts_prebuilder.end_user_data.logger import get_logger

from .common.default_resolver import DefaultSourcePathResolver
from .common.resolver_core import SourcePathResolver


log = get_logger(__name__)


@dataclass
class EngineToInfrastructureInterface:
    """
    Base infrastructure interface for the engine. (Only the engine)

    If prebuilder interface is not set, then that feature
    cannot be used.
    """

    path_resolver_instance: SourcePathResolver = field(
        default_factory=DefaultSourcePathResolver
    )
    """The resolver the engine uses to turn unresolved source paths into files."""

    runtime_xml_generator: Callable[..., str] | None = None
    """Concrete-infrastructure hook returning a target's runtime.xml content,
    given ``(target, profile)``. The engine calls it through this interface so
    it needs no knowledge of the concrete generator; a target never generates
    its own runtime.xml."""

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
