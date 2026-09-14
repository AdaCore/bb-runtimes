#
# Copyright (C) 2025-2026, AdaCore
#

"""
This module has simple wrappers around infrastructure interface through get_infrastructure_interface

It must be the entrypoint for any interaction with the infrastructure.

No logic should be added to this module.
"""

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcePathResolver,
    get_infrastructure_interface,
)
from rts_prebuilder.base_types import UnresolvedPath

# Accessors to common interface data

_resolver: SourcePathResolver


def get_resolver() -> SourcePathResolver:
    """Returns an instance of the path resolver class as defined in the infrastructure interface"""
    global _resolver
    if "_resolver" not in globals():
        _resolver = get_infrastructure_interface().path_resolver_instance
    return _resolver


def resolve_and_read_file(path: Path) -> str:
    """Resolves and reads the content of path and returns it as a string"""
    return get_resolver().resolve_and_readtext(UnresolvedPath(path))


def generate_runtime_xml(target: AbstractTarget, profile: Profile) -> str:
    """Returns a target's runtime.xml content via the infrastructure hook."""
    generator = get_infrastructure_interface().runtime_xml_generator
    if generator is None:
        raise RuntimeError("infrastructure registered no runtime_xml_generator")
    return generator(target, profile)
