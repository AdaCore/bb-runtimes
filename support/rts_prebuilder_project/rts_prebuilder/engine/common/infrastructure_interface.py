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
    SourcePathResolver,
    get_infrastructure_interface,
)
from rts_prebuilder.base_types import (
    AllPossibleScenarioConfigsType,
    RtsSourcesDBType,
    UnresolvedPath,
)

from .sources_db_processor import sources_db_proc

# Accessors to common interface data


def get_all_possible_scenarios() -> AllPossibleScenarioConfigsType:
    """Returns all possible scenarios as defined in the infrastructure interface"""
    return get_infrastructure_interface().common.all_possible_scenarios


def get_all_sources_listing() -> RtsSourcesDBType:
    """Returns a sanity-checked RtsSourcesDBType instance representing all sources listing"""
    return sources_db_proc(get_infrastructure_interface().common.all_sources_listing)


_resolver: SourcePathResolver


def get_resolver() -> SourcePathResolver:
    """Returns an instance of the path resolver class as defined in the infrastructure interface"""
    global _resolver
    if "_resolver" not in globals():
        _resolver = get_infrastructure_interface().common.path_resolver_instance
    return _resolver


def resolve_and_read_file(path: Path) -> str:
    """Resolves and reads the content of path and returns it as a string"""
    return get_resolver().resolve_and_readtext(UnresolvedPath(path))
