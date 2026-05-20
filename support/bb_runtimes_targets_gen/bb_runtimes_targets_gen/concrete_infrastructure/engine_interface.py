#
# Copyright (C) 2025-2026, AdaCore
#

"""
Interface with rts_prebuilder's engine

The goal of this file is to fill a engine_interface
instance.

Search paths for source files (Ada sources, datafiles) are NOT set here.
Callers must add them via::

    engine_interface.common.path_resolver_instance.add_search_paths(...)
"""

from rts_prebuilder.abstract_infrastructure import (
    CommonInterface,
    EngineToInfrastructureInterface,
)

from .common.sources_db import all_scenarios, all_sources_listing


engine_interface = EngineToInfrastructureInterface(
    common=CommonInterface(
        all_sources_listing=all_sources_listing,
        all_possible_scenarios=all_scenarios,
    ),
)
