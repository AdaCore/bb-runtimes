#
# Copyright (C) 2025-2026, AdaCore
#

"""
Interface with rts_prebuilder's engine

The goal of this file is to fill a engine_interface
instance.

Search paths for source files (Ada sources, datafiles) are NOT set here.
Callers must add them via::

    engine_interface.path_resolver_instance.add_search_paths(...)
"""

from rts_prebuilder.abstract_infrastructure import EngineToInfrastructureInterface

from .def_gpr_xml_gen import generate_runtime_xml


engine_interface = EngineToInfrastructureInterface(
    runtime_xml_generator=generate_runtime_xml,
)
