#
# Copyright (C) 2025-2026, AdaCore
#

"""
This is a concrete implementation of abstract_infrastructure module
"""

from .engine_interface import engine_interface
from rts_prebuilder.abstract_infrastructure import (
    GprSwitches,
    Profile,
    SourcesAndFlagsComponent,
)
from .target import DFBBTarget, Target
from .def_gpr_xml_gen import RuntimeXmlSource
from .profile_factories import (
    cert_profile,
    embedded_profile,
    light_profile,
    light_tasking_profile,
    none_profile,
)

__all__ = [  # For engine
    "engine_interface",
    # For target definition by inheritance. A target declares its runtimes on
    # ``profiles`` (see the profile factories below). Each profile carries
    # the components that make up its runtime.
    "Target",
    "SourcesAndFlagsComponent",
    # The switch set both builds are described with, returned by
    # ``get_app_switches`` and ``get_runtime_switches``.
    "GprSwitches",
    # Custom Targets pre-filled by this infrastructure
    "DFBBTarget",
    # runtime.xml source: a target declares one on ``runtime_xml_source`` when
    # it ships a hand-maintained file instead of a generated runtime.xml.
    "RuntimeXmlSource",
    # Profile value object + factories for the standard runtimes. A target's
    # ``profiles`` returns these. A custom profile builds one directly.
    "Profile",
    "none_profile",
    "light_profile",
    "light_tasking_profile",
    "embedded_profile",
    "cert_profile",
]
