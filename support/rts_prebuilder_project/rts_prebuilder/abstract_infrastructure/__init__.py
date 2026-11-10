#
# Copyright (C) 2025-2026, AdaCore
#

"""
Classes in this module provide an consolidated interface for three kind of dependents:

* Engine subpackage

* Implementers of concrete infrastructure packages

* End users

It's main goal is to reduce the set of abstractions that the engine
deals with, to reduce the coupling between it and any infrastructure
specific details.

Any class/function that is part of this subpackage (abstract_infrastructure) should adhere to these rules:

* Must NOT have any external dependencies (no external imports).
  Even within the rts_prebuilder package, it must be self-contained.
  An expception is made for end_user_data subpackage, which
  is there to reduce the number of parameters to

* Must be mandatory for capturing the user's data. Not processing it.
  Any processing logic should go in engine subpackage.

This is implemented by providing blueprints (as in abstract ABC classes or dataclasses)

* Must be mandatory for capturing the user’s data.
  Note that sometimes it can be just because it is the type of an
  input to a user provided transformer.
* Must NOT import anything from outside the module's folder.
* Must NOT perform post-processing / logic on user data.

Symbols exposed in ``__all__``, should be the minimal subset actually needed by
the dependents.

Remember that all APIs are strongly typed in this package (enforced with mypy),
so it includes also the types of function parameters.
"""

# Reminder: If you need to import an external dependency,
#           then you're doing something wrong. (See above rules)

from .common.resolver_core import SourcePathResolver
from .common.default_resolver import DefaultSourcePathResolver
from .common.resolver_steps import AbstractResolutionStep, OutcomeDescription
from .common.source_file import (
    SourceFile,
    GprSwitches,
    Loader,
    dest_name,
)
from .engine_interface import (
    EngineToInfrastructureInterface,
    get_infrastructure_interface,
)
from .runtime_prebuilder.abstract_target import AbstractTarget
from .runtime_prebuilder.abstract_target_generator import AbstractTargetGenerator
from .runtime_prebuilder.component import (
    IncompatibleComponent,
    SourcesAndFlagsComponent,
)
from .runtime_prebuilder.runtime_profile import Profile

__all__ = [
    # Useful for all engine code, it should be able to work completely
    # on abstract classes.
    # As well as for Target / Archs specializations.
    "AbstractTarget",
    "AbstractTargetGenerator",
    "IncompatibleComponent",
    "SourcesAndFlagsComponent",
    "Profile",
    "SourceFile",
    "GprSwitches",
    "Loader",
    "dest_name",
    # Engine code + for implementers a concrete infrastructure
    # Must declare and fill a (engine_interface: EngineInterface) object
    # Interface classes
    "EngineToInfrastructureInterface",
    "SourcePathResolver",
    "DefaultSourcePathResolver",
    "AbstractResolutionStep",
    "OutcomeDescription",
    # Helpers
    "get_infrastructure_interface",
]
