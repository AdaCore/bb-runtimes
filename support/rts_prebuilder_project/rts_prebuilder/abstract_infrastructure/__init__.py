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
from .common.source_file import SourceFile
from .engine_interface import (
    CommonInterface,
    EngineToInfrastructureInterface,
    get_infrastructure_interface,
)
from .runtime_targetizer.abstract_arch_support import AbsractArchSupport
from .runtime_targetizer.abstract_profiles_generator import (
    AbstractProfileToScenarioGenerator,
)
from .runtime_targetizer.abstract_target import AbstractTarget
from .runtime_targetizer.abstract_target_generator import AbstractTargetGenerator
from .runtime_targetizer.component_mixin import SourcesAndFlagsComponentMixin

# Some data types
from .runtime_targetizer.runtime_config import RuntimeConfig

__all__ = [
    # Useful for all engine code, it should be able to work completely
    # on abstract classes.
    # As well as for Target / Archs specializations.
    "AbstractTarget",
    "AbstractTargetGenerator",
    "SourcesAndFlagsComponentMixin",
    "AbsractArchSupport",
    "RuntimeConfig",
    "SourceFile",
    # Engine code + for implementers a concrete infrastructure
    # Must declare and fill a (engine_interface: EngineInterface) object
    # Interface classes
    "EngineToInfrastructureInterface",
    "CommonInterface",
    "SourcePathResolver",
    "DefaultSourcePathResolver",
    "AbstractResolutionStep",
    "OutcomeDescription",
    "AbstractProfileToScenarioGenerator",
    # Helpers
    "get_infrastructure_interface",
]
