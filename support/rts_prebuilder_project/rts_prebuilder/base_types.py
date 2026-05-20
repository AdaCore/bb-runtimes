#
# Copyright (C) 2025-2026, AdaCore
#
"""
Common types definitions shared across the package with their docstrings.

Many different types of paths and directories are used and tend to have ambiguous names.
Type hints are used to differentiate them. In a previous implementation, a source could mean
five different things depending on the context. Many types are mostly weak type aliases, but
still help with code readability and some static analysis. Use your editor's hover feature to
see the docstring of a type.

Some of these type hints can seem heavy or redundant. This is not an indication that type
hints should not be used. These type hints were added on an existing code base that already
had that complexity.

Example:
Everything related to scenarios and rules could be reworked to use common abstractions.
Instead of needing many scenario and rule types, all can fit in a single, more coherent data
model with classes and methods.
"""

import sys
from pathlib import Path
from typing import Literal, NewType, TypedDict

UnresolvedPath = NewType("UnresolvedPath", Path)
"""
Strong type alias for a source file path that is not yet resolved on disk.
UnresolvedPath can be relative paths for a source file within a given
search directory.
We use strong typing to enforce the user of this type
to use their SourcePathResolver to resolve these paths to actual
files on disk before using them.
"""

###################################
# Profiles & Platform identifiers #
###################################

type ValidBaseProfileType = Literal[
    "light", "light-tasking", "embedded", "cert", "none"
]
"""Allowed base profiles, Shall be same as VALID_BASE_PROFILES_SET"""
VALID_BASE_PROFILES_SET = {"light", "light-tasking", "embedded", "cert", "none"}

type ProfileNameType = str
"""The commercial name of the profile (Example: auto-nvdrive, light-tasking...),
Do not confuse with ValidBaseProfileType, which are the base profiles,
but it should be based on a base_profile from that set.
see AbstractTarget.base_profile for more detail on the mapping."""

type PlatformIdType = Literal[
    "bb", "deos", "freertos", "linux", "lynx", "pikeos", "vx7r2cert", "qnx"
]
"""Allowed platforms, shall be same as VALID_PLATFORM_IDS_SET
TODO:5 Consider renaming platform to OS
"""

VALID_PLATFORM_IDS_SET = {
    "bb",
    "deos",
    "freertos",
    "linux",
    "lynx",
    "pikeos",
    "vx7r2cert",
    "qnx",
}
"""Set of all valid platform identifier names.

This set contains the allowed values for platform IDs that can be used
during runtime assembly to select platform-specific sources.
"""

type PlatformSpecificSourcesIdType = Literal[
    "srcs",
    "bb_srcs",
    "deos_srcs",
    "freertos_srcs",
    "linux_srcs",
    "lynx_srcs",
    "pikeos_srcs",
    "vx7r2cert_srcs",
    "qnx_srcs",
]
"""
Platform sources identifiers as present in the RtsSourcesDBType
Same as PlatformIdType but with `_srcs` suffix.
With an additional value, `srcs`, to designates common sources.
"""
VALID_PLATFORM_SPECIFIC_SOURCES_SET = {
    "srcs",  # For common sources
    *[x + "_srcs" for x in VALID_PLATFORM_IDS_SET],
}

# TODO Deduce PlatformSpecificSourcesIdType directly from VALID_PLATFORM_SPECIFIC_SOURCES_SET
# same for PlatformIdType from VALID_PLATFORM_IDS_SET

#############
# Scenarios #
#############
"""
Scenario variables used to configure the runtime sources
"""

type ScenarioKeyType = str
"""
Example: "RTS_Profile", "has_CHERI", "Add_Math_Lib"...
"""

type ScenarioValueType = str
"""
Example:
- "yes", "no" for a simple boolean scenario
- "light", "light-tasking" for a scenario with multiple values
"""

type AllPossibleScenarioValuesType = list[ScenarioValueType]
"""
All possible values for a scenario key.
Example: ["yes", "no"]
Default value is always the first value of the list. So for example for
optional features enabled via a "no" or "yes" value, always set 'no' as the
first option to disable the feature by default (light and light-tasking cases).
"""

type AllPossibleScenarioConfigsType = dict[
    ScenarioKeyType, AllPossibleScenarioValuesType
]
"""
All possible scenario keys and their values.

Example:
{ "has_feature_A": ["yes", "no"], "has_feature_B": ["no", "yes"] ... }
"""

type ScenariosConfigurationType = dict[ScenarioKeyType, ScenarioValueType]
"""
This is a collection of scenarios with their respective selected values.
One value per scenario. For example this is can be a build configuration
requested by the user.

The values are within those allowed by the respective AllPossibleScenarioValuesType
instance.

Example: {"RTS_Profile": "light", "has_CHERI": "yes", ... }
"""

#########
# Rules #
#########

"""
Rules have many respresentations described below,
but at their core, they are a just a specfic scenario configuration
Example : Has_CHERI = yes and Has_libc = no
"""

type RuleStringType = str
"""
Given these variables with their defined type
key : ScenarioKeyType
value1 : ScenarioValueType
value2 : ScenarioValueType

Rules are take the following formats:
* Simple rule: "key:value1"
* Multiple comma separate values rule (can be n value): "key:value1,value2"
* Negated value rule: "key:!value1"

Each rule string evaluates to a boolean:

a) a simple value (e.g. RTS_Profile:light): evaluated to True if
    RTS_Profile is set to "light"
b) a coma-separated list of values (e.g. RTS_Profile:light,light-tasking):
    evaluated to True if RTS_Profile is "light" OR "light-tasking"
c) a negated value, preceded with an exclamation point (e.g.
    RTS_Profile:!light): evaluated to True if RTS_Profile is not "light".

See also ScenarioKeyType and ScenarioValueType definitions.

These rules are only resolved at build time. Not when the RTS source tree is generated.
"""

type RuleStringListType = list[RuleStringType]
"""
Several rules can be associated in a list, the list also resolves to a boolean:
if several rules are defined in the list, then a logical "and" is used to
to consider the rule as valid.
"""

type RuleScenariosType = dict[ScenarioKeyType, list[ScenarioValueType]]
"""
Dictionary of scenario keys and only their acceptable values for a given
SourceFamilyIdType to be used.

Example: {'RTS_Profile': ['light', 'light_tasking'], 'Memory_Profile': ['small']}

Do not confuse with AllPossibleScenarioConfigsType , even if they are both
dict[str, list[str]]. the difference is at their content and usage.
for AllPossibleScenarioConfigsType : the values are all possible values for all scenarios,
                                     no matter which sources it's associated to.
for RuleScenariosType: The values are only the acceptable values for each scenario
                       for a given sources selection. (it can be "conditions" or "requires")
                       So it's usually a very small subset of all possible values.

See The Rule class, it adds more helpers to interact with this type, including
parsing from RuleStringListType and match checking.  Why parsing? it is a decoded
version for of the raw (RuleStringListType) scenarios lists The notions of comma
separated scenario values and negated values are removed to have this pythonic representation.

Parsing Example: for a RuleStringListType value of ['RTS_Profile:!embedded,!cert']
The parsing gives
-> {'RTS_Profile': ['light', 'light_tasking']}
This conversion uses a AllPossibleScenarioConfigsType  instance to know all possible values,
and execute its reasoning.
"""


#######################
# RtsSources database #
#######################

type SourceFamilyIdType = str
"""
These correspond to the categories of the sources.

NOTE They are also used as subdirectories for those source within installation paths.

Example: "common", "gcc_math", "light/exceptions"...
"""


class SourcesFamilyEntryType(TypedDict, total=False):
    """
    Collection of sources with their dependencies and condition flags.

    For each family:

    * 'srcs': a list of sources are defined. Which will be used to populate
      the output RTS tree.
    * '<platform>_srcs': Same but for platform specific sources. Only one will be selected
      when generating the output RTS. (These platforms match values of
      VALID_PLATFORM_SPECIFIC_SOURCES_SET)

    For both 'srcs' amd '<platforms>_srcs', their values or list of UnresolvedPath.

    Rules, can be:

    * 'conditions': A list of rules (RuleStringListType), if that list evaluates to True,
      then the sources from the family should be used by the targetizer.
    * 'requires': A list of rules (RuleStringListType), that must be also set.
      This is used to express dependencies between source families.

    Example::

        {
            "conditions": ["Add_Image_LL_Decimal:yes"],
            "srcs": ["libgnat/s-imde64.ads"],
            "requires": ["Add_Image_Decimal:yes"],
        }

    NOTE: total=False because many fields are optional.
    """

    # Common Sources lists
    srcs: list[UnresolvedPath]

    # One of the following Platform sources lists
    bb_srcs: list[UnresolvedPath]
    deos_srcs: list[UnresolvedPath]
    freertos_srcs: list[UnresolvedPath]
    linux_srcs: list[UnresolvedPath]
    lynx_srcs: list[UnresolvedPath]
    pikeos_srcs: list[UnresolvedPath]
    vx7r2cert_srcs: list[UnresolvedPath]
    qnx_srcs: list[UnresolvedPath]

    # Conditions
    conditions: RuleStringListType

    # Dependencies
    requires: RuleStringListType


type RtsSourcesDBType = dict[SourceFamilyIdType, SourcesFamilyEntryType]
"""
RtsSourceType
This is the main configuration database, it has a list of all source families
and for each family, the sources, condition scenarios and dependency scenarios.
"""

type RawRtsSourcesDBType = dict[
    SourceFamilyIdType, dict[str, list[str] | RuleStringListType]
]
"""
Same as RtsSourcesDBType but without the UnresolvedPath strong typing.
This type is used when reading the DB as written by the user before turning
it into a strongly typed RtsSourcesDBType instance.
"""

############################
# The JSON rts descriptor  #
############################

"""
These types come in use when dealing with the 'rts-sources.json'.
This is the most import output of the RTS source tree generation process.
RTSDescriptorType is the main type.
"""

type GnatOrGnarlType = Literal["gnat", "gnarl"]
"""
The runtime sources are split in two libraries

- gnat: The main runtime library
- gnarl: The Tasking library

For restricted runtimes without tasking (Example: Light), "gnarl" lib doesn't exist.
"""


class RTSLibDescType(TypedDict):
    """One RTS lib descriptor (e.g. 'gnat' or 'gnarl')."""

    scenarios: "AllPossibleScenarioConfigsType"
    sources: "ScenarioLeafType"


type RTSDescriptorType = dict[GnatOrGnarlType, RTSLibDescType]
"""
This is the dict version of the 'rts-sources.json' JSON file.
It's a listing of the ADA source files but also:

  - The list of all scenarios applicable to the source tree.
  - The mapping between each scenario case and what sources should be used
    when that scenario is valid. (Example: If scenario key has_CHERI is "yes",
    then the sources related to CHERI (already present in the source tree)
    should be included in the targetizing process.
  - The sources are always shown by their relative path to the JSON file. So this
    file serves to find the actual source files in the assembled source tree.

The dict nest has this format::

    {
        "gnat" :
        {
            "scenarios": AllPossibleScenarioConfigsType
            # So basically all the scenarios selectable by the user of the source tree
            # The scenarios on this list are list by popularity (see lib_scenarios)

            "sources": ScenarioLeafType # See below
        }
        "gnarl" : # optional
        {
            # Same layout as gnat
        }
    }

TODO It's too complicated to describe this type fully using type hints.
But, A serializable class can be used to reprensent this datatype with
stronger typing. (python dataclasses or pydantic object).
"""

type DefinedScenarioConfigurationStringType = str
"""
DefinedScenarioConfigurationString is very close to RuleString (See RuleStringType)
but RuleString can have multiple valued "RTS_Endianess:big,little", but only has one value.

Example: "RTS_Endianess:big"
"""

type ScenarioLeafType = dict[
    DefinedScenarioConfigurationStringType | Literal["_srcs"],
    "ScenarioLeafType | list[str]",
]
"""
Each ScenarioLeafType instance is a dict of this form:
{
    _srcs: list[str]
    # Each string being the location of the source file, relative to
    # the location of the JSON descriptor file.
    # This serves to find the actual location of the source
    # This logic of finding the files is done in SharedRTSSources class

    Some DefinedScenarioConfigurationStringType instance: ScenarioLeafType object (recursive)
}

The goal of this representation is if the scenario string (such as "RTS_Endianess:big")
is set by the targetizer of the RTS, then the corresponding SourceLeafDict should be built.
And the recursivess is due to the fact that a scenario can make possible another
scenario.
"""

#######################################

# Notes on typing:
# NOTE For more advanced data validation, pydantic.BaseModel could be used
# as child replacement for types.
#  It can have two advantages over python type annotations:
# 1. Runtime data validation (not just checked my static analysis), in
#    users are providing their own RtsSourcesDBType instances.
# 2. Better error messages: Pydantic provides more informative error messages
#    when validation fails, making it easier to identify and fix issues.

#######################################
# Sanity check on the type definitions
# NOTE This is not data checking ! See note above of runtime data validation.
# NOTE only low hanging fruits are checked here.
#######################################


def assert_platform_list_is_valid() -> None:
    """
    This assertions check if the platform is well declared in the type
    definitions.
    Warning: This does not check the actual type instances data.
    """

    if sys.version_info <= (3, 12):
        # Skip, annotations are not available
        # We add ignore unreachable because mypy thinks this is always true
        return

    ref: set[str] = VALID_PLATFORM_SPECIFIC_SOURCES_SET
    td_ann = getattr(SourcesFamilyEntryType, "__annotations__", {})
    td_platform_keys = {k for k in td_ann if k.endswith("srcs")}

    # one set equality check
    if ref != td_platform_keys:
        raise RuntimeError(
            f"Enum values and TypedDict keys differ.\n"
            f"Missing in TypedDict: {ref - td_platform_keys}\n"
            f"Extra in TypedDict: {td_platform_keys - ref}"
        )


assert_platform_list_is_valid()
