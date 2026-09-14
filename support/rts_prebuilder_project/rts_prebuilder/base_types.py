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
"""

from pathlib import Path
from typing import Literal, NewType

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

This set contains the allowed values for platform IDs that components can use
to select platform-specific sources.
"""

# TODO Deduce PlatformIdType directly from VALID_PLATFORM_IDS_SET

#############
# Libraries #
#############

type GnatOrGnarlType = Literal["gnat", "gnarl"]
"""Name of a runtime library, which is also the source subdirectory it lives in:

- ``gnat``: the main runtime library (always present)
- ``gnarl``: the tasking library (present only for tasking runtimes)

Whether a runtime provides ``gnarl`` is not declared anywhere. The specializer
derives it from the sources the runtime's components install, and exposes it as
``RuntimeSpec.libs_tuple``.
"""
