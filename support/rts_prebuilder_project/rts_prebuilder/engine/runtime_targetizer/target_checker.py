#
# Copyright (C) 2025-2026, AdaCore
#

import pathlib
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    SourcesAndFlagsComponentMixin,
)
from rts_prebuilder.base_types import VALID_BASE_PROFILES_SET


def validate_target(target: AbstractTarget) -> None:
    """
    Runtime checks on the target class provided

    This is useful because users of the engine library, can provide their
    own target instance classes.

    DO NOT ADD ANY CHECKS THAT CAN BE DONE STATICALLY.
    The base abstract class AbstractTarget uses type hints and abstract
    properties to enforce the implementation of required properties.
    Running a static type checker (e.g. mypy) on the target class
    must be passing without any errors (and not use of type:ignore).

    This function should only contain checks that cannot be done at static time.
    This keeps this code simple and efficient.

    Note: Implement each check in a separate function. unless it's 1 liner.
    """
    _check_readme_file_attribute(target)
    _check_parent(target)
    _check_system_ads_attribute(target)


def _check_parent(target: AbstractTarget) -> None:
    if target.parent is not None:
        if not isinstance(target.parent, SourcesAndFlagsComponentMixin):
            raise TypeError(
                f"target.parent {target.parent} must be an instance"
                "(not class) of SourcesAndFlagsComponentMixin"
                f" not {type(target.parent)}"
            )


def _check_readme_file_attribute(target: AbstractTarget) -> None:
    if target.readme_file is not None:
        if not isinstance(target.readme_file, pathlib.Path):
            raise TypeError(
                f"Readme file must be a Path instance not {type(target.readme_file)}"
            )


def _check_system_ads_attribute(target: AbstractTarget) -> None:
    """Checks the system_ads dict attribute
    This also checks the target.base_profile() works correctly for each profile
    mentioned in system_ads.
    """

    system_ads = target.system_ads

    if len(system_ads) == 0:
        raise ValueError(
            f"Target {target.name}: system_ads must contain at least one profile"
        )

    for profile, path in system_ads.items():

        base_profile = target.base_profile(profile)

        if base_profile not in VALID_BASE_PROFILES_SET:
            raise ValueError(
                f"Target {target.name}: system_ads contains invalid base_profile '{profile}'"
            )

        if not isinstance(path, str) or len(path) == 0 or not path.endswith(".ads"):
            raise ValueError(
                f"Target {target.name}: system_ads contains invalid path '{path}'"
                f"for base_profile '{profile}'"
            )
