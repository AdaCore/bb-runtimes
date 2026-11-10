#
# Copyright (C) 2025-2026, AdaCore
#

import pathlib
from rts_prebuilder.abstract_infrastructure import AbstractTarget, Loader, Profile
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
    _check_profiles_attribute(target)


def _check_readme_file_attribute(target: AbstractTarget) -> None:
    if target.readme_file is not None:
        if not isinstance(target.readme_file, pathlib.Path):
            raise TypeError(
                f"Readme file must be a Path instance not {type(target.readme_file)}"
            )


def _check_profiles_attribute(target: AbstractTarget) -> None:
    """Check the profiles attribute. It must be non-empty, name each profile
    once, name a valid base profile, and give a well-formed system.ads path.
    The fan-out relies on the names being unique.
    """

    profiles = target.profiles

    if len(profiles) == 0:
        raise ValueError(
            f"Target {target.name}: profiles must contain at least one profile"
        )

    names = [p.name for p in profiles]
    duplicated = sorted({n for n in names if names.count(n) > 1})
    if duplicated:
        raise ValueError(
            f"Target {target.name}: duplicate profile name(s) {duplicated}"
        )

    for profile in profiles:
        if profile.base_profile not in VALID_BASE_PROFILES_SET:
            raise ValueError(
                f"Target {target.name}: profile '{profile.name}' has invalid"
                f" base_profile '{profile.base_profile}'"
            )

        path = profile.system_ads
        if not isinstance(path, pathlib.Path) or path.suffix != ".ads":
            raise ValueError(
                f"Target {target.name}: profile '{profile.name}' has invalid"
                f" system_ads path '{path}'"
            )


def check_loader_names(
    target: AbstractTarget, profile: Profile, loaders: list[Loader]
) -> None:
    """Raise if two of one runtime's loaders share a name. They become the arms
    of one ``case`` statement and the values of one gpr type, so a name has to
    identify exactly one loader.

    Loaders are registered from ``contribute``, so this cannot join
    ``validate_target``: it takes a runtime, not a target. The engine calls it
    while specializing.

    :param target: the target the runtime is being built for.
    :param profile: the profile the runtime is being built for.
    :param loaders: what its components declared.
    :raises ValueError: if a name appears more than once.
    """
    names = [loader.name for loader in loaders]
    duplicated = sorted({name for name in names if names.count(name) > 1})
    if duplicated:
        raise ValueError(
            f"Target {target.name}/{profile.name}: loader(s) {duplicated}"
            f" declared more than once; a loader is one arm of the generated"
            f" case statement"
        )
