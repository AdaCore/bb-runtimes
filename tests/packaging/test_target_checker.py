#
# Copyright (C) 2026, AdaCore
#

"""Exercise target_checker.validate_target's runtime-only checks.

Real TARGETS all pass validation by construction, so each failure branch
needs a purpose-built invalid target instead.
"""

from pathlib import Path
from typing import override

import pytest
from rts_prebuilder.abstract_infrastructure import AbstractTarget, Profile
from rts_prebuilder.engine.target_checker import validate_target


def _profile(
    name: str = "light",
    base_profile: str = "light",
    system_ads: str = "system_light.ads",
) -> Profile:
    return Profile(
        name=name,
        base_profile=base_profile,  # type: ignore[arg-type]
        system_ads=Path(system_ads),
    )


class _FakeTarget(AbstractTarget):
    def __init__(self, *, profiles=None, readme_file=None):
        super().__init__()
        self._profiles = [_profile()] if profiles is None else profiles
        self._readme_file = readme_file

    @property
    @override
    def name(self):
        return "faketarget"

    @property
    @override
    def platform(self):
        return "bb"

    @property
    @override
    def has_double_precision_fpu(self):
        return False

    @property
    @override
    def profiles(self):
        return self._profiles

    @property
    @override
    def target(self):
        return "faketarget-elf"

    @property
    @override
    def readme_file(self):
        return self._readme_file


def test_valid_target_passes():
    validate_target(_FakeTarget())


def test_readme_file_wrong_type_raises_type_error():
    target = _FakeTarget(readme_file="not_a_path")
    with pytest.raises(TypeError):
        validate_target(target)


def test_non_component_dependency_raises_type_error():
    """add_dependency validates at the call site, so the bad value never makes
    it into a tree for validate_target to find."""
    target = _FakeTarget()
    with pytest.raises(TypeError, match="must be an instance"):
        target.add_dependency("not a component")  # type: ignore[arg-type]


def test_empty_profiles_raises_value_error():
    target = _FakeTarget(profiles=[])
    with pytest.raises(ValueError, match="at least one profile"):
        validate_target(target)


def test_duplicate_profile_name_raises_value_error():
    target = _FakeTarget(profiles=[_profile(), _profile()])
    with pytest.raises(ValueError, match="duplicate profile name"):
        validate_target(target)


def test_system_ads_path_not_ending_in_ads_raises_value_error():
    target = _FakeTarget(profiles=[_profile(system_ads="system_light.txt")])
    with pytest.raises(ValueError, match="invalid system_ads path"):
        validate_target(target)


def test_system_ads_empty_path_raises_value_error():
    target = _FakeTarget(profiles=[_profile(system_ads="")])
    with pytest.raises(ValueError, match="invalid system_ads path"):
        validate_target(target)


def test_invalid_base_profile_raises_value_error():
    target = _FakeTarget(
        profiles=[
            _profile(
                name="my-profile",
                base_profile="not-a-valid-base-profile",
                system_ads="system_x.ads",
            )
        ]
    )
    with pytest.raises(ValueError, match="invalid base_profile"):
        validate_target(target)
