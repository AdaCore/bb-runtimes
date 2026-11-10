#
# Copyright (C) 2025-2026, AdaCore
#

"""Validate every target exposed by bb_runtimes_targets_gen.TARGETS."""

import pytest
from bb_runtimes_targets_gen.targets import TARGETS
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)
from rts_prebuilder.engine.profile_specializer import runtime_loaders
from rts_prebuilder.engine.target_checker import validate_target


def test_all_targets_pass_validation():
    targets_to_validate = _targets(limit=None)

    assert targets_to_validate, "TARGETS is empty"

    failures: list[tuple[str, str]] = []
    for name, target in targets_to_validate:
        try:
            validate_target(target)
        except (AssertionError, ValueError) as e:
            failures.append((name, str(e)))

    if failures:
        report = "\n".join(f"  - {n}: {err}" for n, err in failures)
        pytest.fail(f"{len(failures)} target(s) failed validation:\n{report}")


#  A generator expands to a thousand near-identical variants. A few of each
#  cover what the family declares.
VARIANTS_PER_GENERATOR = 5


def test_every_runtime_offers_usable_loaders():
    """Loaders are registered from ``contribute``, so a clash only shows when a
    runtime asks for them. Sweeping here catches it without generating, and for
    the profiles a given run does not build.
    """
    failures: list[tuple[str, str]] = []
    for name, target in _targets(limit=VARIANTS_PER_GENERATOR):
        for profile in target.profiles:
            try:
                runtime_loaders(target, profile)
            except ValueError as e:
                failures.append((f"{name}/{profile.name}", str(e)))

    if failures:
        report = "\n".join(f"  - {n}: {err}" for n, err in failures)
        pytest.fail(f"{len(failures)} runtime(s) offer bad loaders:\n{report}")


def _targets(limit: int | None) -> list[tuple[str, AbstractTarget]]:
    """The targets to sweep, each with the name it is asked for by.

    :param limit: how many variants to take from each generator, all if None.
    """
    out: list[tuple[str, AbstractTarget]] = []
    for item in TARGETS:
        if isinstance(item, AbstractTarget):
            out.append((item.name, item))
        elif isinstance(item, AbstractTargetGenerator):
            for variant in _sample(list(item.generate_variants()), limit):
                out.append((variant, item.instantiate(variant)))
    return out


def _sample(variants: list[str], limit: int | None) -> list[str]:
    """At most ``limit`` variants, spread across the family.

    A generator emits from nested loops, so the first few names all sit in one
    corner of it: the first five of a thousand STM32F0 parts are all stm32f030.

    :param variants: every name the generator emits, in its own order.
    :param limit: how many to keep, all of them if None.
    :returns: the kept names.
    """
    if limit is None or len(variants) <= limit:
        return variants
    step = len(variants) / limit
    return [variants[int(index * step)] for index in range(limit)]
