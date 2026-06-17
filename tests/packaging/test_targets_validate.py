#
# Copyright (C) 2025-2026, AdaCore
#

"""Validate every target exposed by bb_runtimes_targets_gen.ALL_TARGETS."""

import pytest
from bb_runtimes_targets_gen.targets.all_targets import ALL_TARGETS
from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)
from rts_prebuilder.engine.runtime_targetizer.target_checker import validate_target
from rts_prebuilder.engine.common.sources_db_processor import sources_db_proc


def test_all_targets_pass_validation():
    targets_to_validate: list[tuple[str, AbstractTarget]] = []
    for item in ALL_TARGETS:
        if isinstance(item, AbstractTarget):
            targets_to_validate.append((item.name, item))
        elif isinstance(item, AbstractTargetGenerator):
            for variant in item.generate_variants():
                targets_to_validate.append((variant, item.instantiate(variant)))

    assert targets_to_validate, "ALL_TARGETS is empty"

    failures: list[tuple[str, str]] = []
    for name, target in targets_to_validate:
        try:
            validate_target(target)
        except (AssertionError, ValueError) as e:
            failures.append((name, str(e)))

    if failures:
        report = "\n".join(f"  - {n}: {err}" for n, err in failures)
        pytest.fail(f"{len(failures)} target(s) failed validation:\n{report}")


def test_sources_db():
    raw = engine_interface.common.all_sources_listing
    assert raw, "sources database is empty"
    processed = sources_db_proc(raw)
    assert len(processed) == len(raw)
