#
# Copyright (C) 2025-2026, AdaCore
#

"""Smoke test: confirm both wheels installed and importable."""

import importlib


def test_import_rts_prebuilder():
    importlib.import_module("rts_prebuilder")
    importlib.import_module("rts_prebuilder.abstract_infrastructure")
    importlib.import_module("rts_prebuilder.engine")


def test_import_bb_runtimes_targets_gen():
    importlib.import_module("bb_runtimes_targets_gen")
    importlib.import_module("bb_runtimes_targets_gen.targets")
    importlib.import_module(
        "bb_runtimes_targets_gen.concrete_infrastructure.engine_interface"
    )
