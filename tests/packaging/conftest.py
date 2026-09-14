#
# Copyright (C) 2026, AdaCore
#

"""Reset process-wide engine state between tests.

The resolver cache, the compiler choice, and the CLI singleton are
module/class-level globals with no reset API. Left dirty, one test's setup
leaks into the next.
"""

import pytest

from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)


@pytest.fixture(autouse=True)
def _reset_engine_globals():
    yield

    import rts_prebuilder.engine.infrastructure_interface as ii

    if "_resolver" in vars(ii):
        del ii._resolver

    from rts_prebuilder.abstract_infrastructure.engine_interface import (
        EngineToInfrastructureSingleton,
    )

    EngineToInfrastructureSingleton.instance = None
    engine_interface.self_register()

    from rts_prebuilder.end_user_data import compiler_selector

    compiler_selector._compiler = None

    from rts_prebuilder.engine.cli import RuntimePrebuilderCLI

    RuntimePrebuilderCLI._cli_singleton = None
    RuntimePrebuilderCLI._targets = {}
    RuntimePrebuilderCLI._generators = []
