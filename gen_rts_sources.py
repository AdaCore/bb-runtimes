#!/usr/bin/env python3
#
# Copyright (C) 2025-2026, AdaCore
#

"""Simple wrapper script for generating RTS sources using RuntimeAssembler."""

import sys
from pathlib import Path

from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface as interface,
)
from rts_prebuilder.engine import RuntimeAssemblerCLI

_repo_root = Path(__file__).resolve().parent
interface.common.path_resolver_instance.add_search_paths(
    _repo_root / "src",
    _repo_root / "src" / "datafiles",
)

interface.self_register()

if __name__ == "__main__":
    parser = RuntimeAssemblerCLI.create_parser(
        prog="gen_rts_sources", description="Generate RTS (Runtime System) sources"
    )

    args = parser.parse_args()

    sys.exit(RuntimeAssemblerCLI.run_runtime_assembler(args))
