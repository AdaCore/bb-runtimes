#
# Copyright (C) 2025-2026, AdaCore
#

"""
Entry point for the runtime targetizer.
This file is the same for all target packages (including the root
targets/ helper package).
all they need to do is to export TARGETS from their __init__.py.
Then adding a __main__.py symlink to this file will provide a cli.
"""

from rts_prebuilder.engine import RuntimeTargetizerCLI

# Import TARGETS from the current package
from . import TARGETS

from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)

engine_interface.self_register()


def main() -> None:
    """Entry point for the runtime targetizer."""
    RuntimeTargetizerCLI.register_targets(*TARGETS).run()


if __name__ == "__main__":
    main()
