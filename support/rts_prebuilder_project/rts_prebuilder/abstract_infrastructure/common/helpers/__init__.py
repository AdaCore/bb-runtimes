#
# Copyright (C) 2025-2026, AdaCore
#

"""
This module contains minimal functions useful for developpers extending the infrastructure,
and for the engine.
The functions should be pure (no side effects) and stateless.
Any core logic should not be here. shall be in engine instead.

If you need to add logic, remember that: infrastructure is there to capture user data,
not to process it. Processing is done in engine.

WARNING: No import from infrastructure engine is allowed here.
"""
