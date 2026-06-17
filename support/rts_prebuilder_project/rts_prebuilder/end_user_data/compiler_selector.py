#
# Copyright (C) 2025-2026, AdaCore
#

from enum import Enum

# Compiler related globals

Compiler = Enum("Compiler", ["gnat", "gnat_llvm"])

_compiler: Compiler | None = None

DEFAULT_COMPILER = Compiler.gnat


def set_compiler(compiler: Compiler) -> None:
    """Set the compiler choice for this environment"""
    global _compiler
    if compiler not in Compiler:
        raise ValueError(f"Invalid compiler: {compiler}")
    _compiler = compiler


def using_llvm_compiler() -> bool:
    """Check if the current compiler is LLVM-based"""
    return _compiler == Compiler.gnat_llvm


def using_gcc_compiler() -> bool:
    """Check if the current compiler is GCC-based"""
    return _compiler == Compiler.gnat
