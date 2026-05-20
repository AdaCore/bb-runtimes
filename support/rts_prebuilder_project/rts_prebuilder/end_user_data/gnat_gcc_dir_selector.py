#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import Literal

from .logger import get_logger

log = get_logger(__name__)

# GNAT/GCC directory related globals

DEFAULT_GCC_DIR: Path = Path.cwd() / "../gcc"  # Default GCC directory
DEFAULT_GNAT_DIR: Path = Path.cwd() / "../gnat"  # Default GNAT directory

# Type definition for repository names
_repo_name = Literal["gnat", "gcc"]

# Global variables to hold the paths
_gcc_dir: Path | None = None
_gnat_dir: Path | None = None

# Fixme: maybe should be relative to script dir ?


def check_path_exists(path: Path, repo_name: _repo_name) -> None:
    """Check if a given path exists, raise FileNotFoundError if not"""
    if not path.exists():
        raise FileNotFoundError(
            f"Given directory {path} for {repo_name} does not exist."
        )


def set_and_check(path: Path | None, default: Path, repo_name: _repo_name) -> Path:
    """Set and check a given path, using a default if None"""
    if path is None:
        if not default.resolve().exists():
            raise FileNotFoundError(
                f"No directory given for {repo_name}, and default {default} does not exist."
            )
        return default.resolve()

    check_path_exists(path, repo_name)
    return path.resolve()


def set_gccdir(gccdir: Path | None) -> None:
    """Set the GCC directory"""
    global _gcc_dir
    _gcc_dir = set_and_check(gccdir, DEFAULT_GCC_DIR, "gcc")


def get_gccdir() -> Path | None:
    """Get the GCC directory"""

    if _gcc_dir:
        check_path_exists(_gcc_dir, "gcc")
        return _gcc_dir

    return None


def set_gnatdir(gnatdir: Path | None) -> None:
    """Set the GNAT directory"""
    global _gnat_dir

    _gnat_dir = set_and_check(gnatdir, DEFAULT_GNAT_DIR, "gnat")


def get_gnatdir() -> Path | None:
    """Get the GNAT directory"""

    if _gnat_dir:
        check_path_exists(_gnat_dir, "gnat")
        return _gnat_dir
    return None
