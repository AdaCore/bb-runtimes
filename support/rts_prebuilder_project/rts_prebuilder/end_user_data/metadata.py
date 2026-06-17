#
# Copyright (C) 2025-2026, AdaCore
#
"""Metadata file management - handles paths and writing to .prebuild_metadata."""

from pathlib import Path

from rts_prebuilder.end_user_data.logger import get_logger

log = get_logger(__name__)

METADATA_DIR_NAME = ".prebuild_metadata"
_base_dir: Path | None = None


def set_metadata_base_dir(base_dir: Path) -> None:
    """Set base directory. Metadata will be in {base_dir}/.prebuild_metadata."""
    global _base_dir
    _base_dir = base_dir
    log.debug("Metadata base directory set to: %s", base_dir)


def get_metadata_base_dir() -> Path:
    """Get the .prebuild_metadata directory path."""
    if _base_dir is not None:
        return _base_dir / METADATA_DIR_NAME
    return Path.cwd() / METADATA_DIR_NAME


def get_metadata_dir(subdir: Path | None = None) -> Path:
    """Get metadata directory, optionally with subdirectory."""
    base = get_metadata_base_dir()
    if subdir:
        return base / subdir
    return base


def write_metadata(
    filename: Path,
    content: str,
    subdir: Path | None = None,
) -> Path:
    """Write content to metadata file. Creates directories as needed."""
    metadata_dir = get_metadata_dir(subdir)
    metadata_dir.mkdir(parents=True, exist_ok=True)

    file_path = metadata_dir / filename
    file_path.write_text(content)

    log.debug("Wrote metadata to %s (%d bytes)", file_path, len(content))
    return file_path
