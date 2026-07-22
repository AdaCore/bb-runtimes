#
# Copyright (C) 2025-2026, AdaCore
#
"""Metadata file management - handles paths and writing to .prebuild_metadata."""

import shutil
import tempfile
from pathlib import Path

from rts_prebuilder.end_user_data.logger import get_logger

log = get_logger(__name__)

METADATA_DIR_NAME = ".prebuild_metadata"
_base_dir: Path | None = None
_base_cleaned = False
"""Whether the metadata dir has been wiped once this run (see _ensure_clean_base)."""

_default_base_dir: Path | None = None


def set_metadata_base_dir(base_dir: Path) -> None:
    """Set base directory. Metadata will be in {base_dir}/.prebuild_metadata."""
    global _base_dir, _base_cleaned
    _base_dir = base_dir
    _base_cleaned = False
    log.debug("Metadata base directory set to: %s", base_dir)


def _ensure_clean_base() -> None:
    """Wipe the metadata dir once per run, before the first write, so entries
    from a previous run are not reused/mixed in."""
    global _base_cleaned
    if _base_cleaned:
        return
    base = get_metadata_base_dir()
    if base.exists() and any(base.iterdir()):
        shutil.rmtree(base)
        log.info("cleaned stale prebuild metadata at %s", base)
    _base_cleaned = True


def _get_default_base_dir() -> Path:
    """Private temp directory for this process, made on first use.

    A fixed name under the system temp dir would be shared by every concurrent
    run, and by every user of the machine. The per-run wipe would then delete
    metadata another run is still writing.
    """
    global _default_base_dir
    if _default_base_dir is None:
        _default_base_dir = Path(tempfile.mkdtemp(prefix="prebuild_metadata_"))
        log.info(
            "prebuild metadata written under %s (no base dir set; call "
            "set_metadata_base_dir to relocate)",
            _default_base_dir,
        )
    return _default_base_dir


def get_metadata_base_dir() -> Path:
    """Get the .prebuild_metadata directory path."""
    if _base_dir is not None:
        return _base_dir / METADATA_DIR_NAME
    return _get_default_base_dir()


def get_metadata_dir(subdir: Path | None = None) -> Path:
    """Get metadata directory, optionally with subdirectory."""
    _ensure_clean_base()
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
