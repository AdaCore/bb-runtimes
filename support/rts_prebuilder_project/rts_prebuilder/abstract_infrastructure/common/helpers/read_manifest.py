#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from rts_prebuilder.end_user_data.logger import get_logger

log = get_logger(__name__)


def read_manifest(gnat_dir: Path | None) -> list[str]:
    """Load manifest file from toolchain directory"""
    _manifest: list[str] = []

    if not gnat_dir or not gnat_dir.exists():
        return _manifest

    # Try both MANIFEST.GNAT (original gnat location) and MANIFEST files
    manifest_files = [gnat_dir / "MANIFEST.GNAT", gnat_dir / "MANIFEST"]

    for manifest_file in manifest_files:
        log.debug("Checking for manifest file: %s", manifest_file)

        if manifest_file.exists():
            log.debug("Loading manifest file: %s", manifest_file)
            _manifest = []
            with open(manifest_file, "r", encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith("--"):
                        _manifest.append(line)
            break

    if len(_manifest) == 0:
        raise FileNotFoundError(f"Error: No MANIFEST file found in gnat dir {gnat_dir}")

    return _manifest
