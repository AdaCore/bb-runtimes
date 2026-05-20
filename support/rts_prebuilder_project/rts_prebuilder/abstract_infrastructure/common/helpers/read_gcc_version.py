#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path


def read_gcc_version(gccdir: Path) -> str:
    """
    Init gcc version from gcc/BASE-VER file
    """
    gcc_version = ""

    base_ver = gccdir / "gcc" / "BASE-VER"
    with open(base_ver, "r", encoding="utf-8") as fp:
        for line in fp:
            line = line.strip()
            if len(line) > 0:
                gcc_version = line
                break

    return gcc_version
