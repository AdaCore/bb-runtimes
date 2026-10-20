#
# Copyright (C) 2026, AdaCore
#

"""Gnarl runtime components."""

from .bareboard import GnarlBareboard
from .common import GnarlCommon, GnarlCommonNoCheri, GnarlCommonCheri
from .full import GnarlSfp, GnarlFull, GnarlFullExtended
from .linux import GnarlLinux
from .lynx import GnarlLynx
from .per_cpu import GnarlAarch64, GnarlArm, GnarlPpc, GnarlX86, GnarlX8664
from .pikeos import GnarlPikeos
from .qnx import GnarlQnx
from .spinlock import GnarlSpinlockGcc, GnarlSpinlockLeon
from .timer import GnarlTimer32, GnarlTimer64
from .vx7r2cert import GnarlVx7r2Cert

__all__ = [
    "GnarlAarch64",
    "GnarlArm",
    "GnarlBareboard",
    "GnarlCommon",
    "GnarlCommonCheri",
    "GnarlCommonNoCheri",
    "GnarlFull",
    "GnarlFullExtended",
    "GnarlLinux",
    "GnarlLynx",
    "GnarlPikeos",
    "GnarlPpc",
    "GnarlQnx",
    "GnarlSfp",
    "GnarlSpinlockGcc",
    "GnarlSpinlockLeon",
    "GnarlTimer32",
    "GnarlTimer64",
    "GnarlVx7r2Cert",
    "GnarlX86",
    "GnarlX8664",
]
