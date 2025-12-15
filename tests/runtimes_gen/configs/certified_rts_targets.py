#
# Copyright (C) 2025-2026, AdaCore
#

"""Certified RTS targets configuration."""

# List of certified RTS targets
# Each entry contains:
# - name: Target identifier (used as cli_name)
# - top_base_profile: The highest base profile for this target (needed for assembly)
# - platforms: platfrom arg to use in assembly
# - cert_subdir: Certificate subdirectory path

CERTIFIED_RTS_TARGETS = [
    {
        "name": "aarch64-light",
        "top_base_profile": "light",
        "platforms": ["bb"],
        "cert_subdir": "auto-aarch64",
    },
    {
        "name": "aarch64-qnx-nvdrive",
        "top_base_profile": "light-tasking",
        "platforms": ["qnx"],
        "cert_subdir": "auto-nvdrive",
    },
    {
        "name": "aarch64-linux-nvdrive",
        "top_base_profile": "light-tasking",
        "platforms": ["linux"],
        "cert_subdir": "auto-nvdrive",
    },
    {
        "name": "x86_64-linux-nvdrive",
        "top_base_profile": "light-tasking",
        "platforms": ["linux"],
        "cert_subdir": "auto-nvdrive",
    },
    # {
    #     "name": "railway-aarch64-minimal",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "railway-aarch64",
    # },
    # {
    #     "name": "railway-x86_64-minimal",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "railway-aarch64",
    # },
    # {
    #     "name": "railway-arm-minimal",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "railway-arm",
    # },
    # {
    #     "name": "railway-x86-minimal",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "railway-arm",
    # },
    # {
    #     "name": "tms570lc",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "space-tms570",
    # },
    # {
    #     "name": "tms570lc_dcc",
    #     "top_base_profile": "light",
    #     "platforms": ["bb"],
    #     "cert_subdir": "space-tms570",
    # },
]
