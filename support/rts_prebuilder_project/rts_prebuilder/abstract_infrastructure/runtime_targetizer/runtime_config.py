#
# Copyright (C) 2025-2026, AdaCore
#

from dataclasses import dataclass

from rts_prebuilder.base_types import ScenariosConfigurationType


@dataclass()
class RuntimeConfig:
    """
    This class holds the configuration for a given runtime.

    This class is public be because Target can override some of its
    attributes in amend_rts.
    """

    rts_vars: ScenariosConfigurationType
    """
    The scenarios configuration variables for this runtime
    """

    config_files: dict[str, str]
    """
    Configuration files, with filenames with their content as values
    """

    build_flags: dict[str, list[str]]
    """
    Build flags for this runtime
    """

    def __init__(self) -> None:
        """Initialize the RuntimeConfig with empty/default values"""
        self.rts_vars = {}
        self.config_files = {}
        self.build_flags = {}
