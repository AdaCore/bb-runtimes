#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractProfileToScenarioGenerator,
)
from .profiles import DefaultProfileToScenariosGenerator

from .def_gpr_xml_gen import default_runtime_xml_generator
from rts_prebuilder.base_types import PlatformIdType


class Target(AbstractTarget):
    """
    Mainly same things as AbstractTarget but with some default definitions
    """

    def __init__(self) -> None:
        super().__init__()
        self.add_build_scripts(self.build_script, self.prebuild_script)

    @property
    def build_script(self) -> Path:
        """Build script installed as the runtime's build.py (runs gprbuild)."""
        return Path("shared/default_build.py")

    @property
    def prebuild_script(self) -> Path:
        """Pre-build script installed as the runtime's pre_build.py. Defaults to
        a no-op; override to run a step before gprbuild.
        """
        return Path("shared/empty_prebuild.py")

    @override
    @property
    def profile_to_scenarios_generator(
        self,
    ) -> AbstractProfileToScenarioGenerator:
        return DefaultProfileToScenariosGenerator(self)

    @override
    def dump_runtime_xml(self, rts_name, rts) -> str:  # type: ignore[no-untyped-def] # noqa: ANN001 E501

        # TODO:3 remove type ignore when we change the target abstract method to
        # take ScenariosConfig config directly, we don't want to leak ScenariosConfig here
        return default_runtime_xml_generator(self, rts_name, rts.rts_vars)


class DFBBTarget(Target):
    """BB target with single and double FPU"""

    @property
    @override
    def platform(self) -> PlatformIdType:
        return "bb"

    @property
    @override
    def has_single_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_double_precision_fpu(self) -> bool:
        return True

    @property
    @override
    def has_timer_64(self) -> bool:
        return False
