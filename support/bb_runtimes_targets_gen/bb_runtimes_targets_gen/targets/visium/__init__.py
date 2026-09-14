#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
    Profile,
)
from rts_prebuilder.base_types import ValidBaseProfileType
from bb_runtimes_targets_gen.concrete_infrastructure import (
    DFBBTarget,
    RuntimeXmlSource,
    light_profile,
)


class Visium(DFBBTarget):
    @property
    @override
    def name(self) -> str:
        return "mcm"

    @property
    @override
    def target(self) -> str:
        return "visium-elf"

    @override
    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        return True

    @property
    @override
    def has_small_memory(self) -> bool:
        return True

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources("shared/s-macres__libc.adb", "shared/s-textio__stdio.adb")
        self.add_runtime_switches(
            common_switches=("-muser-mode",),
        )

    @property
    @override
    def runtime_xml_source(self) -> RuntimeXmlSource | None:
        return RuntimeXmlSource(Path("visium/mcm/runtime.xml"))

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [light_profile(self, Path("system-xi-visium.ads"))]


# TODO:2 Visium/MCM target is not buildable.
# (it gave "Unexpected cpu visium" error), so don't expose it as a target
# TARGETS = [Visium()]
TARGETS: list[AbstractTarget | AbstractTargetGenerator] = []
