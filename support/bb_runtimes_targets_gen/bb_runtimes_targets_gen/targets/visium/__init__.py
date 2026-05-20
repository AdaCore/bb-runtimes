#
# Copyright (C) 2025-2026, AdaCore
#

from pathlib import Path

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
)
from rts_prebuilder.engine import resolve_and_read_file
from bb_runtimes_targets_gen.concrete_infrastructure import DFBBTarget


class Visium(DFBBTarget):
    @property
    def name(self):
        return "mcm"

    @property
    def target(self):
        return "visium-elf"

    def has_libc(self, base_profile):
        return True

    @property
    def has_small_memory(self) -> bool:
        return True

    def __init__(self):
        super(Visium, self).__init__()
        self.add_gnat_sources("shared/s-macres__libc.adb", "shared/s-textio__stdio.adb")

    def dump_runtime_xml(self, rts_name, rts):
        return resolve_and_read_file(Path("visium/mcm/runtime.xml"))

    def amend_rts(self, rts_profile, conf):
        conf.build_flags["common_flags"] += ["-muser-mode"]

    @property
    def system_ads(self):
        return {"light": "system-xi-visium.ads"}


# TODO:2 Visium/MCM target is not buildable.
# (it gave "Unexpected cpu visium" error), so don't expose it as a target
# TARGETS = [Visium()]
TARGETS: list[AbstractTarget | AbstractTargetGenerator] = []
