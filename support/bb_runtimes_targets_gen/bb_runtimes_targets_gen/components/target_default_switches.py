#
# Copyright (C) 2026, AdaCore
#

"""Baseline runtime-build switches shared by every bb-runtimes target."""

from typing import override

from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    Profile,
    SourcesAndFlagsComponent,
)
from rts_prebuilder.end_user_data.compiler_selector import using_gcc_compiler


class TargetDefaultSwitchesComponent(SourcesAndFlagsComponent):
    """The runtime-build switches every target starts from.

    The ``Target`` base composes it into every target's always-on tree, which is
    what keeps these switches in the infrastructure: the engine names none.
    """

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_runtime_switches(
            debug_switches=("-g",),
        )
        self.add_runtime_switches(
            c_switches=("-DIN_RTS", "-Dinhibit_libc"),
        )

        # FIXME -DLIGHT_RUNTIME goes to every profile, embedded included. It
        # probably belongs behind profile.base_profile in ("light",
        # "light-tasking"), but no target has been checked against that yet.
        self.add_runtime_switches(
            c_switches=("-DLIGHT_RUNTIME",),
        )

        if target.supports_gc_sections:
            self.add_runtime_switches(
                common_switches=("-ffunction-sections", "-fdata-sections"),
            )

        if using_gcc_compiler():
            # GNAT-LLVM doesn't support -fcallgraph-info
            self.add_runtime_switches(
                common_switches=("-fcallgraph-info=su,da",),
            )
