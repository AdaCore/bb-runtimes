#
# Copyright (C) 2025-2026, AdaCore
#

# BSP support for x86_64
from pathlib import Path
from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.abstract_infrastructure import Loader
from rts_prebuilder.abstract_infrastructure import Profile
from rts_prebuilder.base_types import ValidBaseProfileType

from bb_runtimes_targets_gen.concrete_infrastructure import (
    SourcesAndFlagsComponent,
    DFBBTarget,
    embedded_profile,
    light_profile,
    light_tasking_profile,
)


class X8664Arch(SourcesAndFlagsComponent):
    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_gnat_sources(
            "x86_64/src/i-x86_64.adb",
            "x86_64/src/i-x86_64.ads",
            "x86_64/src/i-x86_64-ns16550.adb",
            "x86_64/src/i-x86_64-ns16550.ads",
            "x86_64/src/s-macres.adb",
            "x86_64/src/start.S",
        )
        # s-bbcppr.ads is listed under the Generic target as it will need
        # modifying if we want to support microarchitectures older than
        # Sandy Bridge since the current runtime uses XOPTSAVE in its
        # context switching routine.
        self.add_gnarl_sources("x86_64/src/vector_handlers.S")


class X8664Target(DFBBTarget):
    def __init__(self) -> None:
        super().__init__()
        self.add_dependency(X8664Arch())

    @property
    @override
    def has_huge_memory(self) -> bool:
        return True

    @property
    @override
    def has_timer_64(self) -> bool:
        return True

    @property
    @override
    def is_64bit(self) -> bool:
        return True

    @property
    @override
    def profiles(self) -> list[Profile]:
        return [
            light_profile(self, Path("system-xi-x86_64.ads")),
            light_tasking_profile(self, Path("system-xi-x86_64-sfp.ads")),
            embedded_profile(self, Path("system-xi-x86_64-full.ads")),
        ]


class X8664Generic(X8664Target):
    @property
    @override
    def target(self) -> str:
        return "x86_64-elf"

    @property
    @override
    def name(self) -> str:
        return "x86_64"

    @property
    @override
    def readme_file(self) -> Path | None:
        return Path("x86_64/generic/README")

    @property
    @override
    def default_loader(self) -> str | None:
        return "QEMU"

    @override
    def contribute(self, target: AbstractTarget, profile: Profile) -> None:
        self.add_app_switches(
            link_switches=("-Wl,-z,max-page-size=0x1000",),
        )

        self.add_gnat_sources(
            "shared/s-textio__com1.adb",
            "x86_64/src/lynx.S",
            "x86_64/src/multiboot.S",
            "x86_64/src/multiboot2.S",
        )
        self.add_gnarl_sources(
            "shared/a-intnam__x86_64.ads",
            "shared/s-bbcppr__new.ads",
            "shared/s-bbbosu__x86_64.adb",
            "shared/s-bbcppr__x86_64.adb",
            "shared/s-bbcpsp__x86_64.adb",
            "shared/s-bbcpsp__x86_64.ads",
            "shared/s-bbpara__x86_64.ads",
            "shared/s-bbsumu__generic.adb",
            "x86_64/src/i-x86_64-exception_handler.adb",
            "x86_64/src/i-x86_64-exception_handler.ads",
            "x86_64/src/i-x86_64-io_apic.adb",
            "x86_64/src/i-x86_64-io_apic.ads",
        )

        memory_map = Path("x86_64/generic/memory-map.ld")
        self.add_loaders(
            *(
                Loader(
                    name,
                    linker_scripts=(Path(f"x86_64/generic/{script}"),),
                    included_scripts=(memory_map,),
                )
                for name, script in (
                    ("QEMU", "common-QEMU.ld"),
                    ("LSA", "common-LSA.ld"),
                    ("MULTIBOOT", "common-MULTIBOOT.ld"),
                    ("MULTIBOOT2", "common-MULTIBOOT2.ld"),
                )
            )
        )

    @override
    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        # crti.o and crtn.o do not exist for the x86_64 bare-metal compiler.
        if base_profile == "embedded":
            return Path("x86_64/generic/link-zcx.spec")
        return super().link_spec_file(base_profile)


TARGETS = [X8664Generic()]
