#
# Copyright (C) 2025-2026, AdaCore
#

"""
Base class for target configuration
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import final, override, Literal

from rts_prebuilder.base_types import (
    ValidBaseProfileType,
    PlatformIdType,
)

from rts_prebuilder.end_user_data.logger import get_logger

from .component import SourcesAndFlagsComponent
from .runtime_profile import Profile
from .template_config_mixin import TemplateConfigListerMixin

log = get_logger(__name__)


class AbstractTarget(SourcesAndFlagsComponent, ABC, TemplateConfigListerMixin):
    """
    One target the prebuilder generates runtimes for.

    A target is whatever the runtimes are built for. It can be a board
    (``HiFive1``), a SoC (``PolarFireSOC``), a bare instruction-set variant
    (``RV32IMAC``), or an OS on an architecture (``Aarch64QNX``). Each one is a
    subclass of this class, and that subclass is the only target-specific code
    in the tree. Everything else is shared.

    A subclass holds data, not logic. Its properties answer questions about the
    target: CPU, memory, interrupts, link configurations. They also list the
    runtimes it offers, as ``Profile`` values. They give the answer and stop
    there. The code acting on those answers lives in the engine and in the
    shared components. A property that computes instead of declaring is a sign
    its logic belongs in one of those.

    From that description the prebuilder generates one runtime per profile. It
    walks the target and the profile's components, collects the source files
    and switches each one contributes, and writes them out. The target sits at
    the root of that tree and is resolved whichever profile is being generated.
    The profile adds its own ``components`` on top.

    Being itself a component, a target declares two kinds of state, each in one
    place:

    * structure, in ``__init__``: the shared-source components it is built
      from, via ``self.add_dependency`` and friends.
    * entries, in ``contribute``: its own sources and switches, via
      ``self.add_*``.

    Every node in the tree is queried with the target, so inside the target's
    own ``contribute`` ``self`` and ``target`` are the same object. Register on
    ``self``, and read the properties below off ``target``: that spelling keeps
    working if the code later moves into a shared component.

    Child classes must implement all abstract properties.
    """

    def __init__(self) -> None:
        """
        Initialize
        """
        log.debug("Instantiating target object '%s'...", type(self).__name__)
        SourcesAndFlagsComponent.__init__(self)
        TemplateConfigListerMixin.__init__(self)

    @property
    def build_script(self) -> Path | None:
        """Source of the ``build.py`` installed at the runtime root, which the
        end user runs to rebuild the runtime. ``None`` installs neither this
        script nor ``prepare_build_script``."""
        return None

    @property
    def prepare_build_script(self) -> Path | None:
        """Source of the ``prepare_build.py`` that ``build.py`` calls first, for a
        step gprbuild cannot do. Goes together with ``build_script``."""
        return None

    @property
    def default_loader(self) -> str | None:
        """The loader the generated project selects when the end user passes no
        ``-XLOADER``. ``None`` takes the first one offered. That means the
        target's own first declaration, then its dependencies depth first, then
        the profile's components. Name one here as soon as more than one node
        declares a loader, so the default does not follow the tree shape."""
        return None

    @property
    @abstractmethod
    def name(self) -> str:
        """Target's name, as used to name the runtime (e.g. light-<name>)
        TODO:5 Maybe rename to name_in_runtime ?
        """
        ...

    @final
    @override
    def __str__(self) -> str:
        """String representation of the target is its name"""
        return self.name

    @property
    def cli_name(self) -> str:
        """Optional name used to select the target from the prebuilder CLI
        default to self.name if not set.
        """
        return self.name

    @property
    @abstractmethod
    def platform(self) -> PlatformIdType:
        """
        The platform identifier necessary to assemble the sources for this
        target. As declared in VALID_PLATFORM_IDS_SET.
        """
        ...

    @property
    def platform_variant(self) -> str | None:
        """Which variant of ``platform`` this target is, when one platform ships
        several that install different sources: the PikeOS version, the VxWorks
        kernel-vs-RTP mode. Only the platform's own components read it, so its
        values are that platform's business. ``None`` when the platform has a
        single variant.
        """
        return None

    @property
    @abstractmethod
    def profiles(self) -> list[Profile]:
        """
        The runtimes this target offers, one ``Profile`` per runtime.

        Each ``Profile`` is pure data: its (commercial) name, the base profile
        it maps to, its system.ads and the components that make it up. Profile
        names must be unique, because they drive the runtime fan-out. Build
        them with
        the profile factories (``light_profile``, …) or a bespoke ``Profile``
        for a custom runtime.
        """
        ...

    @property
    @abstractmethod
    def target(self) -> str:
        """Target name, as expected by gprbuild"""
        ...

    @property
    def is_64bit(self) -> bool:
        """Set to True on 64-bit targets"""
        return False

    @property
    def has_timer_64(self) -> bool:
        """
        True if the hardware provides a 64-bit timer. Else 32-bit timer is
        assumed.
        """
        return False

    def runtime_name_generator(self, profile: Profile) -> str:
        """
        Returns the runtime name for the given profile.

        This is the default implementation. It can be overridden by child classes
        to provide custom naming schemes. (or call legacy_runtime_name_generator())
        """

        return f"{profile.name}-{self.name}"

    @final
    def legacy_runtime_name_generator(self, profile: Profile) -> str:
        """
        Returns the legacy runtime name for the given profile.

        This is always in the legacy format rts-<base_profile>
        """

        return f"rts-{profile.base_profile}"

    @property
    def is_os_target(self) -> bool:
        """
        Whether the target is an operating system

        By default we assume we are targeting a bare-metal system.
        """

        return False

    @property
    @final
    def is_native(self) -> bool:
        return "native" in self.target

    @property
    def has_fpu(self) -> bool:
        """
        Whether the hardware provides a FPU.

        By default, set to True on PikeOS, or if has_*_precision_fpu is set.
        """
        return self.has_single_precision_fpu or self.has_double_precision_fpu

    @property
    def has_single_precision_fpu(self) -> bool:
        """
        Whether the single precision floats are supported in FPU
        """
        return self.has_double_precision_fpu

    @property
    @abstractmethod
    def has_double_precision_fpu(self) -> bool:
        """
        Whether the double precision floats are supported in FPU
        """
        ...

    @property
    def has_small_memory(self) -> bool:
        """
        Set to True on targets with limited RAM
        """
        return False

    @property
    def has_huge_memory(self) -> bool:
        """
        Set to True on targets with lots (> 512MB) of RAM
        """
        return False

    @property
    def use_semihosting_io(self) -> bool:
        """
        Whether to use a serial text io or semihosting
        """
        return False

    @property
    def has_command_line_arguments(self) -> bool:
        """True if this target's ``platform`` is an OS that passes command line
        arguments to the program. False on ``bb``, which has no such thing."""
        return False

    @property
    def supports_gc_sections(self) -> bool:
        """Whether the runtime may be built with ``-ffunction-sections`` /
        ``-fdata-sections``. The default is True. Set it False where the linker
        script cannot cope with the resulting sections, as on PikeOS 3."""
        return True

    @property
    def uses_eh_frame_unwinding(self) -> bool:
        """Whether embedded exception propagation relies on DWARF ``.eh_frame``
        instead of a target-native table such as ARM ``.ARM.exidx``. Relying on
        it needs ``-u _Unwind_Find_FDE`` and ``-Wl,--eh-frame-hdr`` in the app
        link. The default is False. Set it True on targets that unwind via
        ``.eh_frame``, such as Morello and powerpc BookE."""
        return False

    @property
    def has_compare_and_swap(self) -> bool:
        """True if the hardware supports an atomic compare-and-swap function.

        The default is True. For now, LEON is the only processor that may lack
        CAS. Every other one supports it, or emulates it properly when it is
        uni-processor.

        Some LEON CPU variants do not support it, but gcc expects them to. That
        can generate invalid instructions.
        """
        return True

    @property
    def has_cplusplus_support(self) -> bool:
        """
        Support of C++ language in the runtime.
        By default, deduce from other properties.
        """
        return not self.has_cheri

    @property
    def has_cheri(self) -> bool:
        """
        True if the hardware supports CHERI instructions

        Default to False
        """
        return False

    def has_libc(self, base_profile: ValidBaseProfileType) -> bool:
        """
        Whether libc is available and used on the target

        Default implementation assumes that only the Embedded profile
        provides a libc (newlib). While other such as "light" / "light-tasking"
        do not provide libc.
        """
        return base_profile == "embedded"

    def libc_implementation(
        self, base_profile: ValidBaseProfileType
    ) -> Literal["newlib", "adalib"]:
        """
        Returns the libc implementation used on the target for the given
        base profile.
        It can be either "newlib" or "adalib".
        """

        if not self.has_libc(base_profile):
            raise RuntimeWarning(
                "libc_implementation() called for a target without libc"
            )

        return "newlib"

    @property
    def use_certifiable_packages(self) -> bool:
        """
        True if the Light and Light-Tasking runtimes are to use certifiable
        runtime components.
        In practice, most packages in these runtimes are certifiable with the
        notable exception of libgcc. When true, our Ada implementation
        of libgcc is used.
        """
        return False

    @property
    def readme_file(self) -> Path | None:
        """Path to the readme file"""
        return None

    def link_spec_file(self, base_profile: ValidBaseProfileType) -> Path | None:
        """Linker spec file installed at the runtime root for this base profile,
        under its own name. ``None`` (the default) installs none.

        :param base_profile: the base profile the runtime is being built for.
        """
        return None
