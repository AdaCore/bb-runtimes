#
# Copyright (C) 2025-2026, AdaCore
#

"""
Base class for target configuration
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Tuple, cast, final, override, Literal, TYPE_CHECKING

from rts_prebuilder.base_types import (
    VALID_BASE_PROFILES_SET,
    ProfileNameType,
    ValidBaseProfileType,
    PlatformIdType,
)

from .component_mixin import SourcesAndFlagsComponentMixin
from .runtime_config import RuntimeConfig
from .template_config_mixin import TemplateConfigListerMixin

if TYPE_CHECKING:
    from .abstract_profiles_generator import (
        AbstractProfileToScenarioGenerator,
    )


class AbstractTarget(ABC, SourcesAndFlagsComponentMixin, TemplateConfigListerMixin):
    """
    Gives information on the target to allow proper configuration of the
    runtime

    Child classes must implement all abstract properties.

    TODO:3 Make a pass on all non abstract properties and see if they can be
            made abstract if the default implementation/value is not generic enough.
    """

    def __init__(self) -> None:
        """
        Initialize
        """
        TemplateConfigListerMixin.__init__(self)
        SourcesAndFlagsComponentMixin.__init__(self)

    @property
    @abstractmethod
    def name(self) -> str:
        """Board's name, as used to name the runtime (e.g. light-<name>)
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
    @abstractmethod
    def system_ads(self) -> dict[str, str]:
        """
        A dictionary of runtime profiles and their associated system.ads

        The keys don't have to be one of VALID_BASE_PROFILES_SET,
        but the base_profile() method must map them to one of these.

        Example::

            {
                "special-runtimeX: "path/to/system_special_runtimeX.ads"
                "light": "path/to/system_light.ads",
            }

        """
        ...

    def base_profile(self, profile: ProfileNameType) -> ValidBaseProfileType:
        """
        Returns the base profile for a given profile.

        This is used to map custom profile names to one of VALID_BASE_PROFILES_SET.

        Example:
            base_profile("special-runtime-X") -> "light"
            base_profile("automotive-runtime-Y") -> "light"
        """

        if profile not in self.system_ads or profile not in VALID_BASE_PROFILES_SET:
            raise Exception(
                f"Target {self.name}: Override base_profile() to map '{profile}' to one of "
                f"VALID_BASE_PROFILES_SET and '{profile}' must be a key in system_ads"
            )

        # We can safely cast here since we checked the profile is in the set
        return cast(ValidBaseProfileType, profile)

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

    def runtime_name_generator(self, profile: ProfileNameType) -> str:
        """
        Returns the runtime name for the given base profile.

        This is the default implementation. It can be overridden by child classes
        to provide custom naming schemes. (or call legacy_runtime_name_generator())
        """

        return f"{profile}-{self.name}"

    @final
    def legacy_runtime_name_generator(self, profile: ProfileNameType) -> str:
        """
        Returns the legacy runtime name for the given base profile.

        This is always in the legacy format rts-<base_profile>
        """

        base_profile = self.base_profile(profile)
        return f"rts-{base_profile}"

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
        return self.target is None or "native" in self.target

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
    def has_double_precision_fpu(self) -> bool:
        """
        Whether the double precision floats are supported in FPU
        """
        raise Exception("not implemented")

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
        """True if the OS supports command line arguments"""
        return False

    @property
    def has_compare_and_swap(self) -> bool:
        """True if the hardware supports an atomic compare-and-swap function.

        The default is to return True here as (at least for now) only the
        LEON processor may not support CAS, all other having proper support or
        at the minimum proper emulation when they are uni-processor.

        On LEON target, some variants of the CPU may not support it, while gcc
        expects the support: this may thus generate invalid instructions.
        """
        return True

    @property
    def has_cplusplus_support(self) -> bool:
        """
        Support of C++ language in the runtime.
        By default, deduce from other properties.
        """
        if self.has_cheri:
            return False
        return True

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
        if base_profile == "embedded":
            return True
        else:
            return False

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
    def compiler_switches(self) -> Tuple[str, ...]:
        """Switches to be used when compiling. Common to Ada, C, ASM"""
        return ()

    @property
    def c_switches(self) -> Tuple[str, ...]:
        """Switches to be used when compiling C code."""
        return ()

    @property
    def readme_file(self) -> Path | None:
        """Path to the readme file"""
        return None

    @property
    def config_files(self) -> dict[str, str]:
        return {}

    @property
    @abstractmethod
    def profile_to_scenarios_generator(self) -> "AbstractProfileToScenarioGenerator":
        """
        Returns the class used to generate scenarios from profiles
        """

    def amend_rts(self, rts_profile: ProfileNameType, conf: RuntimeConfig) -> None:
        """
        This method is called after the default configuration of the runtime
        and right before the engine actually uses the configuration to
        generate the targetized runtime.
        To allow target-specific amendments to the configuration.

        .. note::
            This is not considered a clean pattern, use it only when a first-class
            configuration property cannot be used.
            Usually you can override self.profile_to_scenarios_generator
            to achieve most configuration changes.
            the other config properties properties
            such as config_files and build_flags can also just be inherited and
            overridden to provide target-specific configuration. (with calls of
            super() as needed).

            TODO:3 Apply this existing targets


        :param rts_profile: The profile name for which the runtime is being built
        :param conf: The runtime configuration to amend (See RuntimeConfig)
                     it has scenarios, build flags
        """
        ...

    def pre_build_step(self, obj_dir: Path) -> None:
        """Actions required before building the runtime"""
        ...

    @abstractmethod
    def dump_runtime_xml(self, rts_name: ProfileNameType, rts) -> str:  # type: ignore[no-untyped-def] # noqa: ANN001 E501
        """
        Generates the runtime xml for the target

        :param rts_name: TODO:5 to rename to profile name
        :param rts_filesholder: Used only to grab the scenarios configuration
                                pregenerated for the given profile.
                                TODO:5 must be changed to RuntimeConfig (subset) and remove the ignore type

        """
        ...
