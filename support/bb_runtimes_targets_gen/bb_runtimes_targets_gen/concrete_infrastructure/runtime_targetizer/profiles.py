#
# Copyright (C) 2025-2026, AdaCore
#

import sys

from typing import override

from rts_prebuilder.abstract_infrastructure import AbstractProfileToScenarioGenerator
from rts_prebuilder.base_types import (
    ScenariosConfigurationType,
    ValidBaseProfileType,
    ProfileNameType,
)
from rts_prebuilder.end_user_data.compiler_selector import using_llvm_compiler
from rts_prebuilder.end_user_data.logger import get_logger

log = get_logger(__name__)


class DefaultProfileToScenariosGenerator(AbstractProfileToScenarioGenerator):
    """
    Concrete implementation made for standard profiles (light, light-tasking,
    embedded, cert, none).
    This is just a default implementation which can be used as is or
    extended for custom targets.
    """

    @override
    def profile_to_scenarios(
        self, profile: ProfileNameType
    ) -> ScenariosConfigurationType:
        """
        Generates the scenario configuration based on the target
        and the requested profile.
        """

        base_profile = self.target.base_profile(profile)

        match base_profile:
            case "none":
                return self.no_profile_scenarios()
            case "light":
                return self.light_scenarios()
            case "light-tasking":
                return self.light_tasking_scenarios()
            case "embedded":
                return self.embedded_scenarios()
            case "cert":
                return self.cert_scenarios()

        raise Exception(f"Unknown profile: {base_profile}")

    def no_profile_scenarios(self) -> ScenariosConfigurationType:
        """Generates an empty scenario set with only the RTS_Profile set to none
        With an additional scenario relative to Certifiable_Packages
        based on the target configuration.
        """
        ret: ScenariosConfigurationType = {}

        ret["RTS_Profile"] = "none"

        # Using Certifiable_Packages means libgcc is not linked with. This may
        # be useful for custom profiles which do not want to link with gcc.
        if self.target.use_certifiable_packages:
            ret["Certifiable_Packages"] = "yes"
        else:
            ret["Certifiable_Packages"] = "no"

        return ret

    def _light_subset_scenarios(
        self, base_profile: ValidBaseProfileType = "light"
    ) -> ScenariosConfigurationType:
        """Generates the scenarios configuration used at least for the "light"
        runtime. This function can be reused for more complete profiles.

        :param base_profile: If the susbset is used for another profile,
                             it is mandatory to set this arg.
        """
        ret: ScenariosConfigurationType = {}
        ret["Compiler_Backend"] = "llvm" if using_llvm_compiler() else "gcc"

        ret["Add_Arith64"] = "yes"
        ret["Add_Case_Util"] = "yes"
        ret["Add_Exponent_Float"] = "yes"
        ret["Add_Exponent_Int"] = "yes"
        ret["Add_Exponent_LL_Int"] = "yes"
        ret["Add_Exponent_Modular"] = "yes"
        ret["Add_Float_Util"] = "yes"
        ret["Add_Pack"] = "yes"

        ret["Add_Image_Enum"] = "yes"
        ret["Add_Image_Int"] = "yes"
        ret["Add_Image_LL_Int"] = "yes"
        ret["Add_Image_Decimal"] = "yes"
        ret["Add_Image_LL_Decimal"] = "yes"
        ret["Add_Image_Fixed"] = "yes"
        ret["Add_Image_LL_Fixed"] = "yes"
        ret["Add_Image_Float"] = "yes"
        ret["Add_Image_Char"] = "yes"
        ret["Add_Image_Util"] = "yes"

        ret["Add_Value_Spec"] = "yes"
        ret["Add_Value_LL_Spec"] = "yes"
        ret["Add_Value_Bool"] = "yes"
        ret["Add_Value_Enum"] = "yes"
        ret["Add_Value_Decimal"] = "yes"
        ret["Add_Value_LL_Decimal"] = "yes"
        ret["Add_Value_Fixed"] = "yes"
        ret["Add_Value_LL_Fixed"] = "yes"
        ret["Add_Value_Float"] = "yes"
        ret["Add_Value_Int"] = "yes"
        ret["Add_Value_LL_Int"] = "yes"
        ret["Add_Value_Char"] = "yes"
        ret["Add_Value_Util"] = "yes"

        if self.target.is_64bit:
            ret["Add_Exponent_LLL_Int"] = "yes"

            ret["Add_Image_LLL_Int"] = "yes"
            ret["Add_Image_LLL_Decimal"] = "yes"
            ret["Add_Image_LLL_Fixed"] = "yes"

            ret["Add_Value_LLL_Spec"] = "yes"
            ret["Add_Value_LLL_Int"] = "yes"
            ret["Add_Value_LLL_Decimal"] = "yes"
            ret["Add_Value_LLL_Fixed"] = "yes"

            ret["Add_Pack64"] = "yes"

        # TODO:4 too many if branches, can be replaced by match statement (python 3.10)
        if self.target.target is not None:
            cpu = self.target.target.split("-")[0]

            if cpu in ("aarch64", "morello"):
                ret["CPU_Family"] = "aarch64"
                ret["Has_FMA"] = "yes" if self.target.has_fpu else "no"
            elif cpu in ("arm",):
                ret["CPU_Family"] = "arm"
                ret["Has_FMA"] = "no"
            elif cpu.startswith("leon"):
                ret["CPU_Family"] = "leon"
                ret["Has_FMA"] = "no"
            elif cpu in ("powerpc", "ppc"):
                ret["CPU_Family"] = "powerpc"
                ret["Has_FMA"] = "yes" if self.target.has_fpu else "no"
            elif cpu in ("powerpc64", "ppc64"):
                ret["CPU_Family"] = "powerpc64"
                ret["Has_FMA"] = "yes" if self.target.has_fpu else "no"
            elif cpu in ("x86",):
                ret["CPU_Family"] = "x86"
                ret["Has_FMA"] = "no"
            elif cpu in ("x86_64",):
                ret["CPU_Family"] = "x86_64"
                ret["Has_FMA"] = "no"
            elif cpu in ("riscv32", "r7"):
                ret["CPU_Family"] = "riscv32"
                ret["Has_FMA"] = "yes" if self.target.has_double_precision_fpu else "no"
            elif cpu in ("riscv64",):
                ret["CPU_Family"] = "riscv64"
                ret["Has_FMA"] = "yes" if self.target.has_double_precision_fpu else "no"
            else:
                log.error("Unexpected cpu %s", cpu)
                sys.exit(2)

        if self.target.has_fpu:
            ret["Has_FPU"] = "yes"
        else:
            ret["Has_FPU"] = "no"

        if self.target.has_command_line_arguments:
            ret["Add_Command_Line"] = "yes"
        else:
            ret["Add_Command_Line"] = "no"

        if self.target.has_libc(base_profile):
            ret["Has_libc"] = "yes"
        else:
            ret["Has_libc"] = "no"

        if self.target.has_cheri:
            ret["Has_CHERI"] = "yes"
        else:
            ret["Has_CHERI"] = "no"

        if self.target.use_certifiable_packages:
            ret["Certifiable_Packages"] = "yes"
        else:
            ret["Certifiable_Packages"] = "no"

        if self.target.has_single_precision_fpu:
            if self.target.has_double_precision_fpu:
                # Full hardware
                ret["Add_Math_Lib"] = "hardfloat"
            else:
                # Hardware only for SP.
                ret["Add_Math_Lib"] = "hardfloat_sp"
        else:
            if self.target.has_double_precision_fpu:
                # Hardware only for DP.
                ret["Add_Math_Lib"] = "hardfloat_dp"
            else:
                # No hardware support
                ret["Add_Math_Lib"] = "softfloat"

        if self.target.use_semihosting_io:
            ret["Text_IO"] = "semihosting"
        else:
            ret["Text_IO"] = "serial"

        if self.target.has_small_memory:
            ret["Memory_Profile"] = "small"
        elif self.target.has_huge_memory:
            ret["Memory_Profile"] = "huge"
        else:
            ret["Memory_Profile"] = "large"

        # 64-bit specific packages
        if self.target.is_64bit:
            ret["Add_Arith128"] = "yes"
            ret["Target_Word_Size"] = "64"
        else:
            ret["Target_Word_Size"] = "32"

        # C++ constructors/destructors package
        if self.target.is_os_target or self.target.has_cheri or using_llvm_compiler():
            # not for OSs (it has libc) or morello or LLVM (they do not support C++)
            ret["Add_Ctors_Dtors"] = "no"
        else:
            ret["Add_Ctors_Dtors"] = "yes"

        return ret

    def _light_tasking_subset_scenarios(
        self, base_profile: ValidBaseProfileType = "light-tasking"
    ) -> ScenariosConfigurationType:
        """Generates the scenarios configuration used at least for the "light-tasking"
        runtime. This function can be reused for more complete profiles.
        """

        ret = self._light_subset_scenarios(base_profile=base_profile)

        if self.target.has_timer_64:
            ret["Timer"] = "timer64"
        else:
            ret["Timer"] = "timer32"

        ret["Has_Compare_And_Swap"] = (
            "yes" if self.target.has_compare_and_swap else "no"
        )

        return ret

    def light_scenarios(self) -> ScenariosConfigurationType:
        """
        Generates the Scenario configuration for the light profile
        """
        return self._light_subset_scenarios() | {"RTS_Profile": "light"}

    def light_tasking_scenarios(self) -> ScenariosConfigurationType:
        """
        Generates the Scenario configuration for the light-tasking profile
        """
        return self._light_tasking_subset_scenarios() | {"RTS_Profile": "light-tasking"}

    def embedded_scenarios(self) -> ScenariosConfigurationType:
        """
        Generates the Scenario configuration for the embedded profile
        """
        ret = self._light_tasking_subset_scenarios("embedded") | {
            "RTS_Profile": "embedded"
        }

        ret["Add_Complex_Type_Support"] = "yes"
        ret["Add_Image_Wide_Char"] = "yes"
        ret["Add_Streams"] = "yes"
        ret["Add_Traceback"] = "yes"
        ret["Add_Value_Wide_Char"] = "yes"

        # We don't support certifiable components with Embedded since we
        # our libgcc replacement does not provide exception rts_prebuilder
        ret["Certifiable_Packages"] = "no"

        # use libc
        ret["Add_Ctors_Dtors"] = "no"

        if self.target.has_libc("embedded"):
            ret["Add_C_Integration"] = self.target.libc_implementation("embedded")

        return ret

    def cert_scenarios(self) -> ScenariosConfigurationType:
        """
        Generates the Scenario configuration for the cert profile
        """
        ret = self._light_subset_scenarios("cert") | {"RTS_Profile": "cert"}

        ret["Add_IO_Exceptions"] = "yes"

        ret["Add_Pack"] = "no"
        if self.target.is_64bit:
            ret["Add_Pack64"] = "no"

        return ret
