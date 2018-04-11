#
# Copyright (C) 2016-2018, AdaCore
#
# Generates the scenario variables values to bind the rts sources with the
# BSP to actually create a runtime project.

import sys


class RTSProfiles(object):
    """Defines the scenarii in the shared rts projects"""

    def __init__(self, config):
        # config is a bsp_sources.Target object that defines the properties
        # that are necessary to configure the runtime sources.
        self.config = config

    def zfp_scenarios(self, math_lib, profile='zfp'):
        """Returns the list of directories contained in a base ZFP runtime"""
        ret = {}
        ret['RTS_Profile'] = profile

        if self.config.has_fpu:
            ret['Has_FPU'] = 'yes'
        else:
            ret['Has_FPU'] = 'no'

        if self.config.has_libc(profile):
            ret['Has_libc'] = 'yes'
        else:
            ret['Has_libc'] = 'no'

        # By default, all targets support a Compare_And_Swap instruction
        ret['Has_Compare_And_Swap'] = 'yes'

        if math_lib:
            if self.config.has_single_precision_fpu:
                if self.config.has_double_precision_fpu:
                    # Full hardware
                    ret['Add_Math_Lib'] = 'hardfloat'
                else:
                    # Hardware only for SP.
                    ret['Add_Math_Lib'] = 'hardfloat_sp'
            else:
                if self.config.has_double_precision_fpu:
                    # Hardware only for DP.
                    ret['Add_Math_Lib'] = 'hardfloat_dp'
                else:
                    # No hardware support
                    ret['Add_Math_Lib'] = 'softfloat'
        else:
            ret['Add_Math_Lib'] = 'no'
        ret['Add_Complex_Type_Support'] = 'no'
        ret['Add_C_Integration'] = "no"
        ret['Add_Arith64'] = "no"
        ret['Add_Exponent_Int'] = "no"
        ret['Add_Exponent_LL_Int'] = "no"
        ret['Add_Exponent_Modular'] = "no"
        ret['Add_Exponent_LL_Float'] = "no"
        ret['Add_Image_Enum'] = "no"
        ret['Add_Image_Decimal'] = "no"
        ret['Add_Image_LL_Decimal'] = "no"
        ret['Add_Image_Float'] = "no"
        ret['Add_Image_Int'] = "no"
        ret['Add_Image_LL_Int'] = "no"
        ret['Add_Image_Based_Int'] = "no"
        ret['Add_Image_LL_Based_Int'] = "no"
        ret['Add_Image_Char'] = "no"
        ret['Add_Image_Wide_Char'] = "no"

        if self.config.use_semihosting_io:
            ret['Text_IO'] = 'semihosting'
        else:
            ret['Text_IO'] = 'serial'

        if not self.config.is_pikeos:
            if self.config.has_small_memory:
                ret['Memory_Profile'] = 'small'
            else:
                ret['Memory_Profile'] = 'large'

        return ret

    def sfp_scenarios(self, math_lib, profile='ravenscar-sfp'):
        """Returns the list of directories contained in a base SFP runtime"""
        ret = self.zfp_scenarios(math_lib, profile)
        ret['RTS_Profile'] = 'ravenscar-sfp'

        ret['Add_Image_Enum'] = 'yes'

        if self.config.target is not None:
            cpu = self.config.target.split('-')[0]

            if cpu in ('aarch64', ):
                ret['CPU_Family'] = 'aarch64'
            elif cpu in ('arm', ):
                ret['CPU_Family'] = 'arm'
            elif cpu.startswith('leon'):
                ret['CPU_Family'] = 'leon'
            elif cpu in ('powerpc', 'ppc'):
                ret['CPU_Family'] = 'powerpc'
            elif cpu in ('x86',):
                ret['CPU_Family'] = 'x86'
            else:
                print "Unexpected cpu %s" % cpu
                sys.exit(2)

        if not self.config.is_pikeos:
            # source installation for PikeOS do not consider those
            if self.config.has_timer_64:
                ret['Timer'] = 'timer64'
            else:
                ret['Timer'] = 'timer32'

        else:
            ret['Pikeos_Version'] = self.config.pikeos_version

        return ret

    def full_scenarios(self, math_lib):
        """Returns the list of directories contained in a base full runtime"""
        ret = self.sfp_scenarios(math_lib, 'ravenscar-full')

        # override the RTS value
        ret['RTS_Profile'] = 'ravenscar-full'
        ret['Add_Arith64'] = "yes"
        ret['Add_Complex_Type_Support'] = 'yes'
        ret['Add_Exponent_Int'] = "yes"
        ret['Add_Exponent_LL_Int'] = "yes"
        ret['Add_Exponent_Modular'] = "yes"
        ret['Add_Exponent_LL_Float'] = "yes"
        ret['Add_Image_Int'] = "yes"
        ret['Add_Image_LL_Int'] = "yes"
        ret['Add_Image_Based_Int'] = "yes"
        ret['Add_Image_LL_Based_Int'] = "yes"
        ret['Add_Image_Decimal'] = "yes"
        ret['Add_Image_LL_Decimal'] = "yes"
        ret['Add_Image_Float'] = "yes"
        ret['Add_Image_Char'] = "yes"
        ret['Add_Image_Wide_Char'] = "yes"

        if not self.config.is_pikeos:
            # PikeOS provides its own C library
            # ravenscar-full requires C memory operations, either via newlib
            # or via our own implementation in Ada
            ret['Add_C_Integration'] = "newlib"

        return ret
