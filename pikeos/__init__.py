# BSP support for PowerPC/e500v2
from support import readfile
from support.bsp_sources.target import Target


class PikeOS(Target):
    @property
    def has_command_line_arguments(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def pikeos_version(self):
        raise Exception("not implemented")

    @property
    def pikeos_target(self):
        raise Exception("not implemented")

    def has_libc(self, profile):
        # PikeOS is considered Bare Metal, and we don't provide newlib on
        # this target
        return False

    def __init__(self):
        super(PikeOS, self).__init__()
        self.add_linker_script('pikeos/memory.ld')
        self.add_gnat_sources(
            'src/s-textio__pikeos.adb',
            'src/s-macres__native.adb')
        self.add_gnarl_source('src/a-intnam__dummy.ads')

    def dump_runtime_xml(self, rts_name, rts):
        cnt = readfile('pikeos/runtime.xml')
        if self.pikeos_version == 'pikeos3':
            cnt = cnt.replace('@version@', 'pikeos-3.4')
        else:
            cnt = cnt.replace('@version@', 'pikeos-4.1')
        cnt = cnt.replace('@target@', self.pikeos_target)

        return cnt

    def amend_rts(self, rts_profile, conf):
        super(PikeOS, self).amend_rts(rts_profile, conf)
        if rts_profile == 'embedded':
            # Register ZCX frames (for pikeos-cert-app.c)
            conf.build_flags['c_flags'] += ['-DUSE_ZCX']
        if self.pikeos_version == 'pikeos3':
            # Don't use function/data sections, not supported by linker script
            conf.build_flags['common_flags'] = \
                filter(lambda x: x not in ['-ffunction-sections',
                                           '-fdata-sections'],
                       self.build_flags['common_flags'])


class ArmPikeOS(PikeOS):
    @property
    def name(self):
        return 'arm-pikeos'

    @property
    def target(self):
        return 'arm-sysgo-pikeos'

    @property
    def pikeos_version(self):
        return 'pikeos4'

    @property
    def pikeos_target(self):
        return 'arm/v7hf'

    @property
    def system_ads(self):
        return {
            'light': 'system-pikeos-arm.ads',
            'light-tasking': 'system-pikeos-arm-ravenscar-sfp.ads',
            'embedded': 'system-pikeos-arm-embedded.ads'
        }

    def __init__(self):
        super(ArmPikeOS, self).__init__()
        self.add_gnat_source('pikeos/pikeos-cert-app.c')
        self.add_gnarl_source('pikeos/adaint-pikeos.c')
        self.add_linker_script('pikeos/arm-app.ld')


class ArmPikeOS42(PikeOS):
    @property
    def name(self):
        return 'arm-pikeos4.2'

    @property
    def target(self):
        return 'arm-sysgo-pikeos4'

    @property
    def pikeos_version(self):
        return 'pikeos4.2'

    @property
    def pikeos_target(self):
        return 'arm/v7hf'

    @property
    def system_ads(self):
        return {
            'light': 'system-pikeos42-arm.ads',
            'light-tasking': 'system-pikeos42-arm-ravenscar-sfp.ads',
            'embedded': 'system-pikeos42-arm-embedded.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('pikeos/runtime_p4ext.xml')

    def __init__(self):
        super(ArmPikeOS42, self).__init__()
        self.add_gnat_source('pikeos/p4ext-cert-app.c')
        self.add_gnarl_source('pikeos/adaint-pikeos-p4ext.c')
        self.add_linker_script('pikeos/arm-app.ld')


class AArch64PikeOS5(PikeOS):
    @property
    def name(self):
        return 'aarch64-pikeos5'

    @property
    def target(self):
        return 'aarch64-sysgo-pikeos5'

    @property
    def pikeos_version(self):
        return 'pikeos5'

    @property
    def pikeos_target(self):
        return 'arm/v8hf'

    @property
    def system_ads(self):
        return {
            'light': 'system-pikeos5-aarch64.ads',
            'light-tasking': 'system-pikeos5-aarch64-ravenscar-sfp.ads',
            'embedded': 'system-pikeos5-aarch64-embedded.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('pikeos/runtime_p4ext.xml')

    def __init__(self):
        super(AArch64PikeOS5, self).__init__()
        self.add_gnat_source('pikeos/p4ext-cert-app.c')
        self.add_gnarl_source('pikeos/adaint-pikeos-p4ext.c')
        self.add_linker_script('pikeos/aarch64-pikeos5.ld')


class ArmPikeOS5(PikeOS):
    @property
    def name(self):
        return 'arm-pikeos5'

    @property
    def target(self):
        return 'arm-sysgo-pikeos5'

    @property
    def pikeos_version(self):
        return 'pikeos5'

    @property
    def pikeos_target(self):
        return 'arm/v7hf'

    @property
    def system_ads(self):
        return {
            'light': 'system-pikeos5-arm.ads',
            'light-tasking': 'system-pikeos5-arm-ravenscar-sfp.ads',
            'embedded': 'system-pikeos5-arm-embedded.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('pikeos/runtime_p4ext.xml')

    def __init__(self):
        super(ArmPikeOS5, self).__init__()
        self.add_gnat_source('pikeos/p4ext-cert-app.c')
        self.add_gnarl_source('pikeos/adaint-pikeos-p4ext.c')
        self.add_linker_script('pikeos/arm-pikeos5.ld')


class PPCPikeOS5(PikeOS):
    @property
    def name(self):
        return 'ppc-pikeos5'

    @property
    def target(self):
        return 'powerpc-sysgo-pikeos5'

    @property
    def pikeos_version(self):
        return 'pikeos5'

    @property
    def pikeos_target(self):
        return 'ppc/e500mc-4g'

    @property
    def system_ads(self):
        return {
            'light': 'system-pikeos5-ppc.ads',
            'light-tasking': 'system-pikeos5-ppc-ravenscar-sfp.ads',
            'embedded': 'system-pikeos5-ppc-embedded.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('pikeos/runtime_p4ext.xml')

    def __init__(self):
        super(PPCPikeOS5, self).__init__()
        self.add_gnat_source('pikeos/p4ext-cert-app.c')
        self.add_gnarl_source('pikeos/adaint-pikeos-p4ext.c')
        self.add_linker_script('pikeos/ppc-pikeos5.ld')
