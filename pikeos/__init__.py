# BSP support for PowerPC/e500v2
from support import readfile
from support.bsp_sources.target import Target


class PikeOS(Target):
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
        self.add_sources('arch', [
            'pikeos/pikeos-cert-app.c',
            'src/s-textio__pikeos.adb',
            'src/s-macres__native.adb'])
        self.add_sources('gnarl', [
            'pikeos/adaint-pikeos.c',
            'src/a-intnam__dummy.ads'])

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
        if rts_profile == 'ravenscar-full':
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
            'zfp': 'system-pikeos-arm.ads',
            'ravenscar-sfp': 'system-pikeos-arm-ravenscar-sfp.ads',
            'ravenscar-full': 'system-pikeos-arm-ravenscar-full.ads'
        }

    def __init__(self):
        super(ArmPikeOS, self).__init__()
        self.add_linker_script('pikeos/arm-app.ld')
