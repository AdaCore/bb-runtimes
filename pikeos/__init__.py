# BSP support for PowerPC/e500v2
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import Target

from support import readfile


class PikeOSBSP(ArchSupport):
    @property
    def name(self):
        return 'pikeos'

    def __init__(self):
        super(PikeOSBSP, self).__init__()
        self.add_linker_script('pikeos/memory.ld')
        self.add_sources('arch', [
            'pikeos/pikeos-cert-app.c',
            'src/s-textio__pikeos.adb',
            'src/s-macres__native.adb'])
        self.add_sources('gnarl', [
            'pikeos/adaint-pikeos.c',
            'src/a-intnam__dummy.ads'])


class PikeOS(Target):
    @property
    def parent(self):
        return PikeOSBSP

    def __init__(self):
        super(PikeOS, self).__init__(
            mem_routines=True,
            small_mem=False)

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


class PikeOS3(PikeOS):
    @property
    def pikeos_version(selfs):
        return 'pikeos3'

    def amend_rts(self, rts_profile, conf):
        super(PikeOS3, self).amend_rts(rts_profile, conf)
        # Don't use function/data sections, not supported by linker script
        conf.build_flags['common_flags'] = \
            filter(lambda x: x not in ['-ffunction-sections',
                                       '-fdata-sections'],
                   self.build_flags['common_flags'])


class PikeOS4(PikeOS):
    @property
    def pikeos_version(selfs):
        return 'pikeos4'


class ArmPikeOS(PikeOS4):
    @property
    def name(self):
        return 'arm-pikeos'

    @property
    def target(self):
        return 'arm-pikeos'

    @property
    def pikeos_version(self):
        return 'pikeos4'

    @property
    def pikeos_target(self):
        return 'arm/v7hf'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-arm.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-arm-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-arm-ravenscar-full.ads'

    def __init__(self):
        super(ArmPikeOS, self).__init__()
        self.add_linker_script('pikeos/arm-app.ld')


class PpcPikeOS(PikeOS3):
    @property
    def name(self):
        return 'ppc-pikeos'

    @property
    def target(self):
        return 'ppc-pikeos'

    @property
    def pikeos_version(self):
        return 'pikeos3'

    @property
    def pikeos_target(self):
        return 'ppc/oea'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-ppc.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-ppc-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-ppc-ravenscar-full.ads'

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(PpcPikeOS, self).dump_runtime_xml(rts_name, rts)
        cnt = cnt.replace('/include")', '/include", "-DPPC_OEA")')
        return cnt


class X86PikeOS(PikeOS3):
    @property
    def name(self):
        return 'x86-pikeos'

    @property
    def target(self):
        return 'x86-pikeos'

    @property
    def pikeos_target(self):
        return 'x86/i586'

    @property
    def pikeos_version(self):
        return 'pikeos3'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-x86.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-x86-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-x86-ravenscar-full.ads'
