# BSP support for PowerPC/e500v2
from build_rts_support.bsp import BSP
from build_rts_support.target import Target

from build_rts_support import readfile


class PikeOSBSP(BSP):
    @property
    def name(self):
        return 'pikeos'

    def __init__(self):
        super(PikeOSBSP, self).__init__()
        self.add_linker_script('pikeos/memory.ld')
        self.add_sources('arch', [
            'pikeos-cert-app.c',
            {'s-textio.adb': 's-textio-pikeos.adb',
             's-macres.adb': 's-macres-native.adb'}])
        self.add_sources('gnarl', [
            'adaint-pikeos.c',
            {'a-intnam.ads': 'a-intnam-dummy.ads'}])


class PikeOS(Target):
    @property
    def parent(self):
        return PikeOSBSP

    def __init__(self):
        super(PikeOS, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self, conf):
        conf.rts_xml = readfile('pikeos/runtime.xml')

    def amend_ravenscar_full(self, conf):
        super(PikeOS, self).amend_ravenscar_full(conf)
        # Register ZCX frames (for pikeos-cert-app.c)
        conf.build_flags['c_flags'] += ['-DUSE_ZCX']


class PikeOS3(PikeOS):
    @property
    def pikeos_version(selfs):
        return 'pikeos3'

    def amend_zfp(self, conf):
        super(PikeOS3, self).amend_zfp(conf)
        # Don't use function/data sections, not supported by linker script
        conf.build_flags['common_flags'] = \
            filter(lambda x: x not in ['-ffunction-sections',
                                       '-fdata-sections'],
                   self.build_flags['common_flags'])
        conf.rts_xml = conf.rts_xml.replace(
            '@version@', 'pikeos-3.4')


class PikeOS4(PikeOS):
    @property
    def pikeos_version(selfs):
        return 'pikeos4'

    def amend_zfp(self, conf):
        super(PikeOS4, self).amend_zfp(conf)
        conf.rts_xml = conf.rts_xml.replace(
            '@version@', 'pikeos-4.1')


class ArmPikeOS(PikeOS4):
    @property
    def name(self):
        return 'arm-pikeos'

    @property
    def target(self):
        return 'arm-pikeos'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-arm.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-arm-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-arm-ravenscar-full.ads'

    def amend_zfp(self, conf):
        super(ArmPikeOS, self).amend_zfp(conf)
        conf.rts_xml = conf.rts_xml.replace(
            '@target@', 'arm/v7hf')


class PpcPikeOS(PikeOS3):
    @property
    def name(self):
        return 'ppc-pikeos'

    @property
    def target(self):
        return 'ppc-pikeos'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-ppc.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-ppc-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-ppc-ravenscar-full.ads'

    def amend_zfp(self, conf):
        super(PpcPikeOS, self).amend_zfp(conf)
        conf.rts_xml = conf.rts_xml.replace(
            '/include")', '/include", "-DPPC_OEA")').replace(
            '@target@', 'ppc/oea')


class X86PikeOS(PikeOS3):
    @property
    def name(self):
        return 'x86-pikeos'

    @property
    def target(self):
        return 'x86-pikeos'

    @property
    def zfp_system_ads(self):
        return 'system-pikeos-x86.ads'

    @property
    def sfp_system_ads(self):
        return 'system-pikeos-x86-ravenscar-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-pikeos-x86-ravenscar-full.ads'

    def amend_zfp(self, conf):
        super(X86PikeOS, self).amend_zfp(conf)
        conf.rts_xml = conf.rts_xml.replace(
            '@target@', 'x86/i586')
