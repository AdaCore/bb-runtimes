# BSP support for PowerPC/e500v2
from build_rts_support.target import Target

from build_rts_support import readfile


class PikeOS(Target):
    def __init__(self):
        super(PikeOS, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(PikeOS, self).amend_zfp()
        self.add_sources('arch', [
            'pikeos/memory.ld',
            'pikeos-cert-app.c'])
        self.update_pairs({
            's-textio.adb': 's-textio-pikeos.adb',
            's-macres.adb': 's-macres-native.adb'})
        # Children should add a pair for system.ads and read runtime.xml

    def amend_ravenscar_sfp(self):
        super(PikeOS, self).amend_ravenscar_sfp()
        self.add_sources('arch', 'adaint-pikeos.c')
        self.update_pairs({
            'a-intnam.ads': 'a-intnam-dummy.ads'})

    def amend_ravenscar_full(self):
        super(PikeOS, self).amend_ravenscar_full()
        # Register ZCX frames (for pikeos-cert-app.c)
        self.build_flags['c_flags'] += ['-DUSE_ZCX']


class PikeOS3(PikeOS):
    def amend_zfp(self):
        super(PikeOS3, self).amend_zfp()
        # Don't use function/data sections, not supported by linker script
        self.build_flags['common_flags'] = \
            filter(lambda x: x not in ['-ffunction-sections',
                                       '-fdata-sections'],
                   self.build_flags['common_flags'])

    def amend_ravenscar_sfp(self):
        super(PikeOS3, self).amend_ravenscar_sfp()
        self.shared.append('gnarl/pikeos3')


class PikeOS4(PikeOS):
    def amend_ravenscar_sfp(self):
        super(PikeOS4, self).amend_ravenscar_sfp()
        self.shared.append('gnarl/pikeos4')


class ArmPikeOS(PikeOS4):
    @property
    def target(self):
        return 'arm-pikeos'

    def amend_zfp(self):
        super(ArmPikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(ArmPikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(ArmPikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-arm-ravenscar-full.ads'})


class PpcPikeOS(PikeOS3):
    @property
    def target(self):
        return 'powerpc-pikeos'

    def amend_zfp(self):
        super(PpcPikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc.ads'})
        self.config_files.update(
            {'runtime.xml': readfile('powerpc/pikeos3/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(PpcPikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(PpcPikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-ppc-ravenscar-full.ads'})


class X86PikeOS(PikeOS3):
    @property
    def target(self):
        return 'x86-pikeos'

    def amend_zfp(self):
        super(X86PikeOS, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86.ads'})
        # FIXME: use an i586 specific runtime.xml ?
        self.config_files.update(
            {'runtime.xml': readfile('pikeos/runtime.xml')})

    def amend_ravenscar_sfp(self):
        super(X86PikeOS, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86-ravenscar-sfp.ads'})

    def amend_ravenscar_full(self):
        super(X86PikeOS, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-pikeos-x86-ravenscar-full.ads'})
