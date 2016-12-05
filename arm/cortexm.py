# This module contains cortex-m bsp support
from build_rts_support.bsp import BSP
from build_rts_support.target import Target


class CortexMArch(BSP):
    @property
    def name(self):
        return "cortex-m"

    def __init__(self):
        super(CortexMArch, self).__init__()
        self.add_sources('arch', {
            's-macres.adb': 's-macres-cortexm3.adb'})
        self.add_sources('gnarl', {
            's-bbcppr.adb': 's-bbcppr-armv7m.adb',
            's-bbbosu.adb': 's-bbbosu-armv7m.adb'})
        self.add_sources('gnarl', [
            's-bbcppr.ads',
            's-bbinte.adb',
            's-bbsumu.adb'])


class CortexMTarget(Target):
    @property
    def target(self):
        return "arm-eabi"

    @property
    def has_timer_64(self):
        return False

    @property
    def has_newlib(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return False

    def __init__(self):
        super(CortexMTarget, self).__init__(
            mem_routines=True,
            small_mem=True)

    def amend_zfp(self):
        super(CortexMTarget, self).amend_zfp()
        self.update_pairs({
            'system.ads': 'system-xi-arm.ads'})

    def amend_ravenscar_sfp(self):
        super(CortexMTarget, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-xi-cortexm4-sfp.ads'})

    def amend_ravenscar_full(self):
        super(CortexMTarget, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-cortexm4-full.ads'})
