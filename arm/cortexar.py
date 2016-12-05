# BSP support for Cortex-A/R
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget


class CortexARArch(BSP):
    @property
    def name(self):
        return "cortex-ar"

    def __init__(self):
        super(CortexARArch, self).__init__()
        self.add_sources('arch', [
            'i-arm_v7ar.ads',
            'i-arm_v7ar.adb',
            {'i-cache.ads': 'i-cache.ads',
             'i-cache.adb': 'i-cache-armv7.adb'}])
        self.add_sources('gnarl', [
            {'s-bbcpsp.ads': 's-bbcpsp-arm.ads',
             's-bbcppr.adb': 's-bbcppr-arm.adb'},
            's-bbcppr.ads',
            's-bbinte.adb'])


class CortexARTarget(DFBBTarget):
    @property
    def target(self):
        return 'arm-eabi'

    @property
    def has_newlib(self):
        return True

    def amend_zfp(self):
        super(CortexARTarget, self).amend_zfp()
        self.update_pairs(
            {'system.ads': 'system-xi-arm.ads'})
