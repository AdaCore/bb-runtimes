# BSP support for ARM64
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget


class Aarch64Arch(BSP):
    @property
    def name(self):
        return "aarch64"

    def __init__(self):
        super(Aarch64Arch, self).__init__()
        self.add_sources('arch', {
            'i-cache.ads': 'i-cache.ads',
            'i-cache.adb': 'i-cache-aarch64.adb'})
        self.add_sources('gnarl', [
            {'s-bbcpsp.ads': 's-bbcpsp-aarch64.ads',
             's-bbcppr.ads': 's-bbcppr-ppc.ads',
             's-bbcppr.adb': 's-bbcppr-aarch64.adb'},
            'aarch64/context_switch.S',
            's-bbinte.adb'])


class Aarch64Target(DFBBTarget):
    @property
    def target(self):
        return 'aarch64-elf'

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def has_newlib(self):
        return True

    @property
    def has_timer_64(self):
        return True

    @property
    def zfp_system_ads(self):
        return 'system-xi-aarch64.ads'

    @property
    def sfp_system_ads(self):
        return 'system-xi-arm-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-arm-full.ads'
