from build_rts_support.bsp import BSP
from arm.cortexar import CortexARArch, CortexARTarget


class Rpi2BSP(BSP):
    @property
    def name(self):
        return "raspberry-pi2"

    @property
    def parent(self):
        return CortexARArch

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def mcu(self):
        return 'cortex-a7'

    @property
    def fpu(self):
        return 'vfpv4'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mhard-float',
                '-mcpu=%s' % self.mcu,
                '-mfpu=%s' % self.fpu,
                '-marm', '-mno-unaligned-access')

    def __init__(self):
        super(Rpi2BSP, self).__init__()

        self.add_linker_script('arm/rpi2/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'i-raspberry_pi.ads',
            'arm/rpi2/start-ram.S',
            'arm/rpi2/memmap.s',
            {'s-textio.adb': 's-textio-rpi2.adb',
             's-macres.adb': 's-macres-rpi2.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbpara.ads': 's-bbpara-rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-rpi2.adb'})


class RPI2(CortexARTarget):
    @property
    def bspclass(self):
        return Rpi2BSP

    @property
    def has_timer_64(self):
        return True

    def __init__(self):
        super(RPI2, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_ravenscar_sfp(self):
        super(RPI2, self).amend_ravenscar_sfp()
        self.update_pairs({
            'system.ads': 'system-xi-arm-sfp.ads'})

    def amend_ravenscar_full(self):
        super(RPI2, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-arm-full.ads'})
