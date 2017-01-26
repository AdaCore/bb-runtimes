from arm.cortexar import CortexARTarget


class Rpi2(CortexARTarget):
    @property
    def name(self):
        return "rpi2"

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
    def has_timer_64(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mhard-float',
                '-mcpu=%s' % self.mcu,
                '-mfpu=%s' % self.fpu,
                '-marm', '-mno-unaligned-access')

    @property
    def sfp_system_ads(self):
        return 'system-xi-arm-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-arm-full.ads'

    def __init__(self):
        super(Rpi2, self).__init__(
            mem_routines=True,
            small_mem=False)

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
