from arm.cortexar import CortexARTarget


class Zynq7000(CortexARTarget):
    @property
    def name(self):
        return "zynq7000"

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def mcu(self):
        return 'cortex-a9'

    @property
    def fpu(self):
        return 'vfpv3'

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
    def readme_file(self):
        return 'arm/zynq/README'

    @property
    def sfp_system_ads(self):
        return 'system-xi-cortexa-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-cortexa-full.ads'

    def __init__(self):
        super(Zynq7000, self).__init__(
            mem_routines=True,
            small_mem=False)
        self.add_linker_script('arm/zynq/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'arm/zynq/start-ram.S',
            'arm/zynq/memmap.inc',
            {'s-textio.adb': 's-textio-zynq.adb',
             's-macres.adb': 's-macres-zynq.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-xi-zynq.ads',
            's-bbpara.ads': 's-bbpara-cortexa9.ads',
            's-bbbosu.adb': 's-bbbosu-cortexa9.adb'})
