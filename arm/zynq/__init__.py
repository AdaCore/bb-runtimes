from build_rts_support.bsp import BSP
from arm.cortexar import CortexARArch, CortexARTarget


class Zynq7kBSP(BSP):
    @property
    def name(self):
        return "zynq7k"

    @property
    def parent(self):
        return CortexARArch

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
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mhard-float',
                '-mcpu=%s' % self.mcu,
                '-mfpu=%s' % self.fpu,
                '-marm', '-mno-unaligned-access')

    def __init__(self):
        super(Zynq7kBSP, self).__init__()
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


class Zynq7000(CortexARTarget):
    @property
    def bspclass(self):
        return Zynq7kBSP

    @property
    def has_timer_64(self):
        return True

    def __init__(self):
        super(Zynq7000, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_ravenscar_sfp(self):
        super(Zynq7000, self).amend_ravenscar_sfp()
        self.update_pairs({'system.ads': 'system-xi-cortexa-sfp.ads'})

    def amend_ravenscar_full(self):
        super(Zynq7000, self).amend_ravenscar_full()
        self.update_pairs({
            'system.ads': 'system-xi-cortexa-full.ads'})
