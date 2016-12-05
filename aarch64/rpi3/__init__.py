from build_rts_support.bsp import BSP
from aarch64 import Aarch64Arch, Aarch64Target


class Rpi3BSP(BSP):
    @property
    def name(self):
        return "raspberry-pi3"

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mcpu=cortex-a53')

    def __init__(self):
        super(Rpi3BSP, self).__init__()

        self.add_linker_script('aarch64/rpi3/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'i-raspberry_pi.ads',
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.s',
            'aarch64/rpi3/trap_dump.ads',
            'aarch64/rpi3/trap_dump.adb',
            {'s-textio.adb': 's-textio-rpi2.adb',
             's-macres.adb': 's-macres-rpi2.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbpara.ads': 's-bbpara-rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-rpi3.adb'})


class RPI3(Aarch64Target):
    @property
    def bspclass(self):
        return Rpi3BSP

    def __init__(self):
        super(RPI3, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(RPI3, self).amend_zfp()
        self.update_pairs({'system.ads': 'system-xi-aarch64.ads'})

    def amend_ravenscar_sfp(self):
        super(RPI3, self).amend_ravenscar_sfp()
        self.update_pairs({'system.ads': 'system-xi-arm-sfp.ads'})

    def amend_ravenscar_full(self):
        super(RPI3, self).amend_ravenscar_full()
        self.update_pairs({'system.ads': 'system-xi-arm-full.ads'})
