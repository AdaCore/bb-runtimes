from build_rts_support.bsp import BSP
from aarch64 import Aarch64Arch, Aarch64Target


class AARCH64QEMU(Aarch64Target):
    @property
    def name(self):
        return "qemu"

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def sfp_system_ads(self):
        # Only zfp support for now. To be removed when ravenscar-sfp is in
        return None

    @property
    def full_system_ads(self):
        # Only zfp support for now. To be removed when ravenscar-sfp is in
        return None

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mcpu=cortex-a53')

    def __init__(self):
        super(AARCH64QEMU, self).__init__(
            mem_routines=True,
            small_mem=False)

        self.add_linker_script('aarch64/qemu/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'aarch64/qemu/start-ram.S',
            {'s-textio.adb': 's-textio-zynq.adb',
             's-macres.adb': 's-macres-zynq.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbpara.ads': 's-bbpara-rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-qemu.adb'})
