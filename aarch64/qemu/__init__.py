from build_rts_support.bsp import BSP
from aarch64 import Aarch64Arch, Aarch64Target


class QemuBSP(BSP):
    @property
    def name(self):
        return "qemu xilinx-zynq"

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
        super(QemuBSP, self).__init__()

        self.add_linker_script('aarch64/qemu/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'aarch64/qemu/start-ram.S',
            {'s-textio.adb': 's-textio-zynq.adb',
             's-macres.adb': 's-macres-zynq.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbpara.ads': 's-bbpara-rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-qemu.adb'})


class AARCH64QEMU(Aarch64Target):
    @property
    def bspclass(self):
        return QemuBSP

    def __init__(self):
        super(AARCH64QEMU, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_zfp(self):
        super(AARCH64QEMU, self).amend_zfp()
        self.update_pairs({'system.ads': 'system-xi-aarch64.ads'})

    def amend_ravenscar_sfp(self):
        super(AARCH64QEMU, self).amend_ravenscar_sfp()
        self.update_pairs({'system.ads': 'system-xi-arm-sfp.ads'})

    def amend_ravenscar_full(self):
        super(AARCH64QEMU, self).amend_ravenscar_full()
        self.update_pairs({'system.ads': 'system-xi-arm-full.ads'})
