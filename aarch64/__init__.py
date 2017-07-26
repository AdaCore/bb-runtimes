# BSP support for ARM64
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget


class Aarch64Arch(BSP):
    @property
    def name(self):
        return "aarch64"

    def __init__(self):
        super(Aarch64Arch, self).__init__()
        self.add_sources('arch', [
            'src/i-cache.ads',
            'src/i-cache__aarch64.adb'])
        self.add_sources('gnarl', [
            'src/s-bbcpsp__aarch64.ads',
            'src/s-bbcppr__ppc.ads',
            'src/s-bbcppr__aarch64.adb',
            'aarch64/context_switch.S',
            'src/s-bbinte.adb'])


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
            's-bbpara.ads': 'src/s-bbpara__rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-qemu.adb'})


class Rpi3(Aarch64Target):
    @property
    def name(self):
        return "rpi3"

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mcpu=cortex-a53')

    @property
    def readme_file(self):
        return 'arm/rpi2/README'

    def amend_ravenscar_full(self, cfg):
        super(Rpi3, self).amend_ravenscar_full(cfg)
        cfg.rts_xml = cfg.rts_xml.replace(
            '"-nostartfiles"',
            ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
             '        "-nostartfiles"'))

    def __init__(self):
        super(Rpi3, self).__init__(
            mem_routines=True,
            small_mem=False)

        self.add_linker_script('aarch64/rpi3/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'src/i-raspberry_pi.ads',
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.s',
            'aarch64/rpi3/trap_dump.ads',
            'aarch64/rpi3/trap_dump.adb',
            'src/s-textio__rpi2.adb',
            'src/s-macres__rpi2.adb'])
        self.add_sources('gnarl', [
            'src/a-intnam__rpi2.ads',
            'src/s-bbpara__rpi2.ads',
            'src/s-bbbosu__rpi3.adb'])
