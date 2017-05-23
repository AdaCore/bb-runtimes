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
            'src/i-cache/i-cache.ads',
            'src/i-cache/aarch64/i-cache.adb'])
        self.add_sources('gnarl', [
            'src/s-bbcpsp/aarch64/s-bbcpsp.ads',
            'src/s-bbcppr/new/s-bbcppr.ads',
            'src/s-bbcppr/aarch64/s-bbcppr.adb',
            'aarch64/context_switch.S',
            'src/s-bbinte/generic/s-bbinte.adb'])


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
        return ('RAM', 'MCPART')

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
        self.add_linker_script('aarch64/qemu/mcpart.ld', loader='MCPART')
        self.add_sources('crt0', [
            'aarch64/qemu/start-ram.S',
            'aarch64/qemu/start-part.S',
            'src/s-textio/zynq/s-textio.adb',
            'src/s-macres/zynq/s-macres.adb'])


class Rpi3Base(Aarch64Target):
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
        super(Rpi3Base, self).amend_ravenscar_full(cfg)
        cfg.rts_xml = cfg.rts_xml.replace(
            '"-nostartfiles"',
            ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
             '        "-nostartfiles"'))

    def __init__(self):
        super(Rpi3Base, self).__init__(
            mem_routines=True,
            small_mem=False)

        self.add_linker_script('aarch64/rpi3/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'arm/rpi2/i-raspberry_pi.ads',
            'aarch64/rpi3/trap_dump.ads',
            'aarch64/rpi3/trap_dump.adb',
            'src/s-textio/rpi2/s-textio.adb',
            'src/s-macres/rpi2/s-macres.adb'])
        self.add_sources('gnarl', [
            'arm/rpi2/a-intnam.ads',
            'src/s-bbpara/rpi2/s-bbpara.ads',
            'src/s-bbbosu/rpi3/s-bbbosu.adb'])


class Rpi3(Rpi3Base):
    @property
    def name(self):
        return "rpi3"

    def __init__(self):
        super(Rpi3, self).__init__()

        self.add_sources('crt0', [
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.s'])


class Rpi3Mc(Rpi3Base):
    @property
    def name(self):
        return "rpi3mc"

    def __init__(self):
        super(Rpi3Mc, self).__init__()

        self.add_sources('crt0', [
            'aarch64/rpi3-mc/start-ram.S',
            'aarch64/rpi3-mc/memmap.s'])
