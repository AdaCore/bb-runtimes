# BSP support for ARM64
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class Aarch64Arch(ArchSupport):
    @property
    def name(self):
        return "aarch64"

    def __init__(self):
        super(Aarch64Arch, self).__init__()
        self.add_gnat_sources(
            'src/i-aarch64.ads', 'src/i-aarch64.adb',
            'src/i-cache.ads',
            'src/i-cache__aarch64.adb')
        self.add_gnarl_sources(
            'src/s-bbcpsp__aarch64.ads',
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__aarch64.adb',
            'aarch64/context_switch.S',
            'src/s-bbinte__generic.adb')


class Aarch64Target(DFBBTarget):
    @property
    def target(self):
        return 'aarch64-elf'

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def has_timer_64(self):
        return True

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-arm.ads',
            'ravenscar-sfp': 'system-xi-arm-sfp.ads',
            'ravenscar-full': 'system-xi-arm-full.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(Aarch64Target, self).dump_runtime_xml(rts_name, rts)
        if rts_name == 'ravenscar-full':
            cnt = cnt.replace(
                '"-nostartfiles"',
                ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
                 '        "-nostartfiles"'))
        return cnt


class ZynqMP(Aarch64Target):
    @property
    def name(self):
        return "zynqmp"

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def readme_file(self):
        return 'aarch64/zynqmp/README'

    @property
    def loaders(self):
        return ('RAM', 'QSPI')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'system-xi-arm-gic-sfp.ads',
                'ravenscar-full': 'system-xi-arm-gic-full.ads'}

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mcpu=cortex-a53')

    def amend_rts(self, rts_profile, cfg):
        super(ZynqMP, self).amend_rts(rts_profile, cfg)

    def __init__(self):
        super(ZynqMP, self).__init__()

        self.add_linker_script('aarch64/zynqmp/common.ld')
        self.add_linker_script('aarch64/zynqmp/ram.ld', loader='RAM')
        self.add_linker_script('aarch64/zynqmp/qspi.ld', loader='QSPI')
        self.add_gnat_sources(
            'aarch64/zynqmp/start.S',
            'aarch64/zynqmp/trap_vector.S',
            'aarch64/zynqmp/memmap.S',
            'src/trap_dump__aarch64.ads',
            'src/trap_dump__aarch64.adb',
            'src/s-textio__zynqmp.adb',
            'src/s-macres__zynqmp.adb')
        self.add_gnarl_sources(
            'src/a-intnam__zynqmp.ads',
            'src/s-bbbosu__armv8a.adb',
            'src/s-bbpara__zynqmp.ads')


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

    def __init__(self):
        super(Rpi3Base, self).__init__()

        self.add_linker_script('aarch64/rpi3/ram.ld', loader='RAM')
        self.add_gnat_sources(
            'src/i-raspberry_pi.ads',
            'src/trap_dump__aarch64.ads',
            'src/trap_dump__aarch64.adb',
            'src/s-macres__rpi2.adb')
        self.add_gnarl_sources(
            'src/a-intnam__rpi2.ads',
            'src/s-bbbosu__rpi3.adb')


class Rpi3(Rpi3Base):
    @property
    def name(self):
        return "rpi3"

    def __init__(self):
        super(Rpi3, self).__init__()

        self.add_gnat_sources(
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.S',
            'src/s-textio__rpi2-mini.adb')
        self.add_gnarl_source('src/s-bbpara__rpi2.ads')


class Rpi3Mc(Rpi3Base):
    @property
    def name(self):
        return "rpi3mc"

    def __init__(self):
        super(Rpi3Mc, self).__init__()

        self.add_gnat_sources(
            'aarch64/rpi3-mc/start-ram.S',
            'aarch64/rpi3-mc/traps_el3.S',
            'aarch64/rpi3-mc/traps_el2cur.S',
            'aarch64/rpi3-mc/traps_el2low.S',
            'aarch64/rpi3-mc/traps_common.h',
            'aarch64/rpi3-mc/memmap.S',
            'src/s-textio__rpi2-pl011.adb')
        self.add_gnarl_source('src/s-bbpara__rpi2-hyp.ads')
