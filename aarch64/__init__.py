# BSP support for ARM64
from support import Compiler, target_compiler
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
            'aarch64/context_switch.S')


class MorelloArch(ArchSupport):
    @property
    def name(self):
        return "aarch64"

    def __init__(self):
        super(MorelloArch, self).__init__()
        self.add_gnat_sources(
            'src/i-aarch64.ads', 'src/i-aarch64.adb',
            'src/i-cache.ads',
            'src/i-cache__aarch64.adb')
        self.add_gnarl_sources(
            'src/s-bbcpsp__morello.ads',
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__morello.adb',
            'aarch64/morello/context_switch.S')

class Aarch64Target(DFBBTarget):
    @property
    def target(self):
        return 'aarch64-elf'

    @property
    def parent(self):
        return Aarch64Arch

    @property
    def has_huge_memory(self):
        return True

    @property
    def has_timer_64(self):
        return True

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {
            'light': 'system-xi-arm.ads',
            'light-tasking': 'system-xi-arm-sfp.ads',
            'embedded': 'system-xi-arm-full.ads'
        }

    def amend_rts(self, rts_profile, conf):
        super(Aarch64Target, self).amend_rts(rts_profile, conf)

        # The use of FPU registers (to speed up structure init or because
        # SIMD instructions are used) is incompatible with the libgnarl.
        # In fact, the libgnarl needs to take care of FPU registers context
        # switch and does so by doing lazy context switches: this restores
        # the registers only when they are used by apps. This means that if
        # a FPU register is used out of context, then we're doomed.
        conf.build_flags['common_gnarl_flags'] += ['-mgeneral-regs-only']

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(Aarch64Target, self).dump_runtime_xml(rts_name, rts)
        if rts_name == 'embedded':
            cnt = cnt.replace(
                '"-nostartfiles"',
                ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
                 '        "-nostartfiles"'))
        return cnt


class MorelloTarget(Aarch64Target):

    @property
    def parent(self):
        return MorelloArch

    @property
    def compiler_switches(self):
        return ('-march=morello+c64', '-mabi=purecap')

    @property
    def system_ads(self):
        result = {
            'light': 'system-xi-arm.ads',
            'light-tasking': 'system-xi-arm-gic-sfp.ads',
        }

        # GNAT-LLVM can't build the embedded Morello runtime yet, so we exclude it for
        # the time being.
        if target_compiler() != Compiler.gnat_llvm:
            result['embedded'] = 'system-xi-arm-gic-full.ads'

        return result


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
        return ('RAM', 'QSPI', 'HELIX')

    @property
    def system_ads(self):
        return {'light': 'system-xi-arm-nxstack-light.ads',
                'light-tasking': 'system-xi-arm-nxstack-light-tasking.ads',
                'embedded': 'system-xi-arm-nxstack-embedded.ads'}

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mcpu=cortex-a53',)

    def amend_rts(self, rts_profile, cfg):
        super(ZynqMP, self).amend_rts(rts_profile, cfg)

    def __init__(self):
        super(ZynqMP, self).__init__()

        self.add_linker_script('aarch64/zynqmp/common.ld')
        self.add_linker_script('aarch64/zynqmp/ram.ld', loader='RAM')
        self.add_linker_script('aarch64/zynqmp/qspi.ld', loader='QSPI')
        self.add_linker_script('aarch64/zynqmp/helix.ld', loader='HELIX')
        self.add_gnat_sources(
            'aarch64/zynqmp/start.S',
            'aarch64/zynqmp/trap_vector.S',
            'aarch64/zynqmp/memmap.S',
            'src/trap_dump__aarch64.ads',
            'src/trap_dump__aarch64.adb',
            'src/s-textio__zynqmp.adb',
            'src/s-macres__zynqmp.adb',
            'src/s-mmu.ads',
            'src/s-mmu__aarch64.adb')
        self.add_gnarl_sources(
            'src/a-intnam__zynqmp.ads',
            'src/s-bbbosu__armv8a.adb',
            'src/s-armgic__400.ads', 'src/s-armgic__400.adb',
            'src/s-bbpara__zynqmp.ads')


class Rpi3Base(Aarch64Target):
    @property
    def loaders(self):
        return ('RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mcpu=cortex-a53',)

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


class Morello(MorelloTarget):
    @property
    def name(self):
        return 'morello'

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def use_semihosting_io(self):
        return True

    @property
    def has_cheri(self):
        return True

    def __init__(self):
        super(Morello, self).__init__()
        self.add_gnat_sources(
            'aarch64/morello/start.S',
            'aarch64/morello/memmap.S',
            'aarch64/morello/trap_vector.S',
            'aarch64/morello/reloc_symbols.S',
            'aarch64/morello/s-morell.ads',
            'aarch64/morello/s-morini.adb',
            'aarch64/morello/s-morini.ads',
            'src/trap_dump__aarch64.ads',
            'src/trap_dump__aarch64.adb',
        )
        self.add_gnarl_sources(
            'src/a-intnam__morello.ads',
            'src/s-bbbosu__morello.adb',
            'src/s-armgic__600.ads', 'src/s-armgic__600.adb',
            'src/s-bbpara__morello.ads'
        )

        if self.use_semihosting_io:
            self.add_gnat_sources(
                'src/s-macres__semihosting.adb',
                'src/s-sgshca__cortexar_c64.adb',
            )
        else:
            self.add_gnat_source('src/s-macres__none.adb')

        self.add_linker_script('aarch64/morello/common.ld')
        self.add_linker_script('aarch64/morello/ram.ld', loader='RAM')
