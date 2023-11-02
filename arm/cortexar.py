# BSP support for Cortex-A/R
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class CortexARArch(ArchSupport):
    @property
    def name(self):
        return "cortex-ar"

    def __init__(self):
        super(CortexARArch, self).__init__()
        self.add_gnat_sources(
            'src/i-arm_v7ar.ads',
            'src/i-arm_v7ar.adb',
            'src/i-cache.ads',
            'src/i-cache__armv7.adb')
        self.add_gnarl_sources(
            'src/s-bbcpsp__arm.ads',
            'src/s-bbcppr__new.ads')


class CortexARTarget(DFBBTarget):
    @property
    def target(self):
        return 'arm-eabi'

    @property
    def parent(self):
        return CortexARArch

    @property
    def has_timer_64(self):
        return True

    def amend_rts(self, rts_profile, conf):
        super(CortexARTarget, self).amend_rts(rts_profile, conf)
        if 'embedded' in rts_profile or 'tasking' in rts_profile:
            # s-bbcppr.adb uses the r7 register during context switching: this
            # is not compatible with the use of frame pointers that is emited
            # at -O0 by gcc. Let's disable fp even at -O0.
            conf.build_flags['common_flags'] += ['-fomit-frame-pointer']

            # The use of FPU registers (to speed up structure init or because
            # SIMD instructions are used) is incompatible with the libgnarl.
            # In fact, the libgnarl needs to take care of FPU registers context
            # switch and does so by doing lazy context switches: this restores
            # the registers only when they are used by apps. This means that if
            # a FPU register is used out of context, then we're doomed.
            conf.build_flags['common_gnarl_flags'] += ['-mgeneral-regs-only']


class AM64xR5(CortexARTarget):
    @property
    def name(self):
        return 'am64xr5'

    @property
    def has_small_memory(self):
        return True

    @property
    def loaders(self):
        return ('RAM',)

    @property
    def cpu(self):
        return 'cortex-r5'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mfloat-abi=hard',
                '-mcpu=%s' % self.cpu, '-mfpu=vfpv3-d16', '-marm')

    @property
    def readme_file(self):
        return 'arm/am64xr5/README'

    @property
    def system_ads(self):
        return {'light-tasking': 'system-xi-arm-light-tasking-no-irq-nesting.ads'}

    def add_linker_scripts(self):
        self.add_linker_script('arm/am64xr5/common.ld')
        self.add_linker_script('arm/am64xr5/memmap.ld')
        self.add_linker_script('arm/am64xr5/ram.ld', loader='RAM')

    def __init__(self):
        super(AM64xR5, self).__init__()

        self.add_linker_scripts()

        self.add_gnat_sources(
            'arm/am64xr5/crt0.S',
            'arm/src/s-mpudef.ads',
            'arm/src/s-mpuini.ads', 'arm/src/s-mpuini.adb',
            'src/s-boapar__am64xr5.ads',
            'src/s-macres__none.adb',
            'src/s-textio__16C750.adb')

        self.add_gnarl_sources(
            'src/a-intnam__am64xr5.ads',
            'src/g-interr__ti_vim.ads', 'src/g-interr__ti_vim.adb',
            'src/s-bbpara__am64xr5.ads', 'src/s-bbbosu__am64xr5.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__arm.adb',
            'src/s-ti.ads',
            'src/s-tvinma.ads', 'src/s-tvinma.adb')


class Rpi2Base(CortexARTarget):
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
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mfloat-abi=hard',
                '-mcpu=%s' % self.mcu,
                '-mfpu=%s' % self.fpu,
                '-marm', '-mno-unaligned-access')

    @property
    def readme_file(self):
        return 'arm/rpi2/README'

    @property
    def system_ads(self):
        return {'light': 'system-xi-arm.ads',
                'light-tasking': 'system-xi-arm-sfp.ads',
                'embedded': 'system-xi-arm-full.ads'}

    def __init__(self):
        super(Rpi2Base, self).__init__()

        self.add_linker_script('arm/rpi2/ram.ld', loader='RAM')
        self.add_gnat_sources(
            'src/i-raspberry_pi.ads',
            'src/s-macres__rpi2.adb')
        self.add_gnarl_sources(
            'src/a-intnam__rpi2.ads',
            'src/s-bbbosu__rpi2.adb',
            'src/s-bbcppr__arm.adb')


class Rpi2(Rpi2Base):
    @property
    def name(self):
        return "rpi2"

    def __init__(self):
        super(Rpi2, self).__init__()

        self.add_gnat_sources(
            'arm/rpi2/start-ram.S',
            'arm/rpi2/memmap.S',
            'src/s-textio__rpi2-mini.adb')
        self.add_gnarl_source(
            'src/s-bbpara__rpi2.ads')


class Rpi2Mc(Rpi2Base):
    @property
    def name(self):
        return "rpi2mc"

    def __init__(self):
        super(Rpi2Mc, self).__init__()

        self.add_gnat_sources(
            'arm/rpi2-mc/start-ram.S',
            'arm/rpi2-mc/memmap.S',
            'src/s-textio__rpi2-pl011.adb')
        self.add_gnarl_source(
            'src/s-bbpara__rpi2.ads')


class TMS570(CortexARTarget):
    @property
    def name(self):

        # The TMS570LS31 runtime originally used the Debug Communication
        # Channel (DCC) for Text_IO as it facilitated inhouse board testing.
        # For other TMS570 runtimes, we default to using SCI1 for Text_IO as
        # this is usable out of the box with the TI TMS570 developer kits
        # where DCC is not as easily available.

        if self.variant == 'tms570ls31':
            base = 'tms570'

            if self.uart_io:
                return "%s_sci" % base
            else:
                return base

        else:
            base = 'tms570lc'

            if self.uart_io:
                return base
            else:
                return "%s_dcc" % base

    @property
    def has_small_memory(self):
        return True

    @property
    def loaders(self):
        return ('LORAM', 'FLASH', 'HIRAM')

    @property
    def cpu(self):
        if self.variant == 'tms570ls31':
            return 'cortex-r4f'
        else:
            return 'cortex-r5'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mbig-endian', '-mfloat-abi=hard', '-mcpu=%s' % self.cpu,
                '-mfpu=vfpv3-d16', '-marm', '-mbe32')

    @property
    def readme_file(self):
        return 'arm/tms570/README'

    @property
    def system_ads(self):
        return {'light': 'system-xi-arm.ads',
                'light-tasking': 'system-xi-arm-sfp.ads',
                'embedded': 'system-xi-arm-full.ads'}

    def add_linker_scripts(self):
        self.add_linker_script('arm/tms570/common.ld')
        self.add_linker_script(
            'arm/tms570/%s.ld' % self.variant, dst='tms570.ld')
        self.add_linker_script('arm/tms570/flash.ld', loader='FLASH')
        self.add_linker_script('arm/tms570/hiram.ld', loader='HIRAM')
        self.add_linker_script('arm/tms570/loram.ld', loader='LORAM')
        self.add_linker_switch('-Wl,-z,max-page-size=0x1000', loader=None)

    def __init__(self, variant='tms570ls31', uart_io=False):
        self.variant = variant
        self.uart_io = uart_io
        super(TMS570, self).__init__()

        self.add_linker_scripts()

        self.add_gnat_sources(
            'arm/tms570/crt0.S',
            'arm/tms570/system_%s.c' % self.variant,
            'arm/tms570/s-tms570.ads', 'arm/tms570/s-tms570.adb',
            'src/s-macres__tms570.adb',
            'src/s-boapar__%s.ads' % self.variant)
        if self.cpu == 'cortex-r4f':
            self.add_gnat_source('arm/tms570/cortex-r4.S')
        if self.uart_io:
            self.add_gnat_source('src/s-textio__tms570-sci.adb')
        else:
            self.add_gnat_source('src/s-textio__tms570-dcc.adb')

        self.add_gnarl_sources(
            'src/a-intnam__%s.ads' % self.variant,
            'src/s-bbpara__%s.ads' % self.variant,
            'src/s-bbbosu__tms570.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__arm.adb')


class ZynqmpR5(CortexARTarget):
    @property
    def name(self):
        return 'zynqmpr5'

    @property
    def has_small_memory(self):
        return False

    @property
    def loaders(self):
        return ('RAM',)

    @property
    def cpu(self):
        return 'cortex-r5'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mfloat-abi=hard',
                '-mcpu=%s' % self.cpu, '-mfpu=vfpv3-d16', '-marm')

    @property
    def readme_file(self):
        return 'arm/zynqmpr5/README'

    @property
    def system_ads(self):
        return {'light': 'system-xi-arm.ads',
                'light-tasking': 'system-xi-arm-gic-sfp.ads',
                'embedded': 'system-xi-arm-gic-full.ads'}

    def add_linker_scripts(self):
        self.add_linker_script('arm/zynqmpr5/common.ld')
        self.add_linker_script('arm/zynqmpr5/memmap.ld')
        self.add_linker_script('arm/zynqmpr5/ram.ld', loader='RAM')

    def __init__(self):
        super(ZynqmpR5, self).__init__()

        self.add_linker_scripts()

        self.add_gnat_sources(
            'arm/zynqmpr5/crt0.S',
            'arm/src/s-mpudef.ads',
            'arm/src/s-mpuini.ads', 'arm/src/s-mpuini.adb',
            'src/s-boapar__zynqmpr5.ads')
        self.add_gnat_sources(
            'src/s-textio__zynqmp.adb',
            'src/s-macres__zynqmp.adb')

        self.add_gnarl_sources(
            'arm/armgic_irqtrap.s',
            'src/a-intnam__zynqmp.ads',
            'src/s-armgic__400.ads', 'src/s-armgic__400.adb',
            'src/s-bbpara__zynqmpr5.ads',
            'src/s-bbbosu__zynqmpr5.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__armgic.adb')


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
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mfloat-abi=hard',
                '-mcpu=%s' % self.mcu,
                '-mfpu=%s' % self.fpu,
                '-marm', '-mno-unaligned-access')

    @property
    def readme_file(self):
        return 'arm/zynq/README'

    @property
    def system_ads(self):
        return {'light': 'system-xi-arm.ads',
                'light-tasking': 'system-xi-arm-gic-sfp.ads',
                'embedded': 'system-xi-arm-gic-full.ads'}

    def __init__(self):
        super(Zynq7000, self).__init__()
        self.add_linker_script('arm/zynq/ram.ld', loader='RAM')
        self.add_gnat_sources(
            'arm/zynq/start-ram.S',
            'arm/zynq/memmap.inc',
            'src/s-textio__zynq.adb',
            'src/s-macres__zynq.adb')
        self.add_gnarl_sources(
            'arm/armgic_irqtrap.s',
            'src/a-intnam__zynq.ads',
            'src/s-bbpara__cortexa9.ads',
            'src/s-armgic__400.ads', 'src/s-armgic__400.adb',
            'src/s-bbbosu__cortexa9.adb',
            'src/s-bbcppr__armgic.adb')
