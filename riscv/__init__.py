from support import readfile
from support.bsp_sources.target import DFBBTarget


class RiscV64(DFBBTarget):
    @property
    def name(self):
        return 'riscv64'

    @property
    def target(self):
        return 'riscv64-elf'

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
        return {'zfp': 'system-xi-riscv.ads'}

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(RiscV64, self).dump_runtime_xml(rts_name, rts)
        if rts_name == 'ravenscar-full':
            cnt = cnt.replace(
                '"-nostartfiles"',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')
        return cnt

    def amend_rts(self, rts_profile, conf):
        super(DFBBTarget, self).amend_rts(rts_profile, conf)
        if rts_profile == 'ravenscar-full':
            conf.config_files.update(
                {'link-zcx.spec': readfile('riscv/link-zcx.spec')})


class Spike(RiscV64):
    @property
    def name(self):
        return 'spike'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ["-mcmodel=medany"]

    @property
    def loaders(self):
        return ('RAM', )

    def __init__(self):
        super(Spike, self).__init__()
        self.add_linker_script('riscv/spike/memory-map.ld')
        self.add_linker_script('riscv/spike/common-RAM.ld', loader='RAM')
        self.add_gnat_sources(
            'riscv/start-ram.S',
            'riscv/src/riscv_host_target_interface.ads',
            'riscv/src/riscv_host_target_interface.adb',
            'src/s-macres__riscv-htif.adb',
            'src/s-textio__riscv-htif.adb')


class Unleashed(RiscV64):
    @property
    def name(self):
        return 'unleashed'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ['-march=rv64imafdc', '-mabi=lp64d']

    @property
    def system_ads(self):
        return {'zfp':            'system-xi-riscv.ads',
                'ravenscar-sfp':  'system-xi-riscv-sifive-sfp.ads',
                'ravenscar-full': 'system-xi-riscv-sifive-full.ads'}

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def loaders(self):
        return ('RAM', )

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(Unleashed, self).dump_runtime_xml(rts_name, rts)
        return cnt.replace(
            '"common-RAM.ld"',
            '"common-RAM.ld",\n'
            '               --  This symbol is used by GDB to know the\n'
            '               --  hardware id of the first CPU used by\n'
            '               --  the run-time. The -u option is used to make\n'
            '               --  sure the symbol is not discared in the final\n'
            '               --  binary.\n'
            '               "-u", "__gnat_gdb_cpu_first_id"')

    def __init__(self):
        super(Unleashed, self).__init__()

        self.add_linker_script('riscv/sifive/unleashed/memory-map.ld')
        self.add_linker_script('riscv/sifive/unleashed/common-RAM.ld',
                               loader='RAM')
        self.add_gnat_sources(
            'riscv/sifive/unleashed/start-ram.S',
            'riscv/sifive/fe310/svd/i-fe310.ads',
            'riscv/sifive/fe310/svd/i-fe310-uart.ads',
            'riscv/sifive/fe310/svd/i-fe310-gpio.ads',
            'riscv/sifive/fe310/svd/i-fe310-plic.ads',
            'riscv/sifive/fe310/s-macres.adb',
            'riscv/sifive/unleashed/s-textio.adb',
            'riscv/src/riscv_def.h')
        self.add_gnarl_sources(
            'riscv/sifive/fu540/svd/a-intnam.ads',
            'src/s-bbpara__riscv.ads',
            'src/s-bbbopa__unleashed.ads',
            'src/s-bbbosu__riscv.adb',
            'src/s-bbsuti__riscv_clint.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__riscv.adb',
            'src/s-bbcpsp__riscv.ads',
            'src/s-bbcpsp__riscv.adb',
            'riscv/src/context_switch.S',
            'riscv/src/trap_handler.S',
            'riscv/src/s-bbripl.ads',
            'riscv/sifive/fe310/s-bbripl.adb')


class PolarFireSOC(Unleashed):
    @property
    def name(self):
        return 'polarfiresoc'

    def __init__(self):
        super(Unleashed, self).__init__()

        self.add_linker_script('riscv/sifive/unleashed/memory-map.ld')
        self.add_linker_script('riscv/sifive/unleashed/common-RAM.ld',
                               loader='RAM')
        self.add_gnat_sources(
            'riscv/sifive/unleashed/start-ram.S',
            'riscv/sifive/fe310/svd/i-fe310.ads',
            'riscv/sifive/fe310/svd/i-fe310-plic.ads',
            'riscv/sifive/fe310/s-macres.adb',
            'src/s-textio__ns16550.adb',
            'src/s-bbbopa__polarfiresoc.ads',
            'riscv/src/riscv_def.h')
        self.add_gnarl_sources(
            'riscv/microchip/polarfiresoc/a-intnam.ads',
            'src/s-bbpara__polarfiresoc.ads',
            'src/s-bbbosu__riscv.adb',
            'src/s-bbsuti__riscv_clint.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__riscv.adb',
            'src/s-bbcpsp__riscv.ads',
            'src/s-bbcpsp__riscv.adb',
            'riscv/src/context_switch.S',
            'riscv/src/trap_handler.S',
            'riscv/src/s-bbripl.ads',
            'riscv/microchip/polarfiresoc/s-bbripl.adb')


class RiscV32(DFBBTarget):
    @property
    def name(self):
        return 'riscv32'

    @property
    def target(self):
        return 'riscv32-elf'

    @property
    def has_timer_64(self):
        return True


class HiFive1(RiscV32):
    @property
    def name(self):
        return 'hifive1'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ['-march=rv32imac', '-mabi=ilp32']

    @property
    def has_small_memory(self):
        return True

    @property
    def loaders(self):
        return ('ROM', )

    @property
    def system_ads(self):
        # Only ZFP for for this target, the ravenscar run-times are not stable
        # for the moment.
        return {'zfp': 'system-xi-riscv.ads'}

    def __init__(self):
        super(HiFive1, self).__init__()
        self.add_linker_script('riscv/sifive/hifive1/memory-map.ld')
        self.add_linker_script('riscv/sifive/hifive1/common-ROM.ld',
                               loader='ROM')
        self.add_gnat_sources(
            'riscv/sifive/fe310/start-rom.S',
            'riscv/sifive/fe310/svd/i-fe310.ads',
            'riscv/sifive/fe310/svd/i-fe310-uart.ads',
            'riscv/sifive/fe310/svd/i-fe310-gpio.ads',
            'riscv/sifive/fe310/svd/i-fe310-plic.ads',
            'riscv/sifive/fe310/s-macres.adb',
            'riscv/sifive/hifive1/s-textio.adb',
            'riscv/src/riscv_def.h')
        self.add_gnarl_sources(
            'riscv/sifive/fe310/svd/a-intnam.ads',
            'src/s-bbpara__riscv.ads',
            'src/s-bbbopa__hifive1.ads',
            'src/s-bbbosu__riscv.adb',
            'src/s-bbsuti__riscv_clint.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__riscv.adb',
            'src/s-bbcpsp__riscv.ads',
            'src/s-bbcpsp__riscv.adb',
            'riscv/src/context_switch.S',
            'riscv/src/trap_handler.S',
            'riscv/src/s-bbripl.ads',
            'riscv/sifive/fe310/s-bbripl.adb')


class RV32BASE(RiscV32):
    """
    Generic ZFP run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    def compiler_switches(self):
        # The required compiler switches
        if self.name == "rv32i":
            return ['-march=rv32i', '-mabi=ilp32']

        elif self.name == "rv32im":
            return ['-march=rv32im', '-mabi=ilp32']

        elif self.name == "rv32iac":
            return ['-march=rv32iac', '-mabi=ilp32']

        elif self.name == "rv32imac":
            return ['-march=rv32imac', '-mabi=ilp32']

        elif self.name == "rv32imafc":
            return ['-march=rv32imafc', '-mabi=ilp32f']

        elif self.name == "rv32imafdc":
            return ['-march=rv32imafdc', '-mabi=ilp32d']

    @property
    def has_small_memory(self):
        return True

    @property
    def system_ads(self):
        # Only ZFP for for this generic target
        return {'zfp': 'system-xi-riscv.ads'}

    def __init__(self):
        super(RV32BASE, self).__init__()

        self.add_gnat_sources(
            'riscv/sifive/fe310/s-macres.adb',
            'src/s-textio__stdio.adb')


class RV32I(RV32BASE):
    @property
    def name(self):
        return 'rv32i'


class RV32IM(RV32BASE):
    @property
    def name(self):
        return 'rv32im'


class RV32IAC(RV32BASE):
    @property
    def name(self):
        return 'rv32iac'


class RV32IMAC(RV32BASE):
    @property
    def name(self):
        return 'rv32imac'


class RV32IMAFC(RV32BASE):
    @property
    def name(self):
        return 'rv32imafc'


class RV32IMAFDC(RV32BASE):
    @property
    def name(self):
        return 'rv32imafdc'


class RV64BASE(RiscV64):
    """
    Generic ZFP run-time meant to be used with the startup generator (crt0 and
    ld script).
    """

    @property
    def compiler_switches(self):
        # The required compiler switches
        if self.name == "rv64im":
            return ['-march=rv64im', '-mabi=lp64']

        elif self.name == "rv64imc":
            return ['-march=rv64imc', '-mabi=lp64']

        elif self.name == "rv64imac":
            return ['-march=rv64imac', '-mabi=lp64']

        elif self.name == "rv64imafc":
            return ['-march=rv64imafc', '-mabi=lp64f']

        elif self.name == "rv64imfc":
            return ['-march=rv64imfc', '-mabi=lp64f']

        elif self.name == "rv64imafdc":
            return ['-march=rv64imafdc', '-mabi=lp64d']

    @property
    def system_ads(self):
        # Only ZFP for for this generic target
        return {'zfp': 'system-xi-riscv.ads'}

    def __init__(self):
        super(RV64BASE, self).__init__()

        self.add_gnat_sources(
            'riscv/sifive/fe310/s-macres.adb',
            'src/s-textio__stdio.adb')


class RV64IM(RV64BASE):
    @property
    def name(self):
        return 'rv64im'


class RV64IMC(RV64BASE):
    @property
    def name(self):
        return 'rv64imc'


class RV64IMAC(RV64BASE):
    @property
    def name(self):
        return 'rv64imac'


class RV64IMAFC(RV64BASE):
    @property
    def name(self):
        return 'rv64imafc'


class RV64IMFC(RV64BASE):
    @property
    def name(self):
        return 'rv64imfc'


class RV64IMAFDC(RV64BASE):
    @property
    def name(self):
        return 'rv64imafdc'
