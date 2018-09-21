from support.bsp_sources.target import DFBBTarget


class RiscV64(DFBBTarget):
    @property
    def name(self):
        return 'riscv64'

    @property
    def target(self):
        return 'riscv64-elf'

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-riscv64.ads'}


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
        return ['RAM']

    def __init__(self):
        super(Spike, self).__init__()
        self.add_linker_script('riscv/spike/common-RAM.ld', loader='RAM')
        self.add_linker_script('riscv/spike/memory-map.ld', loader='RAM')
        self.add_sources('crt0',
                         ['riscv/start-ram.S',
                          'riscv/src/riscv_host_target_interface.ads',
                          'riscv/src/riscv_host_target_interface.adb',
                          'src/s-macres__riscv-htif.adb',
                          'src/s-textio__riscv-htif.adb'])


class RiscV32(DFBBTarget):
    @property
    def name(self):
        return 'riscv32'

    @property
    def target(self):
        return 'riscv32-elf'

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-riscv32.ads'}


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
        return ['ROM']

    def __init__(self):
        super(HiFive1, self).__init__()
        self.add_linker_script('riscv/sifive/hifive1/common-ROM.ld',
                               loader='ROM')
        self.add_linker_script('riscv/sifive/hifive1/memory-map.ld',
                               loader='ROM')
        self.add_sources('crt0', ['riscv/sifive/fe310/start-rom.S',
                                  'riscv/sifive/fe310/svd/i-fe310.ads',
                                  'riscv/sifive/fe310/svd/i-fe310-uart.ads',
                                  'riscv/sifive/fe310/svd/i-fe310-gpio.ads',
                                  'riscv/sifive/fe310/s-macres.adb',
                                  'riscv/sifive/hifive1/s-textio.adb'])


class PicoRV32(RiscV32):
    @property
    def name(self):
        return 'picorv32'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ['-march=rv32imc', '-mabi=ilp32']

    @property
    def has_small_memory(self):
        return True

    @property
    def loaders(self):
        return ['ROM']

    def __init__(self):
        super(PicoRV32, self).__init__()

        # Use the same base linker script as the HiFive1
        self.add_linker_script('riscv/sifive/hifive1/common-ROM.ld',
                               loader='ROM')

        self.add_linker_script('riscv/picorv32/memory-map.ld',
                               loader='ROM')

        # Use the same startup code as the HiFive1
        self.add_sources('crt0', ['riscv/sifive/fe310/start-rom.S',
                                  'riscv/sifive/fe310/s-macres.adb',
                                  'riscv/picorv32/s-textio.adb'])
