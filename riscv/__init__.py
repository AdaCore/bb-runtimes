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

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('riscv/spike/runtime.xml')

    def __init__(self):
        super(Spike, self).__init__()
        self.add_linker_script('riscv/spike/common-RAM.ld', loader='')
        self.add_linker_script('riscv/spike/memory-map.ld', loader='')
        self.add_sources('crt0',
                         ['riscv/start-ram.S',
                          'riscv/src/riscv_host_target_interface.ads',
                          'riscv/src/riscv_host_target_interface.adb',
                          'src/s-macres__riscv-htif.adb',
                          'src/s-textio__riscv-htif.adb'])
