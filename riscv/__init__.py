from support import readfile
from support.target import DFBBTarget
from support.bsp import BSP


class RiscVBSP(BSP):
    @property
    def name(self):
        return 'riscv'

    @property
    def add_linker_section(self):
        return False


class RiscV64(DFBBTarget):
    @property
    def name(self):
        return 'riscv64'

    @property
    def target(self):
        return 'riscv64-elf'

    @property
    def parent(self):
        return RiscVBSP

    @property
    def zfp_system_ads(self):
        return 'system-xi-riscv64.ads'

    def __init__(self):
        super(RiscV64, self).__init__(
            mem_routines=True,
            small_mem=False)


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

    def amend_rts(self, rts_profile, conf):
        conf.rts_xml = readfile('riscv/spike/runtime.xml')

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
