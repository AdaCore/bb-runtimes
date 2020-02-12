from support.bsp_sources.target import DFBBTarget
from native import NativeBSP
from support import readfile

# For now, we assume we have a full newlib at hand so can behave
# essentially as in the native case. In particular, we inherit the
# NativeBSP behavior for IOs and provide a noop runtime.xml, free of
# options controlling the libc linkage for example.


class RiscVBSP(NativeBSP):
    @property
    def name(self):
        return 'riscv'


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

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('riscv/spike/runtime.xml')

    @property
    def zfp_system_ads(self):
        return 'system-xi-riscv64.ads'

    def __init__(self):
        super(RiscV64, self).__init__()


class Spike(RiscV64):
    @property
    def name(self):
        return 'spike'

    def __init__(self):
        super(Spike, self).__init__()
