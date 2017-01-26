from build_rts_support import readfile
from build_rts_support.target import DFBBTarget
from native import NativeBSP


class VisiumBSP(NativeBSP):
    @property
    def name(self):
        return 'visium'


class Visium(DFBBTarget):
    @property
    def name(self):
        return 'mcm'

    @property
    def target(self):
        return 'visium-elf'

    @property
    def parent(self):
        return VisiumBSP

    @property
    def zfp_system_ads(self):
        return 'system-xi-visium.ads'

    def __init__(self):
        super(Visium, self).__init__(
            mem_routines=False,
            small_mem=True)
        self.build_flags['common_flags'] += ['-muser-mode']
