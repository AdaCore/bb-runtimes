from build_rts_support import readfile
from build_rts_support.target import DFBBTarget
from native import NativeBSP


class Visium(DFBBTarget):
    @property
    def target(self):
        return 'visium-elf'

    @property
    def bspclass(self):
        return NativeBSP

    def __init__(self):
        super(Visium, self).__init__(
            mem_routines=False,
            small_mem=True)
        self.build_flags['common_flags'] += ['-muser-mode']

    def amend_zfp(self):
        super(Visium, self).amend_zfp()
        self.update_pairs(
            {'system.ads': 'system-xi-visium.ads'})
