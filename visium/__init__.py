from support import readfile
from support.bsp_sources.target import DFBBTarget
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

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('visium/mcm/runtime.xml')

    def amend_rts(self, rts_profile, conf):
        conf.build_flags['common_flags'] += ['-muser-mode']

    @property
    def zfp_system_ads(self):
        return 'system-xi-visium.ads'

    def __init__(self):
        super(Visium, self).__init__(
            mem_routines=False,
            small_mem=True)
