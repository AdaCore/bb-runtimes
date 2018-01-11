from native import NativeBSP
from support import readfile
from support.bsp_sources.target import DFBBTarget


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

    def has_libc(self, profile):
        return True

    def amend_rts(self, rts_profile, conf):
        conf.rts_vars['Has_libc'] = 'yes'
        conf.rts_xml = readfile('visium/mcm/runtime.xml')
        conf.build_flags['common_flags'] += ['-muser-mode']

    @property
    def has_small_memory(self):
        return True

    @property
    def zfp_system_ads(self):
        return 'system-xi-visium.ads'
