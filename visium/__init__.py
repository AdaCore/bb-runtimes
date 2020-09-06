from support import readfile
from support.bsp_sources.target import DFBBTarget


class Visium(DFBBTarget):
    @property
    def name(self):
        return 'mcm'

    @property
    def target(self):
        return 'visium-elf'

    def has_libc(self, profile):
        return True

    @property
    def has_small_memory(self):
        return True

    def __init__(self):
        super(Visium, self).__init__()
        self.add_gnat_sources(
            'src/s-macres__native.adb',
            'src/s-textio__stdio.adb')

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('visium/mcm/runtime.xml')

    def amend_rts(self, rts_profile, conf):
        conf.build_flags['common_flags'] += ['-muser-mode']

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-visium.ads'}
