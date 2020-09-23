from support.bsp_sources.target import DFBBTarget


class X86Native(DFBBTarget):
    @property
    def name(self):
        return 'native-x86'

    @property
    def target(self):
        return None

    def __init__(self):
        super(X86Native, self).__init__()
        self.add_gnat_sources(
            'src/s-macres__native.adb',
            'src/s-textio__stdio.adb')

    def has_libc(self, profile):
        return True

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-x86.ads'}

    def dump_runtime_xml(self, rts_name, rts):
        return ('<?xml version="1.0" ?>\n'
                '<gprconfig>\n'
                '  <configuration>\n'
                '  </configuration>\n'
                '</gprconfig>\n')

    def amend_rts(self, rts_profile, cfg):
        super(X86Native, self).amend_rts(rts_profile, cfg)


class X8664Native(X86Native):
    @property
    def name(self):
        return 'native-x86_64'

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-x86_64.ads'}
