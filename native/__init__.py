from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class NativeBSP(ArchSupport):
    @property
    def name(self):
        return 'native'

    @property
    def add_linker_section(self):
        return False

    def __init__(self):
        super(NativeBSP, self).__init__()
        self.add_sources('crt0', [
            'src/s-macres__native.adb',
            'src/s-textio__stdio.adb'])


class X86Native(DFBBTarget):
    @property
    def name(self):
        return 'x86'

    @property
    def target(self):
        return None

    @property
    def parent(self):
        return NativeBSP

    @property
    def is_native(self):
        return True

    def __init__(self):
        super(X86Native, self).__init__()

    def has_libc(self, profile):
        return True

    @property
    def zfp_system_ads(self):
        return 'system-xi-x86.ads'

    def dump_runtime_xml(self, rts_name, rts):
        return ('<?xml version="1.0" ?>\n'
                '<gprconfig>\n'
                '  <configuration>\n'
                '  </configuration>\n'
                '</gprconfig>\n')


class X8664Native(X86Native):
    @property
    def name(self):
        return 'x86_64'

    @property
    def zfp_system_ads(self):
        return 'system-xi-x86_64.ads'
