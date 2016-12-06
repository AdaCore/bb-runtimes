from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget


class NativeBSP(BSP):
    @property
    def name(self):
        return 'native'

    def __init__(self):
        super(NativeBSP, self).__init__()
        self.add_sources('crt0', {
            's-macres.adb': 's-macres-native.adb',
            's-textio.adb': 's-textio-stdio.adb'})


class X86Native(DFBBTarget):
    @property
    def bspclass(self):
        return NativeBSP

    def __init__(self):
        super(X86Native, self).__init__(
            mem_routines=False,
            small_mem=False)

    def amend_zfp(self):
        super(X86Native, self).amend_zfp()
        self.update_pairs(
            {'system.ads': 'system-xi-x86.ads'})


class X86Linux(X86Native):
    @property
    def target(self):
        return 'x86-linux'

    def __init__(self):
        super(X86Linux, self).__init__()


class X86Windows(X86Native):
    @property
    def target(self):
        return 'x86-windows'

    def __init__(self):
        super(X86Windows, self).__init__()
