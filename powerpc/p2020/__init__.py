from build_rts_support import readfile
from build_rts_support.bsp import BSP
from powerpc import PPCSPEArch, PPCSPETarget


class P2020BSP(BSP):
    @property
    def parent(self):
        return PPCSPEArch

    @property
    def name(self):
        return 'p2020rdb'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=double')

    def __init__(self):
        super(P2020BSP, self).__init__()
        self.add_linker_script('powerpc/p2020/p2020.ld', loader=None)
        self.add_sources('crt0', [
            'powerpc/p2020/start-ram.S',
            'powerpc/p2020/setup.S',
            {'s-macres.adb': 's-macres-p2020.adb',
             's-bbbopa.ads': 's-bbbopa-p2020.ads',
             's-textio.adb': 's-textio-p2020.adb'}])
        self.add_sources('gnarl', [
            's-bbsumu.adb',
            {'s-bbbosu.adb': 's-bbbosu-ppc-openpic.adb',
             's-bbsuti.adb': 's-bbsuti-ppc.adb',
             's-bbpara.ads': 's-bbpara-ppc.ads',
             'a-intnam.ads': 'a-intnam-xi-ppc-openpic.ads'}])


class P2020(PPCSPETarget):
    @property
    def bspclass(self):
        return P2020BSP
