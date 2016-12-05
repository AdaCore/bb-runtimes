from build_rts_support.bsp import BSP
from powerpc import PPCSPEArch, PPCSPETarget


class P5634BSP(BSP):
    @property
    def parent(self):
        return PPCSPEArch

    @property
    def name(self):
        return 'p5634'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=single')

    def __init__(self):
        super(P5634BSP, self).__init__()
        self.add_linker_script('powerpc/mpc5634/5634.ld', loader=None)
        self.add_sources('crt0', [
            'powerpc/mpc5634/start.S',
            {'s-macres.adb': 's-macres-p55.adb',
             's-textio.adb': 's-textio-p55.adb'}])


class P5634(PPCSPETarget):
    @property
    def bspclass(self):
        return P5634BSP
