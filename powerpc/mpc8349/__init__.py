from build_rts_support.bsp import BSP
from powerpc import PPC6XXArch, PPC6XXTarget


class MPC8349BSP(BSP):
    @property
    def parent(self):
        return PPC6XXArch

    @property
    def name(self):
        return 'mpc8349e'

    @property
    def compiler_switches(self):
        return ('-msoft-float', '-mno-sdata')

    def __init__(self):
        super(MPC8349BSP, self).__init__()

        self.add_linker_script('powerpc/mpc8349/ram.ld', loader=None)
        self.add_linker_switch('-Wl,-z,max-page-size=0x1000')
        self.add_sources('crt0', [
            'powerpc/mpc8349/start-ram.S',
            'powerpc/mpc8349/setup.S',
            {'s-macres.adb': 's-macres-8349e.adb',
             's-bbbopa.ads': 's-bbbopa-8349e.ads',
             's-textio.adb': 's-textio-p2020.adb'}])
        self.add_sources('gnarl', {
            's-bbbosu.adb': 's-bbbosu-8349e.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbpara.ads': 's-bbpara-ppc.ads',
            'a-intnam.ads': 'a-intnam-xi-8349e.ads'})


class MPC8349e(PPC6XXTarget):
    @property
    def bspclass(self):
        return MPC8349BSP
