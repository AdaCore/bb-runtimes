from build_rts_support.bsp import BSP
from sparc import LeonArch, LeonTarget


class Leon2BSP(BSP):
    @property
    def name(self):
        return "leon2"

    @property
    def parent(self):
        return LeonArch

    @property
    def loaders(self):
        return ()

    @property
    def c_switches(self):
        # The required compiler switches
        return ('-DLEON', '-DLEON2')

    def __init__(self):
        super(Leon2BSP, self).__init__()

        self.add_linker_script('leon-elf/leon.ld', loader=None)
        self.add_sources('crt0', {
            's-textio.adb': 's-textio-leon.adb',
            's-bbbopa.ads': 's-bbbopa-leon.ads'})
        self.add_sources('gnarl', [
            's-bbsule.ads',
            's-bbsumu.adb',
            {'s-bbbosu.adb': 's-bbbosu-leon.adb',
             's-bbpara.ads': 's-bbpara-leon.ads',
             'a-intnam.ads': 'a-intnam-xi-leon.ads'}])


class Leon2(LeonTarget):
    @property
    def target(self):
        return 'leon-elf'

    @property
    def bspclass(self):
        return Leon2BSP
