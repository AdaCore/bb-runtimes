from build_rts_support.bsp import BSP
from sparc import LeonArch, LeonTarget


class Leon4BSP(BSP):
    @property
    def name(self):
        return "leon4"

    @property
    def parent(self):
        return LeonArch

    @property
    def loaders(self):
        return ()

    @property
    def c_switches(self):
        # The required compiler switches
        return ('-DLEON', '-DLEON3')

    @property
    def readme_file(self):
        return 'sparc/leon3/README'

    def __init__(self):
        super(Leon4BSP, self).__init__()

        self.add_linker_script('leon3-elf/leon.ld', loader=None)
        self.add_sources('crt0', {
            's-textio.adb': 's-textio-leon3.adb',
            's-bbbopa.ads': 's-bbbopa-leon4.ads'})
        self.add_sources('gnarl', [
            's-bbsle3.ads',
            {'s-bbbosu.adb': 's-bbbosu-leon3.adb',
             's-bbpara.ads': 's-bbpara-leon.ads',
             'a-intnam.ads': 'a-intnam-xi-leon3.ads'}])


class Leon4(LeonTarget):
    @property
    def target(self):
        return 'leon3-elf'

    @property
    def bspclass(self):
        return Leon4BSP
