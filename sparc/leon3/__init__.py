from build_rts_support.bsp import BSP
from sparc import LeonArch, LeonTarget


class Leon3(LeonTarget):
    @property
    def name(self):
        return "leon3"

    @property
    def target(self):
        return 'leon3-elf'

    @property
    def parent(self):
        return LeonArch

    @property
    def c_switches(self):
        # The required compiler switches
        return ('-DLEON', '-DLEON3')

    @property
    def readme_file(self):
        return 'sparc/leon3/README'

    def __init__(self):
        super(Leon3, self).__init__()

        self.add_linker_script('leon3-elf/leon.ld', loader=None)
        self.add_sources('crt0', {
            's-textio.adb': 's-textio-leon3.adb',
            's-bbbopa.ads': 's-bbbopa-leon3.ads'})
        self.add_sources('gnat', [
            'i-leon3.ads',
            'i-leon3-uart.ads',
            'i-leon3-cache.ads'])
        self.add_sources('gnarl', [
            'i-leon3-timers.ads',
            'i-leon3-irqmp.ads',
            {'s-bbbosu.adb': 's-bbbosu-leon3.adb',
             's-bbpara.ads': 's-bbpara-leon.ads',
             'a-intnam.ads': 'a-intnam-xi-leon3.ads'}])
