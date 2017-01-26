from aarch64 import Aarch64Target


class Rpi3(Aarch64Target):
    @property
    def name(self):
        return "rpi3"

    @property
    def loaders(self):
        return ('RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mcpu=cortex-a53')

    @property
    def full_system_ads(self):
        return None

    def __init__(self):
        super(Rpi3, self).__init__(
            mem_routines=True,
            small_mem=False)

        self.add_linker_script('aarch64/rpi3/ram.ld', loader='RAM')
        self.add_sources('crt0', [
            'i-raspberry_pi.ads',
            'aarch64/rpi3/start-ram.S',
            'aarch64/rpi3/memmap.s',
            'aarch64/rpi3/trap_dump.ads',
            'aarch64/rpi3/trap_dump.adb',
            {'s-textio.adb': 's-textio-rpi2.adb',
             's-macres.adb': 's-macres-rpi2.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-dummy.ads',
            's-bbpara.ads': 's-bbpara-rpi2.ads',
            's-bbbosu.adb': 's-bbbosu-rpi3.adb'})
