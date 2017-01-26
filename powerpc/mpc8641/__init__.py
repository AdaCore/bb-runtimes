from build_rts_support.bsp import BSP
from powerpc import PPC6XXArch, PPC6XXTarget


class MPC8641(PPC6XXTarget):
    @property
    def name(self):
        return 'mpc8641'

    @property
    def compiler_switches(self):
        return ('-mhard-float', )

    @property
    def loaders(self):
        return ('ROM', 'RAM')

    def __init__(self):
        super(MPC8641, self).__init__()
        self.add_linker_script('powerpc/mpc8641/qemu-rom.ld', loader='ROM')
        self.add_linker_switch('-Wl,-u_start_rom', loader='ROM')
        self.add_linker_script('powerpc/mpc8641/ram.ld', loader='RAM')
        self.add_linker_switch('-Wl,-u_start_ram', loader='RAM')

        self.add_sources('crt0', [
            'powerpc/mpc8641/start-rom.S',
            'powerpc/mpc8641/setup.S'])
        self.add_sources('crt0', {
            's-macres.adb': 's-macres-p2020.adb',
            's-bbbopa.ads': 's-bbbopa-8641d.ads',
            's-textio.adb': 's-textio-p2020.adb'})
        self.add_sources('gnarl', {
            's-bbbosu.adb': 's-bbbosu-ppc-openpic.adb',
            's-bbsuti.adb': 's-bbsuti-ppc.adb',
            's-bbsumu.adb': 's-bbsumu-8641d.adb',
            's-bbpara.ads': 's-bbpara-8641d.ads',
            'a-intnam.ads': 'a-intnam-xi-ppc-openpic.ads'})
