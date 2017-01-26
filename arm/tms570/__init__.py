from build_rts_support.bsp import BSP
from arm.cortexar import CortexARArch, CortexARTarget


class TMS570(CortexARTarget):
    @property
    def name(self):
        return "tms570"

    @property
    def loaders(self):
        return ('PROBE', 'FLASH', 'MONITOR', 'HIRAM', 'LORAM')

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mbig-endian', '-mhard-float', '-mcpu=cortex-r4',
                '-mfpu=vfpv3-d16', '-marm')

    @property
    def sfp_system_ads(self):
        return 'system-xi-arm-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-arm-full.ads'

    def __init__(self):
        super(TMS570, self).__init__(
            mem_routines=True,
            small_mem=True)

        self.add_linker_script('arm/tms570/common.ld')
        self.add_linker_script('arm/tms570/tms570.ld', loader='PROBE')
        self.add_linker_script('arm/tms570/flash.ld', loader='FLASH')
        self.add_linker_script('arm/tms570/monitor.ld', loader='MONITOR')
        self.add_linker_script('arm/tms570/hiram.ld', loader='HIRAM')
        self.add_linker_script('arm/tms570/loram.ld', loader='LORAM')
        self.add_linker_switch('-Wl,-z,max-page-size=0x1000',
                               loader=['FLASH', 'MONITOR', 'HIRAM', 'LORAM'])

        self.add_sources('crt0', [
            'arm/tms570/sys_startup.S',
            'arm/tms570/crt0.S',
            'arm/tms570/start-ram.S',
            'arm/tms570/start-rom.S',
            {'s-textio.adb': 's-textio-tms570.adb',
             's-macres.adb': 's-macres-tms570.adb'}])
        self.add_sources('gnarl', {
            'a-intnam.ads': 'a-intnam-xi-tms570.ads',
            's-bbpara.ads': 's-bbpara-tms570.ads',
            's-bbbosu.adb': 's-bbbosu-tms570.adb'})
        self.add_sources('gnarl', 's-bbsumu.adb')
