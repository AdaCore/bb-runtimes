from build_rts_support.bsp import BSP
from arm.cortexm import CortexMArch, CortexMTarget


class Sf2BSP(BSP):
    @property
    def name(self):
        return 'smartfusion2'

    @property
    def parent(self):
        return CortexMArch

    @property
    def loaders(self):
        return ('ROM', 'USER')

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m3')

    def __init__(self):
        super(Sf2BSP, self).__init__()

        self.add_linker_script('arm/smartfusion2/common-ROM.ld', loader='ROM')
        self.add_linker_script('arm/smartfusion2/memory-map.ld', loader='ROM')

        self.add_sources('crt0', [
            'arm/smartfusion2/start-rom.S',
            'arm/smartfusion2/setup_pll.adb',
            'arm/smartfusion2/setup_pll.ads',
            'arm/smartfusion2/s-sf2.ads',
            'arm/smartfusion2/s-sf2.adb',
            'arm/smartfusion2/s-sf2gpi.ads',
            'arm/smartfusion2/s-sf2gpi.adb',
            'arm/smartfusion2/s-sf2uar.ads',
            'arm/smartfusion2/s-sf2uar.adb',
            'arm/smartfusion2/svd/i-sf2.ads',
            'arm/smartfusion2/svd/i-sf2-system_registers.ads',
            'arm/smartfusion2/svd/i-sf2-mmuart.ads',
            'arm/smartfusion2/svd/i-sf2-gpio.ads',
            {'s-textio.adb': 'arm/smartfusion2/s-textio.adb'}])
        self.add_sources('gnarl', [
            'arm/smartfusion2/svd/handler.S',
            {'s-bbpara.ads': 's-bbpara-smartfusion2.ads',
             'a-intnam.ads': 'arm/smartfusion2/svd/a-intnam.ads'}])


class SmartFusion2(CortexMTarget):
    @property
    def bspclass(self):
        return Sf2BSP

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True
