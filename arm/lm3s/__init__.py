from build_rts_support.bsp import BSP
from arm.cortexm import CortexMArch, CortexMTarget


class _LM3SBSP(BSP):
    @property
    def name(self):
        return 'lm3s'

    @property
    def parent(self):
        return CortexMArch

    @property
    def loaders(self):
        return ('ROM', 'RAM', 'USER')

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m3')

    def __init__(self):
        super(_LM3SBSP, self).__init__()

        self.add_linker_script('arm/lm3s/lm3s-rom.ld', 'ROM')
        self.add_linker_script('arm/lm3s/lm3s-ram.ld', 'RAM')
        self.add_sources('crt0', [
            'arm/lm3s/start-rom.S',
            'arm/lm3s/start-ram.S',
            'arm/lm3s/setup_pll.adb',
            'arm/lm3s/setup_pll.ads',
            {'s-textio.adb': 's-textio-lm3s.adb'}])


class LM3S(CortexMTarget):
    @property
    def bspclass(self):
        return _LM3SBSP

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True
