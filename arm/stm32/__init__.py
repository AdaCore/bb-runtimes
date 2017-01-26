from build_rts_support.bsp import BSP
from arm.cortexm import CortexMArch, CortexMTarget


class Stm32CommonBSP(BSP):
    """Holds sources common to all stm32 boards"""
    @property
    def name(self):
        return 'stm32'

    @property
    def parent(self):
        return CortexMArch

    @property
    def loaders(self):
        return ('ROM', 'RAM', 'USER')

    def __init__(self):
        super(Stm32CommonBSP, self).__init__()

        self.add_linker_script('arm/stm32/common-RAM.ld', loader='RAM')
        self.add_linker_script('arm/stm32/common-ROM.ld', loader='ROM')

        self.add_sources('crt0', [
            {'s-bbpara.ads': 's-bbpara-stm32f4.ads'},
            's-stm32.ads',
            'arm/stm32/start-rom.S',
            'arm/stm32/start-ram.S',
            'arm/stm32/start-common.S',
            'arm/stm32/setup_pll.adb',
            'arm/stm32/setup_pll.ads'])


class Stm32(CortexMTarget):
    """Generic handling of stm32 boards"""
    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return Stm32CommonBSP

    @property
    def has_double_precision_fpu(self):
        if self.mcu == 'stm32f7x9':
            return True
        else:
            return False

    @property
    def cortex(self):
        if self.mcu.startswith('stm32f4'):
            return 'cortex-m4'
        elif self.mcu.startswith('stm32f7'):
            return 'cortex-m7'
        else:
            assert False, "Unexpected MCU %s" % self.mcu

    @property
    def fpu(self):
        if self.cortex == 'cortex-m4':
            return 'fpv4-sp-d16'
        elif not self.has_double_precision_fpu:
            return 'fpv5-sp-d16'
        else:
            return 'fpv5-d16'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mhard-float',
                '-mcpu=%s' % self.cortex,
                '-mfpu=%s' % self.fpu,
                '-mthumb')

    def __init__(self, board):
        self.board = board
        if self.board == 'stm32f4':
            self.mcu = 'stm32f40x'
        elif self.board == 'stm32f429disco':
            self.mcu = 'stm32f429x'
        elif self.board == 'openmv2':
            self.mcu = 'stm32f429x'
        elif self.board == 'stm32f469disco':
            self.mcu = 'stm32f469x'
        elif self.board == 'stm32f746disco':
            self.mcu = 'stm32f7x'
        elif self.board == 'stm32f769disco':
            self.mcu = 'stm32f7x9'
        else:
            assert False, "Unknown stm32 board: %s" % self.board

        super(Stm32, self).__init__()

        self.add_linker_script('arm/stm32/%s/memory-map.ld' % self.mcu,
                               loader=('RAM', 'ROM'))
        # startup code
        self.add_sources('crt0', [
            'arm/stm32/%s/s-bbbopa.ads' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.ads' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.adb' % self.mcu,
            'arm/stm32/%s/svd/i-stm32.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-flash.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-gpio.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-pwr.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-rcc.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-syscfg.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-usart.ads' % self.mcu])

        if self.board == 'stm32f4':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f40x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.board == 'stm32f429disco':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
        elif self.board == 'openmv2':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f4.adb'})
            self.update_pairs('crt0', {
                's-bbbopa.ads': 'arm/stm32/%s/s-bbbopa-openmv2.ads' %
                                self.mcu})
        elif self.board == 'stm32f469disco':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f4x9x.adb',
                's-textio.adb': 's-textio-stm32f469.adb'})
        elif self.board == 'stm32f746disco':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f7x.adb',
                's-textio.adb': 's-textio-stm32f7.adb'})
        elif self.board == 'stm32f769disco':
            self.add_sources('crt0', {
                's-stm32.adb': 's-stm32-f7x.adb',
                's-textio.adb': 's-textio-stm32f7.adb'})

        # ravenscar support
        self.add_sources('gnarl', [
            'arm/stm32/%s/svd/handler.S' % self.mcu,
            'arm/stm32/%s/svd/a-intnam.ads' % self.mcu])
