# This module contains cortex-m bsp support
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import Target

import re


class CortexMArch(ArchSupport):
    @property
    def name(self):
        return "cortex-m"

    def __init__(self):
        super(CortexMArch, self).__init__()
        self.add_gnat_sources(
            'src/s-macres__cortexm3.adb',
            'arm/src/breakpoint_handler-cortexm.S')
        self.add_gnarl_sources(
            'src/s-bbcpsp__cortexm.ads',
            'src/s-bbcppr__old.ads',
            'src/s-bbcppr__armv7m.adb',
            'src/s-bbinte__generic.adb',
            'src/s-bbsumu__generic.adb',
            'src/s-bcpcst__armvXm.ads')


class ArmV7MArch(ArchSupport):
    @property
    def name(self):
        return "armv7-m"

    @property
    def parent(self):
        return CortexMArch

    def __init__(self):
        super(ArmV7MArch, self).__init__()
        self.add_gnarl_sources(
            'src/s-bbbosu__armv7m.adb',
            'src/s-bcpcst__pendsv.adb')


class ArmV6MTarget(Target):
    @property
    def target(self):
        return "arm-eabi"

    @property
    def parent(self):
        return CortexMArch

    @property
    def has_timer_64(self):
        return False

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_double_precision_fpu(self):
        return False

    @property
    def has_small_memory(self):
        return True

    def __init__(self):
        super(ArmV6MTarget, self).__init__()

        self.add_gnat_sources(
            'src/s-bbarat.ads',
            'src/s-bbarat.adb')


class ArmV7MTarget(ArmV6MTarget):
    @property
    def parent(self):
        return ArmV7MArch

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'system-xi-cortexm4-sfp.ads',
                'ravenscar-full': 'system-xi-cortexm4-full.ads'}

    def __init__(self):
        super(ArmV7MTarget, self).__init__()


class LM3S(ArmV7MTarget):
    @property
    def name(self):
        return 'lm3s'

    @property
    def loaders(self):
        return ('ROM', 'RAM')

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m3')

    @property
    def readme_file(self):
        return 'arm/lm3s/README'

    @property
    def system_ads(self):
        # Only zfp supported here
        ret = super(LM3S, self).system_ads
        return {'zfp': ret['zfp']}

    def __init__(self):
        super(LM3S, self).__init__()

        self.add_linker_script('arm/lm3s/lm3s-rom.ld', loader='ROM')
        self.add_linker_script('arm/lm3s/lm3s-ram.ld', loader='RAM')
        self.add_gnat_sources(
            'arm/lm3s/start-rom.S',
            'arm/lm3s/start-ram.S',
            'arm/lm3s/setup_pll.adb',
            'arm/lm3s/setup_pll.ads',
            'src/s-textio__lm3s.adb')


class SamCommonArchSupport(ArchSupport):
    @property
    def name(self):
        return 'sam'

    @property
    def parent(self):
        return ArmV7MArch

    @property
    def loaders(self):
        return ('ROM', 'SAMBA', 'RAM')

    def __init__(self):
        super(SamCommonArchSupport, self).__init__()

        self.add_linker_script('arm/sam/common-SAMBA.ld', loader='SAMBA')
        self.add_linker_script('arm/sam/common-ROM.ld', loader='ROM')
        self.add_linker_script('arm/sam/common-RAM.ld', loader='RAM')

        self.add_gnat_sources(
            'arm/sam/start-rom.S',
            'arm/sam/start-ram.S',
            'arm/sam/start-common.S',
            'arm/sam/setup_pll.ads')


class Sam(ArmV7MTarget):
    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return SamCommonArchSupport

    @property
    def use_semihosting_io(self):
        return True

    @property
    def has_single_precision_fpu(self):
        if self.board == 'sam4s':
            return False
        else:
            return True

    @property
    def has_double_precision_fpu(self):
        if self.board == 'samv71':
            return True
        else:
            return False

    @property
    def cortex(self):
        if self.board == 'samv71':
            return 'cortex-m7'
        else:
            return 'cortex-m4'

    @property
    def fpu(self):
        if self.cortex == 'cortex-m4':
            return 'fpv4-sp-d16'
        elif not self.has_double_precision_fpu:
            return 'fpv5-sp-d16'
        else:
            return 'fpv5-d16'

    @property
    def system_ads(self):
        ret = super(Sam, self).system_ads

        if self.board == 'samv71':
            return ret
        else:
            # No runtime full
            return {'zfp': ret['zfp'],
                    'ravenscar-sfp': ret['ravenscar-sfp']}

    @property
    def compiler_switches(self):
        base = ('-mlittle-endian', '-mthumb', '-mcpu=%s' % self.cortex)

        if not self.has_single_precision_fpu:
            return base
        else:
            return base + ('-mhard-float', '-mfpu=%s' % self.fpu, )

    def __init__(self, board):
        assert board in ('sam4s', 'samg55', 'samv71'), \
            "Unexpected SAM board %s" % board
        self.board = board
        super(Sam, self).__init__()

        self.add_linker_script(
            'arm/sam/%s/memory-map.ld' % self.name)

        self.add_gnat_sources(
            'arm/sam/%s/setup_pll.adb' % self.name,
            'arm/sam/%s/s-bbbopa.ads' % self.name,
            'arm/sam/%s/svd/i-sam.ads' % self.name,
            'arm/sam/%s/svd/i-sam-efc.ads' % self.name,
            'arm/sam/%s/svd/i-sam-pmc.ads' % self.name)

        # ravenscar support
        self.add_gnarl_sources(
            'arm/sam/%s/svd/handler.S' % self.name,
            'arm/sam/%s/s-bbmcpa.ads' % self.name,
            'arm/sam/%s/svd/a-intnam.ads' % self.name)

        if self.board == 'samv71':
            self.add_gnat_sources(
                'arm/sam/samv71/s-samv71.ads',
                'arm/sam/%s/svd/i-sam-pio.ads' % self.name,
                'arm/sam/%s/svd/i-sam-uart.ads' % self.name)
            self.add_gnarl_sources(
                'src/s-bbpara__samv71.ads')
        else:
            self.add_gnat_sources(
                'arm/sam/sam4s/s-sam4s.ads',
                'arm/sam/%s/board_config.ads' % self.name,
                'arm/sam/%s/svd/i-sam-sysc.ads' % self.name)
            self.add_gnarl_sources(
                'src/s-bbpara__sam4s.ads')


class SmartFusion2(ArmV7MTarget):
    @property
    def name(self):
        return 'smartfusion2'

    @property
    def loaders(self):
        return ('ROM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m3')

    @property
    def has_single_precision_fpu(self):
        return False

    @property
    def has_fpu(self):
        # Still add floating point attributes
        return True

    @property
    def system_ads(self):
        # no zfp nor ravenscar-full rts
        ret = super(SmartFusion2, self).system_ads
        return {'ravenscar-sfp': ret['ravenscar-sfp']}

    def __init__(self):
        super(SmartFusion2, self).__init__()

        self.add_linker_script('arm/smartfusion2/common-ROM.ld', loader='ROM')
        self.add_linker_script('arm/smartfusion2/memory-map.ld', loader='ROM')

        self.add_gnat_sources(
            'arm/smartfusion2/start-rom.S',
            'arm/smartfusion2/setup_pll.adb',
            'arm/smartfusion2/setup_pll.ads',
            'arm/smartfusion2/s-sf2.ads',
            'arm/smartfusion2/s-sf2.adb',
            'arm/smartfusion2/s-sf2uar.ads',
            'arm/smartfusion2/s-sf2uar.adb',
            'arm/smartfusion2/svd/i-sf2.ads',
            'arm/smartfusion2/svd/i-sf2-system_registers.ads',
            'arm/smartfusion2/svd/i-sf2-mmuart.ads',
            'arm/smartfusion2/svd/i-sf2-gpio.ads',
            'arm/smartfusion2/s-textio.adb')
        self.add_gnarl_sources(
            'arm/smartfusion2/s-sf2gpi.ads',
            'arm/smartfusion2/s-sf2gpi.adb',
            'arm/smartfusion2/svd/handler.S',
            'arm/smartfusion2/svd/a-intnam.ads',
            'src/s-bbpara__smartfusion2.ads')


class CortexM0CommonArchSupport(ArmV6MTarget):
    @property
    def name(self):
        return 'cortex-m0'

    @property
    def loaders(self):
        return ('ROM', 'RAM')

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m0')

    @property
    def has_fpu(self):
        # We require floating point attributes as soft-float is supported
        return True

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'system-xi-armv6m-sfp.ads',
                'ravenscar-full': 'system-xi-armv6m-full.ads'}

    def __init__(self):
        super(CortexM0CommonArchSupport, self).__init__()

        self.add_gnat_sources('src/s-bbpara__cortexm0.ads')

        self.add_gnarl_sources(
            'src/s-bbbosu__armv6m.adb',
            'src/s-bcpcst__pendsv.adb')


class Stm32F0(CortexM0CommonArchSupport):

    # Flash memory size is determined by the "User code memory size"
    # part of the device name
    flash_sizes = {
        '4': 16,
        '6': 32,
        '8': 64,
        'b': 128,
        'c': 256
    }

    # RAM size for STM32F03x is determined by the
    # device package (pin count) and user code memory size
    # (i.e. the last two characters of the device name, e.g. stm32f030f4)
    f03x_ram_sizes = {  # STM32F03xxx
        'c4': 4,
        'c6': 4,
        'c8': 8,
        'cc': 32,
        'e6': 4,
        'f4': 4,
        'f6': 4,
        'g4': 4,
        'g6': 4,
        'k4': 4,
        'k6': 4,
        'r8': 8,
        'rc': 32,
    }

    # RAM size for STM32F07x is also determined by the
    # device package (pin count) and user code memory size.
    f07x_ram_sizes = {
        'c6': 6,
        'c8': 16,
        'cb': 16,
        'f6': 6,
        'rb': 16,
        'v8': 16,
        'vb': 16,
    }

    @property
    def name(self):
        return self.board

    @property
    def use_semihosting_io(self):
        return True

    @property
    def loaders(self):
        return ('ROM', 'RAM')

    def __init__(self, board_name, base_target, board_params):
        super(Stm32F0, self).__init__()

        # Determine MCU features from target name (e.g. 'stm32f071rb')
        m = re.match(r'.*f0([34579])([0128])([cefgkrv])([468bc])',
                     base_target)
        if m is None:
            raise RuntimeError("Unknown STM32F0 target: " + base_target)
        sub_family_major = m.group(1)
        sub_family_minor = m.group(2)
        package = m.group(3)
        user_code_memory_size = m.group(4)

        self.board = board_name

        # Determine RAM size from sub-family, package, and user code mem. size
        if sub_family_major == '3':
            ram_size = self.f03x_ram_sizes[package + user_code_memory_size]
        elif sub_family_major in '45':
            ram_size = 4  # All STM32F04x/STM32F05x devices have 4K RAM
        elif sub_family_major == '7':
            ram_size = self.f07x_ram_sizes[package + user_code_memory_size]
        else:
            ram_size = 32  # All STM32F09x devices have 32k RAM

        flash_size = self.flash_sizes[user_code_memory_size]

        self.add_linker_script('arm/stm32/stm32f0xx/common-RAM.ld',
                               loader='RAM')
        self.add_linker_script('arm/stm32/stm32f0xx/common-ROM.ld',
                               loader='ROM')

        # Select memory map based on available memory size
        # of the specific device
        self.add_linker_script('arm/stm32/stm32f0xx/memory-map.ld.tmpl')

        # Set template variables required by linker script
        self.add_template_config_value(
            'STM32_Linker_Flash_Size',
            '{}K'.format(flash_size))
        self.add_template_config_value(
            'STM32_Linker_RAM_Size',
            '{}K'.format(ram_size))

        # Common source files
        self.add_gnat_sources(
            'arm/stm32/stm32f0xx/s-stm32.ads',
            'arm/stm32/stm32f0xx/s-stm32.adb',
            'arm/stm32/stm32f0xx/start-rom.S',
            'arm/stm32/stm32f0xx/start-ram.S',
            'arm/stm32/stm32f0xx/setup_pll.ads',
            'arm/stm32/stm32f0xx/setup_pll.adb',
            'arm/stm32/stm32f0xx/s-bbmcpa.ads.tmpl',
            'arm/stm32/stm32f0xx/s-bbbopa.ads.tmpl',
            ('arm/stm32/stm32f0xx/stm32f0x{}'
             '/svd/i-stm32.ads').format(sub_family_minor),
            ('arm/stm32/stm32f0xx/stm32f0x{}'
             '/svd/i-stm32-flash.ads').format(sub_family_minor),
            ('arm/stm32/stm32f0xx/stm32f0x{}'
             '/svd/i-stm32-rcc.ads').format(sub_family_minor))

        # Configure MCU parameters based on family.
        simple_clock_tree = (sub_family_major in '35')
        self.add_template_config_value('STM32_Simple_Clock_Tree',
                                       str(simple_clock_tree))

        # Determine clock configuration from provided board_parameters.
        clock_configs = self._configure_clocks(board_params,
                                               simple_clock_tree)
        for key, value in clock_configs.items():
            self.add_template_config_value(key, value)

        # Choose interrupt names based on family
        self.add_gnarl_sources(
            ('arm/stm32/stm32f0xx/stm32f0x{}/'
             'svd/a-intnam.ads').format(sub_family_minor))

    def _configure_clocks(self, board_params, simple_clock_tree):
        sysclk_freq = board_params.get('sysclk_frequency', 48000000)
        hse_freq = board_params.get('hse_frequency', 8000000)
        clock_source = board_params.get('clock_source', 'HSE')
        hse_bypass = bool(board_params.get('hse_bypass', False))
        lsi_enabled = bool(board_params.get('lsi_enabled', True))
        apb_prescaler = board_params.get('apb_prescaler', 1)

        assert clock_source in ['HSI', 'HSE', 'HSI48'], \
            'Invalid clock_source: {}'.format(clock_source)

        if simple_clock_tree:
            assert clock_source != 'HSI48', \
                'HSI48 is not available for STM32f03x and STM32F05x devices'

        assert apb_prescaler in [1, 2, 4, 8, 16], \
            'Invalid apb_prescaler: {}'.format(apb_prescaler)

        assert hse_freq in range(4000000, 32000001), \
            'Invalid hse_frequency: {} (must be 4-32 MHz)'.format(hse_freq)

        hsi_freq = 8000000
        pllin_freq = None  # Don't use PLL unless needed below
        pllsrc = 'HSI_2'  # Default, might be overriden below

        # Determine SYSCLK source from the configured clock_source and
        # target sysclk_frequency.
        #
        # Note that the PLL is only used to generate SYSCLK if the
        # target SYSCLK frequency is not exactly equal to the frequency
        # of the clock source. Otherwise, the PLL is not used to save power.

        if clock_source == 'HSE':
            # Only enable PLL if needed to attain target SYSCLK frequency.
            if sysclk_freq == hse_freq:
                sysclk_src = 'HSE'
            else:
                sysclk_src = 'PLL'
                pllsrc = 'HSE_PREDIV'
                pllin_freq = hse_freq

        elif clock_source == 'HSI':
            if sysclk_freq == hsi_freq:
                sysclk_src = 'HSI'

            else:
                sysclk_src = 'PLL'
                if simple_clock_tree:
                    # Fixed /2 divider on F03x and F05x devices
                    pllsrc = 'HSI_2'
                    pllin_freq = hsi_freq / 2
                else:
                    pllsrc = 'HSI_PREDIV'
                    pllin_freq = hsi_freq

        elif clock_source == 'HSI48':
            sysclk_src = 'HSI48'
            assert sysclk_freq == 48000000, \
                'sysclk_frequency must be 48000000 ' \
                'when clock_source is HSI48'

        if pllin_freq is None:
            # PLL not needed. Set defaults
            prediv = 1
            pllmul = 2 # minimum allowed value
        else:
            # PLL needed to obtain target main clock frequency.
            # Brute force all possible PREDIV and PLLMUL values to find the
            # configuration that generates the target frequency.
            pll_configs = []

            for prediv in range(1, 17):
                # PREDIV not available on simple clock tree devices
                if pllsrc != 'HSI_2' or prediv == 1:
                    for pllmul in range(2, 17):
                        pllout_freq = int((pllin_freq / prediv) * pllmul)

                        # Only accept configurations that result in the
                        # target SYSCLK speed.
                        #
                        # Also ensure the PLL output frequency is in the
                        # range 16-48 MHz.
                        if pllout_freq == sysclk_freq and \
                           pllout_freq >= 16000000 and \
                           pllout_freq <= 48000000:
                            pll_configs.append({
                                'PREDIV': 1,
                                'PLLMUL': pllmul,
                                'PLLOUT_FREQ': pllout_freq
                            })

            # Check that there is at least one configuration that generates
            # the requested clock speed. This fails if there is no possible
            # PLL configuration that can generate the requested frequency.
            assert len(pll_configs) > 0, \
                'Cannot generate requested sysclk_frequency of ' + \
                '{} Hz '.format(sysclk_freq) + \
                'from the requested clock_source ({})'.format(clock_source)

            # Use first valid PLL configuration found.
            cfg = pll_configs[0]
            prediv = cfg['PREDIV']
            pllmul = cfg['PLLMUL']
            pllout_freq = cfg['PLLOUT_FREQ']

        # Choose configuration value for STM32_APB_PRE
        if apb_prescaler == 1:
            stm32_apb_pre = 'System.STM32.APBPRE_DIV1'
        else:
            stm32_apb_pre = '(Enabled => True, Value => System.STM32.DIV{})' \
                .format(apb_prescaler)

        # AHB_PRE is fixed to 1 in the current implementation, since
        # the implementation of System.BB.Parameters assumes that the
        # processor frequency (Clock_Frequency) is equal to
        # Main_Clock_Frequency, which is set to SYSCLK.

        return {
            'STM32_Main_Clock_Frequency': str(sysclk_freq),
            'STM32_HSE_Clock_Frequency': str(hse_freq),
            'STM32_HSE_Bypass': str(hse_bypass),
            'STM32_LSI_Enabled': str(lsi_enabled),
            'STM32_PLL_Src': 'System.STM32.PLL_SRC_' + pllsrc,
            'STM32_SYSCLK_Src': 'System.STM32.SYSCLK_SRC_' + sysclk_src,
            'STM32_PREDIV': str(prediv),
            'STM32_PLLMUL_Value': str(pllmul),
            'STM32_AHB_PRE': 'System.STM32.AHBPRE_DIV1',
            'STM32_APB_PRE': stm32_apb_pre
        }


class CortexM1CommonArchSupport(ArmV6MTarget):
    @property
    def name(self):
        return 'microsemim1'

    @property
    def loaders(self):
        return ('TCM', 'RAM', )

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m1')

    @property
    def has_fpu(self):
        # We require floating point attributes as soft-float is supported
        return True

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'system-xi-armv6m-sfp.ads',
                'ravenscar-full': 'system-xi-armv6m-full.ads'}

    def __init__(self):
        super(CortexM1CommonArchSupport, self).__init__()

        self.add_linker_script('arm/cortex-m1/common-RAM.ld',
                               loader='RAM')
        self.add_linker_script('arm/cortex-m1/common-TCM.ld',
                               loader='TCM')

        self.add_gnat_sources(
            'arm/cortex-m1/start-tcm.S',
            'arm/cortex-m1/start-ram.S')

        self.add_gnarl_sources(
            'src/s-bbpara__cortexm1.ads',
            'src/s-bbbosu__armv6m.adb',
            'src/s-bcpcst__pendsv.adb')


class MicrosemiM1(CortexM1CommonArchSupport):
    @property
    def name(self):
        return 'microsemi-m1'

    def __init__(self):
        super(MicrosemiM1, self).__init__()

        self.add_linker_script('arm/cortex-m1/microsemi/memory-map.ld')

        self.add_gnat_sources(
            'arm/cortex-m1/microsemi/s-bbbopa.ads',
            'arm/cortex-m1/microsemi/s-bbmcpa.ads',
            'arm/cortex-m1/microsemi/s-textio.adb',
            'arm/cortex-m1/microsemi/svd/i-microsemi.ads',
            'arm/cortex-m1/microsemi/svd/i-microsemi-coreuartapb.ads')

        self.add_gnarl_sources(
            'arm/cortex-m1/microsemi/a-intnam.ads')


class NRF51(ArmV6MTarget):
    @property
    def name(self):
        return 'nRF51'

    @property
    def loaders(self):
        return ('ROM', )

    @property
    def has_fpu(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m0')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'system-xi-armv6m-sfp.ads',
                'ravenscar-full': 'system-xi-armv6m-full.ads'}

    def __init__(self):
        super(NRF51, self).__init__()

        self.add_linker_script('arm/nordic/nrf51/common-ROM.ld', loader='ROM')

        self.add_gnat_sources(
            'arm/nordic/nrf51/svd/i-nrf51.ads',
            'arm/nordic/nrf51/svd/i-nrf51-clock.ads',
            'arm/nordic/nrf51/svd/i-nrf51-rtc.ads',
            'arm/nordic/nrf51/svd/i-nrf51-uart.ads',
            'arm/nordic/nrf51/svd/handler.S',
            'arm/nordic/nrf51/start-rom.S',
            'arm/nordic/nrf51/s-bbmcpa.ads')

        self.add_gnarl_sources(
            'arm/nordic/nrf51/svd/a-intnam.ads',
            'src/s-bbpara__nrf51.ads',
            'src/s-bbbosu__nrf51.adb',
            'src/s-bcpcst__pendsv.adb')


class Microbit(NRF51):
    @property
    def name(self):
        return 'microbit'

    @property
    def use_semihosting_io(self):
        return False

    def __init__(self):
        super(Microbit, self).__init__()

        self.add_linker_script('arm/nordic/nrf51/memory-map_nRF51822xxAA.ld',
                               'memory-map.ld')

        self.add_gnat_sources(
            'arm/nordic/nrf51/s-bbbopa__microbit.ads',
            'src/s-textio__microbit.adb')


class NRF52(ArmV7MTarget):
    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return CortexMArch

    @property
    def loaders(self):
        return ('ROM', )

    @property
    def has_fpu(self):
        return True

    @property
    def system_ads(self):
        # Use custom System package since system-xi-cortexm4 assumes
        # 4-bit interrupt priorities, but the nRF52 only supports
        # 3-bit interrupt priorities. This requires different
        # definitions for Priority and Interrupt_Priority in System.
        return {'zfp': 'system-xi-arm.ads',
                'ravenscar-sfp': 'arm/nordic/nrf52/system-xi-nrf52-sfp.ads',
                'ravenscar-full': 'arm/nordic/nrf52/system-xi-nrf52-full.ads'}

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-mhard-float',
                '-mfpu=fpv4-sp-d16', '-mcpu=cortex-m4')

    def __init__(self, board_name, board_params):
        super(NRF52, self).__init__()

        self.board = board_name

        self.add_linker_script('arm/nordic/nrf52/common-ROM.ld', loader='ROM')
        self.add_linker_script('arm/nordic/nrf52/memory-map_%s.ld' % self.name,
                               'memory-map.ld')

        self.add_gnat_sources(
            'arm/nordic/nrf52/s-bbmcpa.ads',
            'arm/nordic/nrf52/start-common.S',
            'arm/nordic/nrf52/start-rom.S',
            'arm/nordic/nrf52/setup_board.ads')

        self.add_gnarl_sources(
            'src/s-bbpara__nrf52.ads',
            'src/s-bbbosu__nrf52.adb.tmpl',
            'src/s-bcpcst__pendsv.adb')

        use_hfxo = board_params.get('use_hfxo', False)
        lfclk_src = board_params.get('use_hfxo', 'Xtal')
        use_swo_trace = board_params.get('use_swo_trace', True)
        use_reset_pin = board_params.get('use_reset_pin', True)
        alarm_rtc_periph = board_params.get('alarm_rtc_periph', 'RTC0')

        assert type(use_hfxo) == bool, "use_hfxo must be of type bool"

        assert type(use_swo_trace) == bool, \
            "use_swo_trace must be of type bool"

        assert type(use_reset_pin) == bool, \
            "use_reset_pin must be of type bool"

        assert lfclk_src in ["Xtal", "Rc", "Synth"], \
            "Invalid value for lfclk_src." \
            "Valid values are: 'Xtal', 'Rc', 'Synth'"

        assert alarm_rtc_periph in ["RTC0", "RTC1", "RTC2"], \
            "Invalid value for alarm_rtc_periph." \
            "Valid values are: 'RTC0', 'RTC1', 'RTC2'"

        self.add_template_config_value("NRF52_Use_HFXO",
                                       str(use_hfxo))
        self.add_template_config_value("NRF52_LFCLK_Src",
                                       lfclk_src)
        self.add_template_config_value("NRF52_Use_SWO_Trace",
                                       str(use_swo_trace))
        self.add_template_config_value("NRF52_Use_Reset_Pin",
                                       str(use_reset_pin))
        self.add_template_config_value("NRF52_Alarm_RTC_Periph",
                                       str(alarm_rtc_periph))

        # s-bbbosu.adb cannot depend on Ada.Interrupt.Names to get the
        # selected RTC peripheral's Interrupt_ID, so it is passed in
        # as a template parameter.
        alarm_interrupt_ids = {
            "RTC0": "11",
            "RTC1": "17",
            "RTC2": "36"
        }
        self.add_template_config_value("NRF52_Alarm_Interrupt_ID",
                                       alarm_interrupt_ids[alarm_rtc_periph])


class NRF52840(NRF52):
    @property
    def name(self):
        return 'nrf52840'

    @property
    def use_semihosting_io(self):
        return True

    def __init__(self, board_name, board_params):
        super(NRF52840, self).__init__(board_name, board_params)

        self.add_gnat_sources(
            'arm/nordic/nrf52/nrf52840/s-bbbopa.ads',
            'arm/nordic/nrf52/nrf52840/setup_board.adb.tmpl',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-ccm.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-clock.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-ficr.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-gpio.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-uicr.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-nvmc.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-rtc.ads',
            'arm/nordic/nrf52/nrf52840/svd/i-nrf52-temp.ads')
        self.add_gnarl_sources(
            'arm/nordic/nrf52/nrf52840/svd/handler.S',
            'arm/nordic/nrf52/nrf52840/svd/a-intnam.ads')


class NRF52832(NRF52):
    @property
    def name(self):
        return 'nrf52832'

    @property
    def use_semihosting_io(self):
        return True

    def __init__(self, board_name, board_params):
        super(NRF52832, self).__init__(board_name, board_params)

        self.add_gnat_sources(
            'arm/nordic/nrf52/nrf52832/s-bbbopa.ads',
            'arm/nordic/nrf52/nrf52832/setup_board.adb.tmpl',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-clock.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-ficr.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-gpio.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-uicr.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-nvmc.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-rtc.ads',
            'arm/nordic/nrf52/nrf52832/svd/i-nrf52-temp.ads')

        self.add_gnarl_sources(
            'arm/nordic/nrf52/nrf52832/svd/handler.S',
            'arm/nordic/nrf52/nrf52832/svd/a-intnam.ads')


class Stm32CommonArchSupport(ArchSupport):
    """Holds sources common to all stm32 boards"""
    @property
    def name(self):
        return 'stm32'

    @property
    def parent(self):
        return ArmV7MArch

    @property
    def loaders(self):
        return ('ROM', 'RAM')

    @property
    def readme_file(self):
        return 'arm/stm32/README'

    def __init__(self):
        super(Stm32CommonArchSupport, self).__init__()

        self.add_linker_script('arm/stm32/common-RAM.ld', loader='RAM')
        self.add_linker_script('arm/stm32/common-ROM.ld', loader='ROM')

        self.add_gnat_sources(
            'src/s-bbpara__stm32f4.ads',
            'arm/stm32/s-stm32.ads',
            'arm/stm32/start-rom.S',
            'arm/stm32/start-ram.S',
            'arm/stm32/start-common.S',
            'arm/stm32/setup_pll.adb',
            'arm/stm32/setup_pll.ads')


stm32_board_configuration = {
    'stm32f4':           {'STM32_Main_Clock_Frequency': '168_000_000',
                          'STM32_HSE_Clock_Frequency': '8_000_000',
                          'STM32_FLASH_Latency': '5'},

    'nucleo_f401re':     {'STM32_Main_Clock_Frequency': '168_000_000',
                          'STM32_HSE_Clock_Frequency': '8_000_000',
                          'STM32_FLASH_Latency': '5'},

    'feather_stm32f405': {'STM32_Main_Clock_Frequency': '168_000_000',
                          'STM32_HSE_Clock_Frequency': '12_000_000',
                          'STM32_FLASH_Latency': '5'},

    'stm32f429disco':    {'STM32_Main_Clock_Frequency': '180_000_000',
                          'STM32_HSE_Clock_Frequency': '8_000_000',
                          'STM32_FLASH_Latency': '5'},

    'openmv2':           {'STM32_Main_Clock_Frequency': '180_000_000',
                          'STM32_HSE_Clock_Frequency': '12_000_000',
                          'STM32_FLASH_Latency': '5'},

    'stm32f469disco':    {'STM32_Main_Clock_Frequency': '180_000_000',
                          'STM32_HSE_Clock_Frequency': '8_000_000',
                          'STM32_FLASH_Latency': '5'},

    'stm32f746disco':    {'STM32_Main_Clock_Frequency': '200_000_000',
                          'STM32_HSE_Clock_Frequency': '25_000_000',
                          'STM32_FLASH_Latency': '5'},

    'stm32756geval':     {'STM32_Main_Clock_Frequency': '180_000_000',
                          'STM32_HSE_Clock_Frequency': '25_000_000',
                          'STM32_FLASH_Latency': '5'},

    'stm32f769disco':    {'STM32_Main_Clock_Frequency': '200_000_000',
                          'STM32_HSE_Clock_Frequency': '25_000_000',
                          'STM32_FLASH_Latency': '6'},
    }


class Stm32(ArmV7MTarget):
    """Generic handling of stm32 boards"""
    @property
    def name(self):
        return self.board

    @property
    def parent(self):
        return Stm32CommonArchSupport

    @property
    def use_semihosting_io(self):
        return True

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
        if self.board in ['stm32f4', 'feather_stm32f405']:
            self.mcu = 'stm32f40x'
        elif self.board in ['nucleo_f401re']:
            self.mcu = 'stm32f401'
        elif self.board in ['stm32f429disco', 'openmv2']:
            self.mcu = 'stm32f429x'
        elif self.board in ['stm32f469disco']:
            self.mcu = 'stm32f469x'
        elif self.board in ['stm32f746disco', 'stm32756geval']:
            self.mcu = 'stm32f7x'
        elif self.board in ['stm32f769disco']:
            self.mcu = 'stm32f7x9'
        else:
            assert False, "Unknown stm32 board: %s" % self.board

        super(Stm32, self).__init__()

        # Add board template configuration
        for key, value in stm32_board_configuration[self.board].items():
            self.add_template_config_value(key, value)

        self.add_template_config_value('Board_Name', self.board)
        self.add_template_config_value('MCU_Name', self.mcu)

        self.add_linker_script('arm/stm32/%s/memory-map.ld' % self.mcu)

        # startup code
        self.add_gnat_sources(
            'arm/stm32/%s/s-bbmcpa.ads' % self.mcu,
            'arm/stm32/%s/s-bbmcpa.adb' % self.mcu,
            'arm/stm32/%s/svd/i-stm32.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-flash.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-gpio.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-pwr.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-rcc.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-syscfg.ads' % self.mcu,
            'arm/stm32/%s/svd/i-stm32-usart.ads' % self.mcu)

        self.add_gnat_source('arm/stm32/s-bbbopa.ads.tmpl')

        if self.mcu in ['stm32f40x']:
            self.add_gnat_source('arm/stm32/stm32f40x/s-stm32.adb')

        elif self.mcu in ['stm32f401']:
            self.add_gnat_source('arm/stm32/stm32f401/s-stm32.adb')

        elif self.mcu in ['stm32f429x',
                          'stm32f469x']:
            self.add_gnat_source('arm/stm32/stm32f429x/s-stm32.adb')

        elif self.mcu in ['stm32f7x',
                          'stm32f7x9']:
            self.add_gnat_source('arm/stm32/stm32f7x/s-stm32.adb')

        # ravenscar support
        self.add_gnarl_sources(
            'arm/stm32/%s/svd/handler.S' % self.mcu,
            'arm/stm32/%s/svd/a-intnam.ads' % self.mcu)


class CortexM0(ArmV6MTarget):
    @property
    def name(self):
        return 'cortex-m0'

    @property
    def has_fpu(self):
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m0')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads'}


class CortexM0P(CortexM0):
    @property
    def name(self):
        return 'cortex-m0p'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m0plus')


class CortexM1(ArmV6MTarget):
    @property
    def name(self):
        return 'cortex-m1'

    @property
    def has_fpu(self):
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m1')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads'}


class CortexM3(ArmV7MTarget):
    @property
    def name(self):
        return 'cortex-m3'

    @property
    def has_fpu(self):
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m3')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads'}


class CortexM4(ArmV7MTarget):
    @property
    def name(self):
        return 'cortex-m4'

    @property
    def has_fpu(self):
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-msoft-float',
                '-mcpu=cortex-m4')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads'}


class CortexM4F(CortexM4):
    @property
    def name(self):
        return 'cortex-m4f'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-mhard-float',
                '-mcpu=cortex-m4', '-mfpu=fpv4-sp-d16')


class CortexM7F(ArmV7MTarget):
    @property
    def name(self):
        return 'cortex-m7f'

    @property
    def has_fpu(self):
        return True

    @property
    def use_semihosting_io(self):
        return True

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-mhard-float',
                '-mcpu=cortex-m7', '-mfpu=fpv5-sp-d16')

    @property
    def system_ads(self):
        return {'zfp': 'system-xi-arm.ads'}


class CortexM7DF(CortexM7F):
    @property
    def name(self):
        return 'cortex-m7df'

    @property
    def compiler_switches(self):
        # The required compiler switches
        return ('-mlittle-endian', '-mthumb', '-mhard-float',
                '-mcpu=cortex-m7', '-mfpu=fpv5-d16')
