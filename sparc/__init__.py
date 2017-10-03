# BSP support for Sparc/Leon
from support import readfile
from support.bsp import BSP
from support.target import DFBBTarget


class LeonArch(BSP):
    @property
    def name(self):
        return "leon"

    def __init__(self):
        super(LeonArch, self).__init__()
        self.add_linker_switch('-Wl,-u_start', loader=None)
        self.add_sources('arch', [
            'sparc/leon/crt0.S',
            'sparc/leon/hw_init.S',
            'sparc/src/sparc.h',
            'src/s-macres__leon.adb'])
        self.add_sources('gnarl', [
            'src/s-bbcppr__old.ads',
            'src/s-bbcppr__sparc.adb',
            'src/s-bcpith__sparc.adb',
            'src/s-bbcpsp__leon.ads',
            'sparc/src/context_switch.S',
            'sparc/src/trap_handler.S',
            'sparc/src/interrupt_masking.S',
            'sparc/src/floating_point.S',
            'src/s-bbcaco.ads',
            'src/s-bbcaco__leon.adb',
            'src/s-bbinte__generic.adb'])


class LeonTarget(DFBBTarget):
    @property
    def zfp_system_ads(self):
        return 'system-xi-sparc.ads'

    @property
    def sfp_system_ads(self):
        return 'system-xi-sparc-ravenscar.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-sparc-full.ads'

    def __init__(self):
        super(LeonTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_rts(self, rts_profile, conf):
        super(LeonTarget, self).amend_rts(rts_profile, conf)
        conf.rts_xml = \
            conf.rts_xml.replace(
                ' "-nolibc",', '')
        if rts_profile == 'ravenscar-full':
            # Use leon-zcx.specs to link with -lc.
            conf.config_files.update(
                {'link-zcx.spec': readfile('sparc/leon/leon-zcx.specs')})
            conf.rts_xml = conf.rts_xml.replace(
                '"-nostartfiles",',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",')


class Leon2(LeonTarget):
    @property
    def name(self):
        return "leon"

    @property
    def target(self):
        return 'leon-elf'

    @property
    def parent(self):
        return LeonArch

    @property
    def c_switches(self):
        # The required compiler switches
        return ('-DLEON', '-DLEON2')

    def __init__(self):
        super(Leon2, self).__init__()

        self.add_linker_script('sparc/leon/leon.ld', loader=None)
        self.add_sources('crt0', [
            'src/s-textio__leon.adb',
            'src/s-bbbopa__leon.ads'])
        self.add_sources('gnarl', [
            'src/s-bbsumu__generic.adb',
            'src/s-bbsule__leon.ads',
            'src/s-bbbosu__leon.adb',
            'src/s-bbpara__leon.ads',
            'src/a-intnam__leon.ads'])


class Leon3(LeonTarget):
    @property
    def name(self):
        if self.smp:
            return "leon3-smp"
        else:
            return "leon3"

    @property
    def target(self):
        return 'leon3-elf'

    @property
    def parent(self):
        return LeonArch

    @property
    def zfp_system_ads(self):
        if self.smp:
            # zfp runtime makes no sense in the context of SMP variant
            return None
        else:
            return 'system-xi-sparc.ads'

    @property
    def need_fix_ut699(self):
        return True

    @property
    def c_switches(self):
        # The required compiler switches
        res = ('-DLEON', '-DLEON3')
        if self.need_fix_ut699:
            res += ('-DFIX_UT699',)
        return res

    @property
    def compiler_switches(self):
        if self.need_fix_ut699:
            return ('-mfix-ut699',)
        return ()

    @property
    def has_single_precision_fpu(self):
        # Single precision sqrt is buggy on UT699
        return not self.need_fix_ut699

    @property
    def readme_file(self):
        return 'sparc/leon3/README'

    def __init__(self, smp):
        self.smp = smp
        super(Leon3, self).__init__()

        self.add_linker_script('sparc/leon3/leon.ld', loader=None)
        self.add_sources('crt0', [
            'src/s-textio__leon3.adb',
            'src/s-bbbopa__leon3-%s.ads' % ('smp' if smp else 'up', )])
        self.add_sources('gnat', [
            'src/i-leon3.ads',
            'src/i-leon3-uart.ads',
            'src/i-leon3-cache.ads'])
        self.add_sources('gnarl', [
            'src/i-leon3-timers.ads',
            'src/i-leon3-irqmp.ads',
            'src/s-bbbosu__leon3.adb',
            'src/s-bbpara__leon.ads',
            'src/a-intnam__leon3.ads'])


class Leon4(Leon3):
    @property
    def name(self):
        if self.smp:
            return "leon4-smp"
        else:
            return "leon4"

    def __init__(self, smp):
        super(Leon4, self).__init__(smp)
        if smp:
            self.update_pair('s-bbbopa.ads', 'src/s-bbbopa__leon4-smp.ads')
        else:
            self.update_pair('s-bbbopa.ads', 'src/s-bbbopa__leon4-up.ads')
