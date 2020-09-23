# BSP support for Sparc/Leon
from support import readfile
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class LeonArch(ArchSupport):
    @property
    def name(self):
        return "leon"

    def __init__(self):
        super(LeonArch, self).__init__()
        self.add_linker_switch('-Wl,-u_start', loader=None)
        self.add_gnat_sources(
            'sparc/leon/crt0.S',
            'sparc/leon/hw_init.S',
            'sparc/src/sparc.h',
            'src/s-macres__leon.adb')
        self.add_gnarl_sources(
            'src/s-bbcppr__old.ads',
            'src/s-bbcppr__sparc.adb',
            'src/s-bcpith__sparc.adb',
            'src/s-bbcpsp__leon.ads',
            'sparc/src/context_switch.S',
            'sparc/src/trap_handler.S',
            'sparc/src/interrupt_masking.S',
            'src/s-bbcaco.ads',
            'src/s-bbcaco__leon.adb',
            'src/s-bbinte__generic.adb')


class LeonTarget(DFBBTarget):
    @property
    def parent(self):
        return LeonArch

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-sparc.ads',
            'ravenscar-sfp': 'system-xi-sparc-ravenscar.ads',
            'ravenscar-full': 'system-xi-sparc-full.ads'
        }

    def amend_rts(self, rts_profile, conf):
        super(LeonTarget, self).amend_rts(rts_profile, conf)
        if rts_profile == 'ravenscar-full':
            # Use leon-zcx.specs to link with -lc.
            conf.config_files.update(
                {'link-zcx.spec': readfile('sparc/leon/leon-zcx.specs')})

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(LeonTarget, self).dump_runtime_xml(rts_name, rts)
        cnt = cnt.replace(' "-nolibc",', '')
        if rts_name == 'ravenscar-full':
            cnt = cnt.replace(
                '"-nostartfiles",',
                '"--specs=${RUNTIME_DIR(ada)}/link-zcx.spec",')
        return cnt


class Leon2(LeonTarget):
    @property
    def name(self):
        return "leon"

    @property
    def target(self):
        return 'leon-elf'

    @property
    def c_switches(self):
        # The required compiler switches
        return ('-DLEON', '-DLEON2')

    def __init__(self):
        super(Leon2, self).__init__()

        self.add_linker_script('sparc/leon/leon.ld', loader=None)
        self.add_gnat_sources(
            'src/s-textio__leon.adb',
            'src/s-bbbopa__leon.ads')
        self.add_gnarl_sources(
            'src/s-bbsumu__generic.adb',
            'src/s-bbsule__leon.ads',
            'src/s-bbbosu__leon.adb',
            'src/s-bbpara__leon.ads',
            'src/a-intnam__leon.ads')


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
    def system_ads(self):
        ret = super(Leon3, self).system_ads
        if self.smp:
            # zfp runtime makes no sense in the context of SMP variant
            del(ret['zfp'])
        return ret

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
        ret = ()
        if not self.smp:
            # see R409-022: -mcpu=leon3 makes gcc generate CASA instruction
            # when expanding compare_and_swap_4 intrinsic, which is invalid
            # SPARCv8 insn on most leon3.
            ret += ('-mcpu=leon',)
        else:
            ret += ('-mcpu=leon3',)

        if self.need_fix_ut699:
            ret += ('-mfix-ut699',)
        return ret

    @property
    def has_single_precision_fpu(self):
        # Single precision sqrt is buggy on UT699
        return not self.need_fix_ut699

    @property
    def has_compare_and_swap(self):
        if not self.smp:
            # see R409-022
            return False
        else:
            return True

    @property
    def readme_file(self):
        return 'sparc/leon3/README'

    def __init__(self, smp):
        self.smp = smp
        super(Leon3, self).__init__()

        self.add_linker_script('sparc/leon3/leon.ld', loader=None)
        self.add_gnat_sources(
            'src/s-textio__leon3.adb',
            'src/s-bbbopa__leon3-%s.ads' % ('smp' if smp else 'up', ),
            'src/i-leon3.ads',
            'src/i-leon3-uart.ads',
            'src/i-leon3-cache.ads')
        self.add_gnarl_sources(
            'src/i-leon3-timers.ads',
            'src/i-leon3-irqmp.ads',
            'src/s-bbbosu__leon3.adb',
            'src/s-bbpara__leon.ads',
            'src/a-intnam__leon3.ads')


class Leon4(Leon3):
    @property
    def name(self):
        if self.smp:
            return "leon4-smp"
        else:
            return "leon4"

    @property
    def need_fix_ut699(self):
        return False

    def __init__(self, smp):
        super(Leon4, self).__init__(smp)
        self.update_pair(
            's-bbbopa.ads',
            'src/s-bbbopa__leon4-%s.ads' % ('smp' if smp else 'up', ))
