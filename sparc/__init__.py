# BSP support for Sparc/Leon
from build_rts_support import readfile
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget


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
            {'s-macres.adb': 's-macres-leon.adb',
             'sparc.h': 'sparc-bb.h'}])
        self.add_sources('gnarl', [
            {'s-bbcppr.adb': 's-bbcppr-sparc.adb',
             's-bcpith.adb': 's-bcpith-bb-sparc.adb',
             'context_switch.S': 'context_switch-bb-sparc.S',
             'trap_handler.S': 'trap_handler-bb-sparc.S',
             'interrupt_masking.S': 'interrupt_masking-bb-sparc.S',
             'floating_point.S': 'floating_point-bb-sparc.S',
             's-bbcaco.adb': 's-bbcaco-leon.adb'},
            's-bbcaco.ads',
            's-bbcppr.ads',
            's-bbinte.adb'])


class LeonTarget(DFBBTarget):
    @property
    def has_newlib(self):
        return True

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

    def amend_zfp(self, conf):
        super(LeonTarget, self).amend_zfp(conf)
        conf.rts_xml = \
            conf.rts_xml.replace(
                ' "-nolibc",', '')

    def amend_ravenscar_full(self, conf):
        super(LeonTarget, self).amend_ravenscar_full(conf)
        # Use leon-zcx.specs to link with -lc.
        conf.config_files.update(
            {'link-zcx.spec':
             readfile('sparc/leon/leon-zcx.specs')})
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
        self.add_sources('crt0', {
            's-textio.adb': 's-textio-leon.adb',
            's-bbbopa.ads': 's-bbbopa-leon.ads'})
        self.add_sources('gnarl', [
            's-bbsule.ads',
            's-bbsumu.adb',
            {'s-bbbosu.adb': 's-bbbosu-leon.adb',
             's-bbpara.ads': 's-bbpara-leon.ads',
             'a-intnam.ads': 'a-intnam-xi-leon.ads'}])


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

    def __init__(self):
        super(Leon3, self).__init__()

        self.add_linker_script('sparc/leon3/leon.ld', loader=None)
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


class Leon4(Leon3):
    @property
    def name(self):
        return "leon4"

    def __init__(self):
        super(Leon4, self).__init__()
        self.update_pair('s-bbbopa.ads', 's-bbbopa-leon4.ads')
