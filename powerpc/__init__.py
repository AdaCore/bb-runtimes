# BSP support for PowerPC/e500v2
from build_rts_support.bsp import BSP
from build_rts_support.target import DFBBTarget

from build_rts_support import readfile


class PPCArch(BSP):
    @property
    def name(self):
        return "powerpc"

    def __init__(self):
        super(PPCArch, self).__init__()
        self.add_sources('gnarl', [
            'src/s-bbcppr/new/s-bbcppr.ads',
            'src/s-bbcppr/ppc/s-bbcppr.adb',
            'src/s-bbinte/ppc/s-bbinte.adb'])


class PPC6XXArch(PPCArch):
    def __init__(self):
        super(PPC6XXArch, self).__init__()
        self.add_sources('gnarl-ppc6xx', [
            'powerpc/6xx/context_switch.S',
            'powerpc/6xx/handler.S'])
        self.add_sources('gnarl-ppc6xx', [
            'src/s-bbcpsp/6xx/s-bbcpsp.ads',
            'src/s-bbcpsp/6xx/s-bbcpsp.adb'])


class PPCSPEArch(PPCArch):
    def __init__(self):
        super(PPCSPEArch, self).__init__()
        self.add_sources('gnarl-spe', [
            'powerpc/spe/handler.S',
            'powerpc/spe/context_switch.S'])
        self.add_sources('gnarl-spe', [
            'src/s-bbcpsp/spe/s-bbcpsp.ads',
            'src/s-bbcpsp/spe/s-bbcpsp.adb'])


class PPC6XXTarget(DFBBTarget):
    @property
    def target(self):
        return 'powerpc-elf'

    @property
    def parent(self):
        return PPC6XXArch

    @property
    def has_timer_64(self):
        return True

    @property
    def has_newlib(self):
        return False

    @property
    def has_fpu(self):
        # Add fpu support
        return True

    @property
    def has_single_precision_fpu(self):
        # But use soft float in the math lib
        return False

    @property
    def has_double_precision_fpu(self):
        # But use soft float in the math lib
        return False

    @property
    def zfp_system_ads(self):
        return 'system-xi-ppc.ads'

    @property
    def sfp_system_ads(self):
        return 'system-xi-ppc-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-ppc-full.ads'

    def __init__(self):
        super(PPC6XXTarget, self).__init__(
            mem_routines=True,
            small_mem=False)

    def amend_ravenscar_full(self, conf):
        super(PPC6XXTarget, self).amend_ravenscar_full(conf)
        conf.config_files.update(
            {'link-zcx.spec':
             readfile('powerpc/prep/link-zcx.spec')})
        conf.rts_xml = conf.rts_xml.replace(
                '"-nostartfiles", "-lgnat", "-lgcc"',
                '"-nolibc",\n' +
                '         "-lgnat", "-lgcc", "-lgnat",\n' +
                '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"')


class MPC8349e(PPC6XXTarget):
    @property
    def name(self):
        return 'mpc8349e'

    @property
    def compiler_switches(self):
        return ('-msoft-float', '-mno-sdata')

    def __init__(self):
        super(MPC8349e, self).__init__()

        self.add_linker_script('powerpc/mpc8349/ram.ld', loader=None)
        self.add_linker_switch('-Wl,-z,max-page-size=0x1000')
        self.add_sources('crt0', [
            'powerpc/mpc8349/start-ram.S',
            'powerpc/mpc8349/setup.S',
            'src/s-macres/8349e/s-macres.adb',
            'src/s-bbbopa/8349e/s-bbbopa.ads',
            'src/s-textio/p2020/s-textio.adb'])
        self.add_sources('gnarl', [
            'src/s-bbbosu/8349e/s-bbbosu.adb',
            'src/s-bbpara/ppc/s-bbpara.ads',
            {'s-bbsuti.adb': 's-bbsuti-ppc.adb'},
            'src/a-intnam/8349e/a-intnam.ads'])


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

    @property
    def readme_file(self):
        return 'powerpc/mpc8641/README'

    def __init__(self):
        super(MPC8641, self).__init__()
        self.add_linker_script('powerpc/mpc8641/qemu-rom.ld', loader='ROM')
        self.add_linker_switch('-Wl,-u_start_rom', loader='ROM')
        self.add_linker_script('powerpc/mpc8641/ram.ld', loader='RAM')
        self.add_linker_switch('-Wl,-u_start_ram', loader='RAM')

        self.add_sources('crt0', [
            'powerpc/mpc8641/start-rom.S',
            'powerpc/mpc8641/setup.S'])
        self.add_sources('crt0', [
            'src/s-macres/p2020/s-macres.adb',
            'src/s-bbbopa/8641d/s-bbbopa.ads',
            'src/s-textio/p2020/s-textio.adb'])
        self.add_sources('gnarl', [
            'src/s-bbbosu/ppc-openpic/s-bbbosu.adb',
            {'s-bbsuti.adb': 's-bbsuti-ppc.adb'},
            'src/s-bbsumu/8641d/s-bbsumu.adb',
            'src/s-bbpara/8641d/s-bbpara.ads',
            'src/a-intnam/ppc-openpic/a-intnam.ads'])


class PPCSPETarget(PPC6XXTarget):
    @property
    def parent(self):
        return PPCSPEArch

    @property
    def target(self):
        return 'powerpc-eabispe'

    @property
    def zfp_system_ads(self):
        return 'system-xi-e500v2.ads'

    @property
    def sfp_system_ads(self):
        return 'system-xi-e500v2-sfp.ads'

    @property
    def full_system_ads(self):
        return 'system-xi-e500v2-full.ads'


class P5634(PPCSPETarget):
    @property
    def name(self):
        return 'p5634'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=single')

    def __init__(self):
        super(P5634, self).__init__()
        self.add_linker_script('powerpc/mpc5634/5634.ld', loader=None)
        self.add_sources('crt0', [
            'powerpc/mpc5634/start.S',
            'src/s-macres/p55/s-macres.adb',
            'src/s-textio/p55/s-textio.adb'])


class P2020(PPCSPETarget):
    @property
    def name(self):
        return 'p2020'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=double', )

    @property
    def readme_file(self):
        return 'powerpc/p2020/README'

    def __init__(self):
        super(P2020, self).__init__()
        self.add_linker_script('powerpc/p2020/p2020.ld', loader=None)
        self.add_sources('crt0', [
            'powerpc/p2020/start-ram.S',
            'powerpc/p2020/setup.S',
            'src/s-macres/p2020/s-macres.adb',
            'src/s-bbbopa/p2020/s-bbbopa.ads',
            'src/s-textio/p2020/s-textio.adb'])
        self.add_sources('gnarl', [
            'src/s-bbsumu/generic/s-bbsumu.adb',
            'src/s-bbbosu/ppc-openpic/s-bbbosu.adb',
            {'s-bbsuti.adb': 's-bbsuti-ppc.adb'},
            'src/s-bbpara/ppc/s-bbpara.ads',
            'src/a-intnam/ppc-openpic/a-intnam.ads'])


class P5566(PPCSPETarget):
    @property
    def name(self):
        return 'p5566'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=single', )

    @property
    def loaders(self):
        return ('EXTRAM', 'BAM', 'FLASH')

    @property
    def readme_file(self):
        return 'powerpc/p5566/README'

    def __init__(self):
        super(P5566, self).__init__()
        self.add_linker_script('powerpc/p5566/bam.ld', loader='BAM')
        self.add_linker_script('powerpc/p5566/flash.ld', loader='FLASH')
        self.add_linker_script('powerpc/p5566/ram.ld', loader='EXTRAM')
        self.add_sources('crt0', [
            'powerpc/p5566/start-bam.S',
            'powerpc/p5566/start-ram.S',
            'powerpc/p5566/start-flash.S',
            'powerpc/p5566/setup.S',
            'powerpc/p5566/setup-pll.S',
            'src/s-macres/p55/s-macres.adb',
            'src/s-textio/p55/s-textio.adb'])
        self.add_sources('gnarl', [
            'src/s-bbsumu/generic/s-bbsumu.adb',
            'src/s-bbbopa/p55/s-bbbopa.ads',
            {'s-bbsuti.adb': 's-bbsuti-ppc.adb'},
            'src/s-bbbosu/p55/s-bbbosu.adb',
            'src/s-bbpara/p55/s-bbpara.ads',
            'src/a-intnam/p55/a-intnam.ads'])
