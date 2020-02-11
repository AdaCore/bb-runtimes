# BSP support for PowerPC/e500v2
from support import readfile
from support.bsp_sources.archsupport import ArchSupport
from support.bsp_sources.target import DFBBTarget


class PPCArch(ArchSupport):
    @property
    def name(self):
        return "powerpc"

    def __init__(self):
        super(PPCArch, self).__init__()
        self.add_gnarl_sources(
            'src/s-bbcppr__new.ads',
            'src/s-bbcppr__ppc.adb',
            'src/s-bbinte__ppc.adb')


class PPC6XXArch(ArchSupport):
    @property
    def parent(self):
        return PPCArch

    @property
    def name(self):
        return '6xx'

    def __init__(self):
        super(PPC6XXArch, self).__init__()
        self.add_gnarl_sources(
            'powerpc/6xx/context_switch.S',
            'powerpc/6xx/handler.S',
            'src/s-bbcpsp__6xx.ads',
            'src/s-bbcpsp__6xx.adb')


class PPCSPEArch(ArchSupport):
    @property
    def parent(self):
        return PPCArch

    @property
    def name(self):
        return 'spe'

    def __init__(self):
        super(PPCSPEArch, self).__init__()
        self.add_gnarl_sources(
            'powerpc/spe/context_switch.S',
            'powerpc/spe/handler.S',
            'src/s-bbcpsp__spe.ads',
            'src/s-bbcpsp__spe.adb')


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
    def system_ads(self):
        return {
            'zfp': 'system-xi-ppc.ads',
            'ravenscar-sfp': 'system-xi-ppc-sfp.ads',
            'ravenscar-full': 'system-xi-ppc-full.ads'
        }

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(PPC6XXTarget, self).dump_runtime_xml(rts_name, rts)
        if rts_name == 'ravenscar-full':
            cnt = cnt.replace(
                '"-nostartfiles"',
                ('"-u", "_Unwind_Find_FDE", "-Wl,--eh-frame-hdr",\n'
                 '         "--specs=${RUNTIME_DIR(ada)}/link-zcx.spec"'))
        return cnt

    def amend_rts(self, rts_profile, conf):
        super(PPC6XXTarget, self).amend_rts(rts_profile, conf)
        # kill shrink-wrap-separate when building the runtime as this prevents
        # the frame to be properly built and thus prevents gdb from unwinding
        # the runtime (see R220-013).
        conf.build_flags['common_flags'] += ['-fno-shrink-wrap-separate']
        if rts_profile == 'ravenscar-full':
            conf.config_files.update(
                {'link-zcx.spec': readfile('powerpc/prep/link-zcx.spec')})


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
        self.add_gnat_sources(
            'powerpc/mpc8349/start-ram.S',
            'powerpc/mpc8349/setup.S',
            'src/s-macres__8349e.adb',
            'src/s-bbbopa__8349e.ads',
            'src/s-textio__p2020.adb')
        self.add_gnarl_sources(
            'src/s-bbbosu__8349e.adb',
            'src/s-bbpara__ppc.ads',
            'src/s-bbsuti__ppc.adb',
            'src/a-intnam__8349e.ads')


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

        self.add_gnat_sources(
            'powerpc/mpc8641/start-rom.S',
            'powerpc/mpc8641/setup.S',
            'src/s-macres__p2020.adb',
            'src/s-bbbopa__8641d.ads',
            'src/s-textio__p2020.adb')
        self.add_gnarl_sources(
            'src/s-bbbosu__ppc-openpic.adb',
            'src/s-bbsuti__ppc.adb',
            'src/s-bbsumu__8641d.adb',
            'src/s-bbpara__8641d.ads',
            'src/a-intnam__ppc-openpic.ads')


class PPCSPETarget(PPC6XXTarget):
    @property
    def parent(self):
        return PPCSPEArch

    @property
    def target(self):
        return 'powerpc-eabispe'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-xi-e500v2.ads',
            'ravenscar-sfp': 'system-xi-e500v2-sfp.ads',
            'ravenscar-full': 'system-xi-e500v2-full.ads'
        }


class P5634(PPCSPETarget):
    @property
    def name(self):
        return 'p5634'

    @property
    def compiler_switches(self):
        return ('-mfloat-gprs=single',)

    def __init__(self):
        super(P5634, self).__init__()
        self.add_linker_script('powerpc/mpc5634/5634.ld', loader=None)
        self.add_gnat_sources(
            'powerpc/mpc5634/start.S',
            'src/s-macres__p55.adb',
            'src/s-textio__p55.adb')


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
        self.add_gnat_sources(
            'powerpc/p2020/start-ram.S',
            'powerpc/p2020/setup.S',
            'src/s-macres__p2020.adb',
            'src/s-bbbopa__p2020.ads',
            'src/s-textio__p2020.adb')
        self.add_gnarl_sources(
            'src/s-bbsumu__generic.adb',
            'src/s-bbbosu__ppc-openpic.adb',
            'src/s-bbsuti__ppc.adb',
            'src/s-bbpara__ppc.ads',
            'src/a-intnam__ppc-openpic.ads')


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
        self.add_gnat_sources(
            'powerpc/p5566/start-bam.S',
            'powerpc/p5566/start-ram.S',
            'powerpc/p5566/start-flash.S',
            'powerpc/p5566/setup.S',
            'powerpc/p5566/setup-pll.S',
            'src/s-macres__p55.adb',
            'src/s-textio__p55.adb')
        self.add_gnarl_sources(
            'src/s-bbsumu__generic.adb',
            'src/s-bbbopa__p55.ads',
            'src/s-bbsuti__ppc.adb',
            'src/s-bbbosu__p55.adb',
            'src/s-bbpara__p55.ads',
            'src/a-intnam__p55.ads')
