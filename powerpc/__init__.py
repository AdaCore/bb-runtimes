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
        self.add_sources('gnarl', {
            's-bbcppr.adb': 's-bbcppr-ppc.adb',
            's-bbcppr.ads': 's-bbcppr-ppc.ads',
            's-bbinte.adb': 's-bbinte-ppc.adb'})


class PPC6XXArch(PPCArch):
    def __init__(self):
        super(PPC6XXArch, self).__init__()
        self.add_sources('gnarl-ppc6xx', [
            'powerpc/6xx/context_switch.S',
            'powerpc/6xx/handler.S'])
        self.add_sources('gnarl-ppc6xx', {
            's-bbcpsp.ads': 's-bbcpsp-6xx.ads',
            's-bbcpsp.adb': 's-bbcpsp-6xx.adb'})


class PPCSPEArch(PPCArch):
    def __init__(self):
        super(PPCSPEArch, self).__init__()
        self.add_sources('gnarl-spe', [
            'powerpc/spe/handler.S',
            'powerpc/spe/context_switch.S'])
        self.add_sources('gnarl-spe', {
            's-bbcpsp.ads': 's-bbcpsp-spe.ads',
            's-bbcpsp.adb': 's-bbcpsp-spe.adb'})


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
