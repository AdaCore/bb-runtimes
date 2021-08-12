from support import readfile
from support.bsp_sources.target import Target


class Vx7r2Cert(Target):
    def __init__(self):
        super(Vx7r2Cert, self).__init__()
        self.add_gnat_sources('src/s-macres__vx7r2cert.adb')

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def has_huge_memory(self):
        return True

    @property
    def is_legacy_format(self):
        return True

    def has_libc(self, profile):
        return True

    def dump_runtime_xml(self, rts_name, rts):
        return readfile('vx7r2cert/runtime.xml')


class Vx7r2Cert64(Vx7r2Cert):
    def __init__(self):
        super(Vx7r2Cert64, self).__init__()

    def is_64bit(self):
        return True


class AArch64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self):
        super(AArch64Vx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'aarch64-vx7r2'

    @property
    def name(self):
        return 'aarch64-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-arm-zfp.ads',
            'light-tasking': 'system-vxworks7-arm-ravenscar-sfp.ads'}


class ArmVx7r2Cert(Vx7r2Cert):
    def __init__(self):
        super(ArmVx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'arm-vx7r2'

    @property
    def name(self):
        return 'arm-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-arm-zfp.ads',
            'light-tasking': 'system-vxworks7-arm-ravenscar-sfp.ads'}


class PPCVx7r2Cert(Vx7r2Cert):
    def __init__(self):
        super(PPCVx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'ppc-vx7r2'

    @property
    def name(self):
        return 'ppc-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-ppc-zfp.ads',
            'light-tasking': 'system-vxworks7-ppc-ravenscar-sfp.ads'}


class PPC64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self):
        super(PPC64Vx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'ppc64-vx7r2'

    @property
    def name(self):
        return 'ppc64-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-ppc-zfp.ads',
            'light-tasking': 'system-vxworks7-ppc-ravenscar-sfp.ads'}


class X86Vx7r2Cert(Vx7r2Cert):
    def __init__(self):
        super(X86Vx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'x86-vx7r2'

    @property
    def name(self):
        return 'x86-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-x86-zfp.ads',
            'light-tasking': 'system-vxworks7-x86-ravenscar-sfp.ads'}


class X86_64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self):
        super(X86_64Vx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'x86_64-vx7r2'

    @property
    def name(self):
        return 'x86_64-vx7r2cert'

    @property
    def system_ads(self):
        return {
            'zfp': 'system-vxworks7-x86-zfp.ads',
            'light-tasking': 'system-vxworks7-x86-ravenscar-sfp.ads'}
