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
    def has_timer_64(self):
        return True

    @property
    def is_legacy_format(self):
        return True


class AArch64Vx7r2Cert(Vx7r2Cert):
    def __init__(self):
        super(AArch64Vx7r2Cert, self).__init__()

    @property
    def target(self):
        return 'aarch64-vx7r2'

    @property
    def name(self):
        return 'aarch64-vx7r2cert'

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {'zfp': 'system-vxworks7-aarch64-zfp.ads'}
