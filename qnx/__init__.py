from support.bsp_sources.target import Target


class QNX(Target):
    def __init__(self):
        super(QNX, self).__init__()
        self.add_gnat_sources('src/s-macres__qnx.adb')

    def has_libc(self, profile):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def is_legacy_format(self):
        return True

    @property
    def use_certifiable_packages(self):
        return True


class Aarch64QNX(QNX):
    def __init__(self):
        super(Aarch64QNX, self).__init__()

    @property
    def is_64bit(self):
        return True

    @property
    def target(self):
        return 'aarch64-qnx'

    @property
    def name(self):
        return 'qnx'

    @property
    def system_ads(self):
        return {
            'light': 'system-qnx-aarch64-light.ads',
            'light-tasking': 'system-qnx-aarch64-light-tasking.ads'}
