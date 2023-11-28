from support.bsp_sources.target import Target


class QNX(Target):
    def __init__(self):
        super(QNX, self).__init__()
        self.add_gnat_sources('src/s-macres__qnx.adb')

    @property
    def has_command_line_arguments(self):
        return True

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
            'light': 'system-qnx-arm-light.ads',
            'light-tasking': 'system-qnx-arm-light-tasking.ads'}

    def amend_rts(self, rts_profile, cfg):
        cfg.build_flags['common_flags'] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            '-fno-omit-frame-pointer',
        ]


class ARMQNX(QNX):
    def __init__(self):
        super(ARMQNX, self).__init__()

    @property
    def is_64bit(self):
        return False

    @property
    def target(self):
        return 'arm-qnx'

    @property
    def name(self):
        return 'qnx'

    @property
    def system_ads(self):
        return {
            'light': 'system-qnx-arm-light.ads',
            'light-tasking': 'system-qnx-arm-light-tasking.ads'}
