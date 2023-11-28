from support import readfile
from support.bsp_sources.target import Target


class Vx7r2Cert(Target):
    def __init__(self, is_rtp):
        self._is_rtp = is_rtp
        self._rtp_suffix = "-rtp" if self._is_rtp else ""
        super(Vx7r2Cert, self).__init__()
        self.add_gnat_sources('src/s-macres__vx7r2cert.adb')

    @property
    def has_command_line_arguments(self):
        return True

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

        compiler_package = """
            -- Prevent optimizations turning some loop patterns into
            -- libc calls, e.g. string length computations into strlen,
            -- as these introduce undesirable (for cert) dependencies
            -- against external symbols.
            package Compiler is
              Common_Required_Switches :=
                ("-fno-tree-loop-distribute-patterns");
              for Leading_Required_Switches ("Ada") use
                Compiler'Leading_Required_Switches ("Ada") &
                Common_Required_Switches;
              for Leading_Required_Switches ("C") use
                Compiler'Leading_Required_Switches ("C") &
                Common_Required_Switches;
            end Compiler;
        """
        return readfile(f'vx7r2cert/runtime{self._rtp_suffix}.xml') % {
            "compiler": compiler_package}

    def amend_rts(self, rts_profile, cfg):
        if self._is_rtp:
            cfg.rts_vars["Is_RTP"] = "yes"
            cfg.build_flags['common_flags'] += [
                    '-mrtp', '-DCERT_RTP', '-D_WRS_VXCERT_RTP']


class Vx7r2Cert64(Vx7r2Cert):
    def __init__(self, is_rtp):
        super(Vx7r2Cert64, self).__init__(is_rtp)

    def is_64bit(self):
        return True


class AArch64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self, is_rtp=False):
        super(AArch64Vx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'aarch64-vx7r2'

    @property
    def name(self):
        return f'aarch64-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-arm-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-arm-zfp.ads',
                'light-tasking': 'system-vxworks7-arm-ravenscar-sfp.ads'}

    def amend_rts(self, rts_profile, conf):
        super(AArch64Vx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags['common_flags'] += [
            '-mno-outline-atomics',
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See T709-039 for more info.
            '-fno-omit-frame-pointer',
        ]


class ArmVx7r2Cert(Vx7r2Cert):
    def __init__(self, is_rtp=False):
        super(ArmVx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'arm-vx7r2'

    @property
    def name(self):
        return f'arm-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-arm-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-arm-zfp.ads',
                'light-tasking': 'system-vxworks7-arm-ravenscar-sfp.ads'}

    def amend_rts(self, rts_profile, conf):
        super(ArmVx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags['common_flags'] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            '-fno-omit-frame-pointer',
        ]


class PPCVx7r2Cert(Vx7r2Cert):
    def __init__(self, is_rtp=False):
        super(PPCVx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'ppc-vx7r2'

    @property
    def name(self):
        return f'ppc-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-ppc-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-ppc-zfp.ads',
                'light-tasking': 'system-vxworks7-ppc-ravenscar-sfp.ads'}


class PPC64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self, is_rtp=False):
        super(PPC64Vx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'ppc64-vx7r2'

    @property
    def name(self):
        return f'ppc64-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-ppc64-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-ppc-zfp.ads',
                'light-tasking': 'system-vxworks7-ppc64-ravenscar-sfp.ads'}


class X86Vx7r2Cert(Vx7r2Cert):
    def __init__(self, is_rtp=False):
        super(X86Vx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'x86-vx7r2'

    @property
    def name(self):
        return f'x86-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-x86-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-x86-zfp.ads',
                'light-tasking': 'system-vxworks7-x86-ravenscar-sfp.ads'}


class X86_64Vx7r2Cert(Vx7r2Cert64):
    def __init__(self, is_rtp=False):
        super(X86_64Vx7r2Cert, self).__init__(is_rtp)

    @property
    def target(self):
        return 'x86_64-vx7r2'

    @property
    def name(self):
        return f'x86_64-vx7r2cert{self._rtp_suffix}'

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                'light-tasking': 'system-vxworks7-x86-ravenscar-sfp-rtp.ads'}
        else:
            return {
                'light': 'system-vxworks7-x86-zfp.ads',
                'light-tasking': 'system-vxworks7-x86-ravenscar-sfp.ads'}

    def amend_rts(self, rts_profile, conf):
        super(X86_64Vx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags['common_flags'] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            '-fno-omit-frame-pointer',
        ]


class AArch64Vx7r2Cert_RTP(AArch64Vx7r2Cert):
    def __init__(self):
        super(AArch64Vx7r2Cert_RTP, self).__init__(is_rtp=True)


class ArmVx7r2Cert_RTP(ArmVx7r2Cert):
    def __init__(self):
        super(ArmVx7r2Cert_RTP, self).__init__(is_rtp=True)


class PPCVx7r2Cert_RTP(PPCVx7r2Cert):
    def __init__(self):
        super(PPCVx7r2Cert_RTP, self).__init__(is_rtp=True)


class PPC64Vx7r2Cert_RTP(PPC64Vx7r2Cert):
    def __init__(self):
        super(PPC64Vx7r2Cert_RTP, self).__init__(is_rtp=True)


class X86Vx7r2Cert_RTP(X86Vx7r2Cert):
    def __init__(self):
        super(X86Vx7r2Cert_RTP, self).__init__(is_rtp=True)


class X86_64Vx7r2Cert_RTP(X86_64Vx7r2Cert):
    def __init__(self):
        super(X86_64Vx7r2Cert_RTP, self).__init__(is_rtp=True)
