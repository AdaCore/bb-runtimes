from support import readfile
from support.bsp_sources.target import Target


class Vx7r2Cert(Target):
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

    @property
    def is_os_target(self):
        return True

    def has_libc(self, profile):
        return True

    @property
    def loaders(self):
        if self._is_rtp:
            # We reuse the existing LOADER infrastructure to select between
            # full and partial linking options.
            return ("FULL_LINK", "PARTIAL_LINK")
        else:
            return None

    @property
    def compiler_switches(self):
        if self._is_rtp:
            return ("-mrtp",)
        else:
            return ()

    def amend_rts(self, rts_profile, cfg):
        if self._is_rtp:
            cfg.rts_vars["Is_RTP"] = "yes"
            cfg.build_flags["common_flags"] += [
                "-mrtp",
                "-DCERT_RTP",
                "-D_WRS_VXCERT_RTP",
            ]

    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(Vx7r2Cert, self).dump_runtime_xml(rts_name, rts)

        if self._is_rtp:
            cnt = cnt.replace(
                "   package Linker is",
                '   VSB_Dir := external ("VSB_DIR");\n\n   package Linker is\n',
            )
            cnt = cnt.replace("_vsb_dir_env_", '" & VSB_DIR & "')

        return cnt

    def __init__(self, is_rtp):
        self._is_rtp = is_rtp
        self._rtp_suffix = "-rtp" if self._is_rtp else ""
        super(Vx7r2Cert, self).__init__()

        self.add_gnat_sources("src/s-macres__vx7r2cert.adb")

        if self._is_rtp:
            # Provide the required switches to perform a full RTP link.
            # Switches for partial link are performed by the GNATbench
            # makefiles.
            self.add_linker_switch("-nodefaultlibs", loader="FULL_LINK")
            self.add_linker_switch("-nostartfiles", loader="FULL_LINK")
            self.add_linker_switch("-l:certRtp.o", loader="FULL_LINK")
            self.add_linker_switch(
                "-L_vsb_dir_env_/usr/lib/common/objcert", loader="FULL_LINK"
            )
            self.add_linker_switch(
                "-T_vsb_dir_env_/usr/ldscripts/rtp.ld", loader="FULL_LINK"
            )
        else:
            self.add_linker_switch("-nostdlib")


class Vx7r2Cert64(Vx7r2Cert):
    def __init__(self, is_rtp):
        super(Vx7r2Cert64, self).__init__(is_rtp)

    def is_64bit(self):
        return True


class AArch64Vx7r2Cert(Vx7r2Cert64):
    @property
    def target(self):
        return "aarch64-vx7r2"

    @property
    def name(self):
        return f"aarch64-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-arm-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-arm-zfp.ads",
                "light-tasking": "system-vxworks7-arm-ravenscar-sfp.ads",
            }

    def amend_rts(self, rts_profile, conf):
        super(AArch64Vx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags["common_flags"] += [
            "-mno-outline-atomics",
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See T709-039 for more info.
            "-fno-omit-frame-pointer",
        ]

    def __init__(self, is_rtp=False):
        super(AArch64Vx7r2Cert, self).__init__(is_rtp)


class MorelloVx7r2Cert(AArch64Vx7r2Cert):
    @property
    def target(self):
        return "morello-vx7r2"

    @property
    def name(self):
        return f"morello-vx7r2cert{self._rtp_suffix}"

    @property
    def compiler_switches(self):
        if self._is_rtp:
            return ("-march=morello+c64", "-mabi=purecap")
        else:
            # Until the kernel supports purecap mode, just build a regular
            # aarch64 kernel runtimes. This saves us from having to make a
            # special morello exception in built_rts.py to exclude the kernel
            # runtimes.
            return ()

    @property
    def has_cheri(self):
        return True

    @property
    def system_ads(self):
        if self._is_rtp:
            return {
                "light": "system-vxworks7-arm-light-rtp.ads",
                "light-tasking": "system-vxworks7-arm-ravenscar-sfp-rtp.ads",
            }
        else:
            return {
                "light": "system-vxworks7-arm-zfp.ads",
                "light-tasking": "system-vxworks7-arm-ravenscar-sfp.ads",
            }

    def __init__(self, is_rtp=False):
        super(MorelloVx7r2Cert, self).__init__(is_rtp)


class ArmVx7r2Cert(Vx7r2Cert):
    @property
    def target(self):
        return "arm-vx7r2"

    @property
    def name(self):
        return f"arm-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-arm-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-arm-zfp.ads",
                "light-tasking": "system-vxworks7-arm-ravenscar-sfp.ads",
            }

    def amend_rts(self, rts_profile, conf):
        super(ArmVx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags["common_flags"] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            "-fno-omit-frame-pointer",
        ]

    def __init__(self, is_rtp=False):
        super(ArmVx7r2Cert, self).__init__(is_rtp)


class PPCVx7r2Cert(Vx7r2Cert):
    @property
    def target(self):
        return "ppc-vx7r2"

    @property
    def name(self):
        return f"ppc-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-ppc-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-ppc-zfp.ads",
                "light-tasking": "system-vxworks7-ppc-ravenscar-sfp.ads",
            }

    def __init__(self, is_rtp=False):
        super(PPCVx7r2Cert, self).__init__(is_rtp)


class PPC64Vx7r2Cert(Vx7r2Cert64):
    @property
    def target(self):
        return "ppc64-vx7r2"

    @property
    def name(self):
        return f"ppc64-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-ppc64-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-ppc-zfp.ads",
                "light-tasking": "system-vxworks7-ppc64-ravenscar-sfp.ads",
            }

    def __init__(self, is_rtp=False):
        super(PPC64Vx7r2Cert, self).__init__(is_rtp)


class X86Vx7r2Cert(Vx7r2Cert):
    @property
    def target(self):
        return "x86-vx7r2"

    @property
    def name(self):
        return f"x86-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-x86-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-x86-zfp.ads",
                "light-tasking": "system-vxworks7-x86-ravenscar-sfp.ads",
            }

    def __init__(self, is_rtp=False):
        super(X86Vx7r2Cert, self).__init__(is_rtp)


class X86_64Vx7r2Cert(Vx7r2Cert64):
    @property
    def target(self):
        return "x86_64-vx7r2"

    @property
    def name(self):
        return f"x86_64-vx7r2cert{self._rtp_suffix}"

    @property
    def system_ads(self):
        if self._is_rtp:
            return {"light-tasking": "system-vxworks7-x86-ravenscar-sfp-rtp.ads"}
        else:
            return {
                "light": "system-vxworks7-x86-zfp.ads",
                "light-tasking": "system-vxworks7-x86-ravenscar-sfp.ads",
            }

    def amend_rts(self, rts_profile, conf):
        super(X86_64Vx7r2Cert, self).amend_rts(rts_profile, conf)
        conf.build_flags["common_flags"] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            "-fno-omit-frame-pointer",
        ]

    def __init__(self, is_rtp=False):
        super(X86_64Vx7r2Cert, self).__init__(is_rtp)


# RTP note: the -Wl,--defsym=__wrs_rtp_base= linker switch is defined in the
# gprconfig_kb database.


class AArch64Vx7r2Cert_RTP(AArch64Vx7r2Cert):
    def __init__(self):
        super(AArch64Vx7r2Cert_RTP, self).__init__(is_rtp=True)


class MorelloVx7r2Cert_RTP(MorelloVx7r2Cert):
    def dump_runtime_xml(self, rts_name, rts):
        cnt = super(MorelloVx7r2Cert_RTP, self).dump_runtime_xml(rts_name, rts)
        # While Morello VxWorks Cert can't build a GOS_SAFETY profile VSB,
        # link with the regular RTP libraries. Note: gprconfig_kb
        # unfortunately forces -nostdlib, which is why we need to be explicit
        # with the system library links.
        return cnt.replace(
            '"-l:certRtp.o"', '"-l:crt0.o", "-lc", "-lc_internal_s", "-lgcc"'
        )

    def __init__(self):
        super(MorelloVx7r2Cert_RTP, self).__init__(is_rtp=True)
        self.add_linker_switch("-Wl,--defsym=__wrs_rtp_base=0x80000000")


class ArmVx7r2Cert_RTP(ArmVx7r2Cert):
    def __init__(self):
        super(ArmVx7r2Cert_RTP, self).__init__(is_rtp=True)


class PPCVx7r2Cert_RTP(PPCVx7r2Cert):
    def __init__(self):
        super(PPCVx7r2Cert_RTP, self).__init__(is_rtp=True)
        self.add_linker_switch("-lgnu", loader="FULL_LINK")


class PPC64Vx7r2Cert_RTP(PPC64Vx7r2Cert):
    def __init__(self):
        super(PPC64Vx7r2Cert_RTP, self).__init__(is_rtp=True)
        self.add_linker_switch("-lgnu", loader="FULL_LINK")


class X86Vx7r2Cert_RTP(X86Vx7r2Cert):
    def __init__(self):
        super(X86Vx7r2Cert_RTP, self).__init__(is_rtp=True)


class X86_64Vx7r2Cert_RTP(X86_64Vx7r2Cert):
    def __init__(self):
        super(X86_64Vx7r2Cert_RTP, self).__init__(is_rtp=True)
