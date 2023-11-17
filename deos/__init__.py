from support import readfile
from support.bsp_sources.target import Target


class Deos(Target):
    def __init__(self):
        super(Deos, self).__init__()
        self.add_gnat_sources("src/s-macres__deos.adb")

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
    def has_small_memory(self):
        return True

    @property
    def is_legacy_format(self):
        return True

    def has_libc(self, profile):
        return True

    def dump_runtime_xml(self, rts_name, rts):
        return readfile("deos/runtime.xml")

    def amend_rts(self, rts_profile, cfg):
        # cfg.build_flags["common_flags"] += ["-DCERT", '-I" & DESK_HOME & "/include']
        # cfg.build_flags["external_paths"] += ['Desk_Home := external ("DESKHOME");']
        # Temporary workaround while we work on a more cleaner implementation of
        # providing paths in target_options.py
        cfg.build_flags["common_flags"] += [
            "-DCERT",
            "-I../../../../../../../../../src/deos-20220524-linux/desk/include",
        ]


class ArmDeos(Deos):
    def __init__(self):
        super(ArmDeos, self).__init__()

    @property
    def target(self):
        return "arm-elf"

    @property
    def name(self):
        return "arm-deos"

    @property
    def system_ads(self):
        return {"cert": "system-deos-arm-light.ads"}

    def amend_rts(self, rts_profile, cfg):
        super(ArmDeos, self).amend_rts(rts_profile, cfg)
        cfg.build_flags["common_flags"] += [
            "-mabi=aapcs-linux",
            "-march=armv7-a",
            "-mfloat-abi=hard",
            "-mfpu=vfpv3",
            "-mthumb",
            "-mthumb-interwork",
            "-mno-unaligned-access",
            "-mrestrict-it",
            "-fPIC",
        ]
