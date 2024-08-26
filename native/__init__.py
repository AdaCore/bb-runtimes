from support.bsp_sources.target import DFBBTarget


class Native(DFBBTarget):
    def __init__(self, target):
        super().__init__()
        self.add_gnat_sources("src/s-macres__native.adb")
        # Use the bb source of s-textio for Windows as a custom
        # source profile is not yet supported.
        # eng/toolchain/bb-runtimes#78
        if "windows" in target:
            self.add_gnat_sources("src/s-textio__stdio.adb")

    @property
    def target(self):
        return None

    def has_libc(self, profile):
        return True

    def dump_runtime_xml(self, rts_name, rts):
        return (
            '<?xml version="1.0" ?>\n'
            "<gprconfig>\n"
            "  <configuration>\n"
            "  </configuration>\n"
            "</gprconfig>\n"
        )

    def amend_rts(self, rts_profile, cfg):
        super().amend_rts(rts_profile, cfg)


class X86Native(Native):
    @property
    def name(self):
        return "native-x86"

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}


class X8664Native(Native):
    @property
    def name(self):
        return "native-x86_64"

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}


class Aarch64Native(Native):
    @property
    def name(self):
        return "native-aarch64"

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {"light": "system-native-arm-light.ads"}
