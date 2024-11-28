from support.bsp_sources.target import DFBBTarget


class Windows(DFBBTarget):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("src/s-macres__libc.adb", "src/s-textio__stdio.adb")

    @property
    def name(self):
        return self.target

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

    @property
    def is_legacy_format(self):
        return True


class X86Windows(Windows):
    @property
    def target(self):
        return "x86-windows"

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}


class X8664Windows(Windows):
    @property
    def target(self):
        return "x86_64-windows"

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {"light": "system-native-x86-light.ads"}
