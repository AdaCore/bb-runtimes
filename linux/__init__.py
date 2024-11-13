from support.bsp_sources.target import DFBBTarget


class Linux(DFBBTarget):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("src/s-macres__libc.adb")
        self.add_gnarl_sources("linux/adaint.c")

    def has_libc(self, profile):
        return True

    @property
    def name(self):
        return self.target

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


class X86Linux(Linux):
    @property
    def target(self):
        return "x86-linux"

    @property
    def system_ads(self):
        return {
            "light": "system-native-x86-light.ads",
            "light-tasking": "system-native-x86-light-tasking.ads",
        }


class X8664Linux(Linux):
    @property
    def target(self):
        return "x86_64-linux"

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-native-x86-light.ads",
            "light-tasking": "system-native-x86-light-tasking.ads",
        }


class Aarch64Linux(Linux):
    @property
    def target(self):
        return "aarch64-linux"

    @property
    def is_64bit(self):
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-native-arm-light.ads",
            "light-tasking": "system-native-arm-light-tasking.ads",
        }
