from support.bsp_sources.target import Target
from support.bsp_sources.archsupport import ArchSupport


class Lynx(Target):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("src/s-macres__libc.adb", "lynx/stdio_symbols.c")

    def has_libc(self, profile):
        return True

    @property
    def is_os_target(self):
        return True

    @property
    def has_command_line_arguments(self):
        return True

    @property
    def is_legacy_format(self):
        return True


class PPCLynx(Lynx):
    def __init__(self):
        super().__init__()

    @property
    def has_double_precision_fpu(self):
        # Disable FPU support. Otherwise, `__builtins_*` could fallbacks
        # on missing features of LynxOS libm.
        return False

    @property
    def target(self):
        return "ppc-lynx178"

    @property
    def name(self):
        return "lynx"

    @property
    def system_ads(self):
        return {
            "light": "system-lynxos178-ppc.ads",
        }
