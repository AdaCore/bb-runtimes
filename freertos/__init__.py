from support.bsp_sources.target import Target
from support.bsp_sources.archsupport import ArchSupport


class ArmFreeRTOS(Target):
    def __init__(self):
        super().__init__()
        self.add_gnat_sources("src/s-macres__native.adb")
        self.add_linker_switch("-Wl,-r", loader=None)
        self.add_linker_switch("-nostdlib", loader=None)

    @property
    def target(self):
        return "arm-eabi"

    def has_libc(self, profile):
        return True

    @property
    def system_ads(self):
        return {
            "light": "system-xi-arm.ads",
        }


class ArmV7AFP_FreeRTOS(ArmFreeRTOS):
    @property
    def has_timer_64(self):
        return True

    @property
    def has_single_precision_fpu(self):
        return True

    @property
    def has_double_precision_fpu(self):
        return True

    @property
    def name(self):
        return "v7a-fp"

    @property
    def compiler_switches(self):
        return (
            "-march=armv7-a+fp",
            "-mfloat-abi=hard",
            "-marm",
            "-mno-unaligned-access",
        )
