import os
import subprocess
import shutil
import tempfile

from support.bsp_sources.target import Target


class QNX(Target):
    def __init__(self):
        super(QNX, self).__init__()
        self.add_gnat_sources("src/s-macres__libc.adb")

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
    def is_os_target(self):
        return True

    @property
    def use_certifiable_packages(self):
        return True

    def pre_build_step(self, obj_dir):
        # For QNX, create a dummy shared library with the name
        # of the shared last chance handler.
        # Use a temporary file to create an empty file.
        # delete_on_close is needed on windows to be able to read the
        # file (otherwise we get a permission denied).
        tf = tempfile.NamedTemporaryFile(mode="rt", delete_on_close=False)
        subprocess.check_call(
            [
                "aarch64-nto-qnx-gcc",
                "-shared",
                "-o",
                os.path.join(obj_dir, "libada_lch.so"),
                tf.name,
            ]
        )


class Aarch64QNX(QNX):
    def __init__(self):
        super(Aarch64QNX, self).__init__()

    @property
    def is_64bit(self):
        return True

    @property
    def target(self):
        return "aarch64-qnx"

    @property
    def name(self):
        return "qnx"

    @property
    def system_ads(self):
        return {
            "light": "system-qnx-arm-light.ads",
            "light-tasking": "system-qnx-arm-light-tasking.ads",
        }

    def amend_rts(self, rts_profile, cfg):
        cfg.build_flags["common_flags"] += [
            # The traceback implementation in our restricted runtimes
            # for this platform relies on all frames having a frame
            # pointer, so make sure it is always there.
            # See V217-008 for more info.
            "-fno-omit-frame-pointer",
        ]
        cfg.build_flags["shared_linker_flags"] += [
            # Add an explicit dependency on libada_lch so that the last
            # chance handler is loaded when we use the Ada runtime.
            "-lada_lch",
        ]


class ARMQNX(QNX):
    def __init__(self):
        super(ARMQNX, self).__init__()

    @property
    def is_64bit(self):
        return False

    @property
    def target(self):
        return "arm-qnx"

    @property
    def name(self):
        return "qnx"

    @property
    def system_ads(self):
        return {
            "light": "system-qnx-arm-light.ads",
            "light-tasking": "system-qnx-arm-light-tasking.ads",
        }
