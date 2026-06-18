#
# Copyright (C) 2025-2026, AdaCore
#

"""Pre-build step for QNX runtimes.

Installed at the runtime root as pre_build.py; build.py imports it and calls
prebuild_step_run() before gprbuild.
"""

import os
import subprocess
import tempfile


def prebuild_step_run(obj_dir: str) -> None:
    # Create a dummy shared library named after the shared last chance handler
    # so the runtime can link against it. The temporary file is the empty
    # translation unit; delete_on_close is needed on Windows to be able to read
    # the file (otherwise we get a permission denied).
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
