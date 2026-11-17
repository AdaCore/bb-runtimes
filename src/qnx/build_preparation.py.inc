#
# Copyright (C) 2025-2026, AdaCore
#


def prepare_build(obj_dir: str) -> None:
    """Create the shared last-chance-handler library needed for runtime linking."""
    import os
    import subprocess
    import tempfile

    # Use an empty translation unit to create the dummy library needed for linking.
    # Set delete_on_close=False so the compiler can reopen the file on Windows
    # while Python keeps it open (Python 3.13 docs, tempfile.NamedTemporaryFile).
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
