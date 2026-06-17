#
# Copyright (C) 2025-2026, AdaCore
#

"""
Module to locate the rts-sources.json file for a given target, when not given
by the user.
Please be verbose enough in logging, so the user can understand what
exactly what is tried and what failed.
"""

import os
import shutil
import subprocess
import sys
from pathlib import Path

from rts_prebuilder.abstract_infrastructure import AbstractTarget
from rts_prebuilder.end_user_data.compiler_selector import using_llvm_compiler
from rts_prebuilder.end_user_data.logger import get_logger

log = get_logger(__name__)


def locate_json_descriptor(target: AbstractTarget) -> Path | None:
    """
    Locate the rts-sources.json file for the given target using gprls
    or manual deduction.

    :param target: Object needed to fetch some properties
    :return: The path to the json descriptor
    """
    log.info("Auto-locating rts-sources.json for target %s", target)

    try:
        if not target.is_native:
            gprls_output = subprocess.check_output(
                ["gprls", "-v", f"--target={target}"],
            ).decode()
        else:
            gprls_output = subprocess.check_output(
                ["gprls", "-v"], stderr=subprocess.STDOUT
            ).decode()
    except FileNotFoundError as e:
        log.warning("Could not run gprls %s", e)
        gprls_output = None

    except subprocess.CalledProcessError as e:
        log.warning(
            "gprls returned non-zero exit code %s. Output:\n%s",
            e,
            e.output.decode(),
        )
        gprls_output = None

    in_prj_search_path = False
    descriptor_path = None

    if gprls_output is not None:
        for line in gprls_output.splitlines():
            if not in_prj_search_path:
                if line == "Project Search Path:":
                    in_prj_search_path = True
                continue
            line = line.strip()
            if line == "<Current_Directory>":
                continue
            if len(line) == 0:
                break

            tentative = Path(line) / "rts-sources.json"

            if os.path.exists(tentative):
                descriptor_path = Path(tentative)
                break

    # If gprls did not work, try to find out from compiler path
    if descriptor_path is None:
        log.warning("gprls did not help to locate rts-sources.json")

    if descriptor_path is None:
        descriptor_path = _locate_json_descriptor_from_compiler_path(target)

    if not descriptor_path:
        log.error("Could not auto-locate rts-sources.json for target %s. ", target)
        sys.exit(1)

    if descriptor_path and not descriptor_path.exists():
        log.error(
            "Auto-located rts-sources.json at %s but it does not exist on disk.",
            descriptor_path,
        )
        sys.exit(1)

    log.info("Auto-located rts-sources.json at %s", descriptor_path)

    return descriptor_path


def _locate_json_descriptor_from_compiler_path(target: AbstractTarget) -> Path | None:
    """
    Locate the rts-sources.json file for the given target using gprls
    or manual deduction.

    :param descriptor: The descriptor (usually named rts-sources.json)
                        which holds information about the source location
    :return: The path to the json descriptor
    """
    log.info(
        "Trying to locate rts-sources.json using compiler path deduction",
    )
    # First look in the relative path
    compiler_name_pattern = "%s-llvm-gcc" if using_llvm_compiler() else "%s-gcc"
    compiler_bin_str = shutil.which(compiler_name_pattern % target.target)

    if not compiler_bin_str:
        log.warning(
            "Could not find compiler binary %s in PATH",
            compiler_name_pattern % target.target,
        )
        return None

    compiler = Path(compiler_bin_str)

    lib_dir = "gnat-llvm" if using_llvm_compiler() else "gnat"
    json_desc_path: Path = (
        compiler.parent / target.target / "lib" / lib_dir / "rts-sources.json"
    )

    if json_desc_path.exists():
        log.info(
            "Located rts-sources.json at %s using compiler path deduction",
            json_desc_path,
        )
        return json_desc_path

    log.warning(
        "Could not locate rts-sources.json at %s using compiler path deduction",
        json_desc_path,
    )

    return None
