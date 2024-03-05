import sys
import os
import re
from enum import Enum

sys.path.append(os.path.dirname(os.path.dirname(__file__)))

# The set of compilers that we can assemble runtimes for.
Compiler = Enum("Compiler", ["gnat", "gnat_llvm"])

DATA_DIR = os.path.join(os.path.dirname(__file__), "data")
REPO_DIR = os.path.abspath(os.path.dirname(os.path.dirname(__file__)))
_SRC_SEARCH_PATH = [
    REPO_DIR,
]
_TARGET_COMPILER = Compiler.gnat


def get_gnat_version(gnat_dir):
    try:
        with open(os.path.join(os.path.abspath(gnat_dir), "gnatvsn.ads"), "r") as fd:
            gnatvsn_content = fd.read()
    except Exception:
        print("cannot find gnatvsn.ads")
        sys.exit(1)
    m = re.search(
        r"Library_Version : " + r'constant String := "([0-9]+)(\.([0-9]+))?";',
        gnatvsn_content,
    )
    if m:
        version = m.group(1).strip().split(".")[0]
        date = m.group(3)
        if date:
            return f"{version}.{date}"
        return version

    print("cannot find GNAT version in gnatvsn.ads")
    sys.exit(1)


def add_source_search_path(path):
    """Adds an additional default search path for source files"""
    abspath = os.path.abspath(path)

    if abspath not in _SRC_SEARCH_PATH:
        _SRC_SEARCH_PATH.append(abspath)


def fullpath(filename):
    """Returns the full path of filename.

    If filename is a relative path, then returns the path relative to the
    script's directory.
    """
    if os.path.isabs(filename):
        return filename

    for p in _SRC_SEARCH_PATH:
        abspath = os.path.join(p, filename)
        if os.path.exists(abspath):
            return abspath

    return os.path.join(REPO_DIR, filename)


def getdatafilepath(filename):
    """Retrieves the path of filename in the data directory"""
    return os.path.join(DATA_DIR, filename)


def readfile(filename):
    """Reads the content of filename, relative to the bb-runtimes directory"""
    fp = open(fullpath(filename), "r")
    res = fp.read()
    fp.close()
    return res


def is_string(arg):
    """Handles differencies in behavior between python2 and 3 concerning
    strings

    This checks the type of arg against basestring on python2 and against
    str on python3"""
    if sys.version_info[0] < 3:
        # Ignore F821 (undefined name) as basestring is
        # only defined in python2
        return isinstance(arg, basestring)  # noqa: F821
    else:
        return isinstance(arg, str)


def set_target_compiler(comp):
    """Set the compiler that we are assembling runtimes for"""
    assert comp in Compiler
    global _TARGET_COMPILER
    _TARGET_COMPILER = comp


def target_compiler():
    return _TARGET_COMPILER


def using_llvm_compiler():
    return target_compiler() == Compiler.gnat_llvm
