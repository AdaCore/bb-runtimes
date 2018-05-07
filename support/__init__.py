import sys
import os

sys.path.append(os.path.dirname(os.path.dirname(__file__)))

DATA_DIR = os.path.join(os.path.dirname(__file__), 'data')
REPO_DIR = os.path.abspath(os.path.dirname(os.path.dirname(__file__)))
_SRC_SEARCH_PATH = [REPO_DIR, ]


def add_source_search_path(path):
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


def datapath(filename):
    return os.path.join(DATA_DIR, filename)


def readfile(filename):
    """Reads the content of filename, relative to the bb-runtimes directory"""
    fp = open(fullpath(filename), 'r')
    res = fp.read()
    fp.close()
    return res
