import sys
import os

sys.path.append(os.path.dirname(os.path.dirname(__file__)))


def fullpath(filename):
    """Returns the full path of filename.

    If filename is a relative path, then returns the path relative to the
    script's directory.
    """
    if os.path.isabs(filename):
        return filename
    else:
        return os.path.abspath(
            os.path.join(os.path.dirname(os.path.dirname(__file__)),
                         filename))


def readfile(filename):
    """Reads the content of filename, relative to the bb-runtimes directory"""
    fp = open(fullpath(filename), 'r')
    res = fp.read()
    fp.close()
    return res
