#
# Copyright (C) 2025-2026, AdaCore
#

import types
from importlib import resources
from pathlib import Path

# TODO This function should be just a call to the resolver as well
# The resolver should make sure of having a step that looks in the
# package data files.
# Currently the resolver seem to deal be able to find sounces from wheel
# package just fine, not sure if this is needed at all with the modern
# but apparently we can start seeing problems with zipped packages.
# or other packaging usage cases. Leaving it here until we are sure
# it is not needed.


def read_package_file(package: str | types.ModuleType | None, rel_path: Path) -> str:
    """
    Return a real pathlib.Path to a resource inside a package, even if the
    package is zipped. The returned path is a temporary copy if needed.

    Caller is responsible for deleting the file if it's temporary.


    :param package: The package anchor (e.g. __package__ or module object).
    :param rel_path: Relative path inside that package.
    :return:
    """

    if package is None:
        # We still keep None type possitble to allow __package__ usage
        # by called (can be None)
        raise ValueError("Given package cannot be located")

    if rel_path.is_absolute():
        raise ValueError(f"rel_path must be relative, got: {rel_path}")

    res = resources.files(package).joinpath(*rel_path.parts)

    return res.read_text(encoding="utf-8")
