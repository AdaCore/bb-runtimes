#
# Copyright (C) 2026, AdaCore
#

from pathlib import Path
from string import Template as BuiltinTemplate

from .infrastructure_interface import resolve_and_read_file


class Template:
    """Load and render a template with named parameters.

    .. note::

        The rendering engine can be replaced with Jinja or another template
        engine without changing this interface.
    """

    def __init__(self, path: Path) -> None:
        self._template = BuiltinTemplate(resolve_and_read_file(path))

    @property
    def parameters(self) -> set[str]:
        return set(self._template.get_identifiers())

    def render(self, values: dict[str, str]) -> str:
        """Substitute values, raising KeyError for a missing parameter."""
        return self._template.substitute(values)
