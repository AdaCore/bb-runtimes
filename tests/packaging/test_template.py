#
# Copyright (C) 2026, AdaCore
#

from pathlib import Path

import pytest

import rts_prebuilder.engine.template as template_engine
from bb_runtimes_targets_gen.concrete_infrastructure.engine_interface import (
    engine_interface,
)
from rts_prebuilder.abstract_infrastructure import TemplateFile
from rts_prebuilder.base_types import UnresolvedPath
from rts_prebuilder.engine.file_pair import TemplateFilePair


def test_template_parameters_and_literal_braces(monkeypatch):
    monkeypatch.setattr(
        template_engine, "resolve_and_read_file", lambda path: "{ ${value} } $$"
    )
    template = template_engine.Template(Path("input.tmpl"))
    assert template.parameters == {"value"}
    assert template.render({"value": "{unchanged}"}) == "{ {unchanged} } $"
    with pytest.raises(KeyError, match="value"):
        template.render({})


@pytest.mark.parametrize("link", [False, True])
def test_template_installation(tmp_path, link):
    engine_interface.self_register()
    source = tmp_path / "source.S.tmpl"
    source.write_text("${value}\n", encoding="utf-8")
    declaration = TemplateFile(
        UnresolvedPath(source), dest_subdir=Path("gnat"), dest_path=Path("output.c")
    )
    file_pair = TemplateFilePair(declaration, {"value": "rendered"})
    assert file_pair.is_in_dir(Path("gnat"))
    assert file_pair.suffixes == {".c"}
    output = tmp_path / "runtime"
    file_pair.install(output, link)
    destination = output / "gnat" / "output.c"
    assert destination.read_bytes() == b"rendered\n"
    assert not destination.is_symlink()
    with pytest.raises(FileExistsError):
        file_pair.install(output, link)
