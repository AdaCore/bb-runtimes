#
# Copyright (C) 2026, AdaCore
#

"""Exercise RuntimePrebuilderCLI's argument parsing and orchestration, minus
the disk-writing install path (covered by end-to-end/manual testing instead).
"""

import argparse
import logging
from pathlib import Path
from typing import override

import pytest
from rts_prebuilder.abstract_infrastructure import (
    AbstractTarget,
    AbstractTargetGenerator,
    Profile,
)
from rts_prebuilder.engine.cli import RuntimePrebuilderCLI


class _StubRuntimeXml:
    """Minimal runtime_xml_source stand-in: the generator only calls render()."""

    def render(self) -> str:
        return "<runtime/>"


class _FakeTarget(AbstractTarget):
    def __init__(self, name="faketarget"):
        super().__init__()
        self._name = name

    @property
    @override
    def name(self):
        return self._name

    @property
    @override
    def platform(self):
        return "bb"

    @property
    @override
    def has_double_precision_fpu(self):
        return False

    @property
    @override
    def profiles(self):
        return [
            Profile(
                name="light",
                base_profile="light",
                system_ads=Path("system_light.ads"),
            )
        ]

    @property
    @override
    def target(self):
        return f"{self._name}-elf"

    @property
    def runtime_xml_source(self):
        return _StubRuntimeXml()


class _FakeGenerator(AbstractTargetGenerator):
    def __init__(self, pattern, variants, instances=None):
        self._pattern = pattern
        self._variants = variants
        self._instances = instances or {}

    @property
    @override
    def cli_name_pattern(self):
        return self._pattern

    @override
    def generate_variants(self):
        yield from self._variants

    @override
    def instantiate(self, name):
        return self._instances.get(name)


def _base_args(**overrides):
    defaults = {
        "target": [],
        "list_targets": False,
        "output_dir": None,
        "gcc_dir": None,
        "gnat_dir": None,
        "compiler": "gnat",
        "overwrite": False,
        "verbose": False,
        "link": False,
        "profiles": None,
        "source_search_path": [],
    }
    defaults.update(overrides)
    return argparse.Namespace(**defaults)


# --- create_parser ----------------------------------------------------------


def test_parser_profiles_splits_comma_list():
    cli = RuntimePrebuilderCLI()
    parser = cli.create_parser(prog="p", description="d")
    args = parser.parse_args(["--profiles", "light, light-tasking"])
    assert args.profiles == ["light", "light-tasking"]


def test_parser_source_search_path_is_repeatable():
    cli = RuntimePrebuilderCLI()
    parser = cli.create_parser(prog="p", description="d")
    args = parser.parse_args(
        ["--source-search-path", "/a", "--source-search-path", "/b"]
    )
    assert args.source_search_path == [Path("/a"), Path("/b")]


# --- register_targets ---------------------------------------------------


def test_register_targets_splits_concrete_and_generators():
    target = _FakeTarget()
    gen = _FakeGenerator("fam{0,1}", ["fam0", "fam1"])
    cli = RuntimePrebuilderCLI.register_targets(target, gen)
    assert cli._targets[target.cli_name] is target
    assert gen in cli._generators


def test_register_targets_rejects_unsupported_type():
    with pytest.raises(TypeError):
        RuntimePrebuilderCLI.register_targets("not a target")  # type: ignore[arg-type]


def test_register_targets_accumulates_across_calls():
    t1 = _FakeTarget("t1")
    t2 = _FakeTarget("t2")
    RuntimePrebuilderCLI.register_targets(t1)
    cli = RuntimePrebuilderCLI.register_targets(t2)
    assert set(cli._targets) == {"t1", "t2"}


# --- run() ---------------------------------------------------------------


def test_run_without_registration_raises_runtime_error():
    with pytest.raises(RuntimeError, match="No targets registered"):
        RuntimePrebuilderCLI.run()


# --- run_prebuilder branches that don't touch disk ------------------------


def test_run_prebuilder_list_targets_returns_empty_and_prints(capsys):
    target = _FakeTarget()
    cli = RuntimePrebuilderCLI.register_targets(target)
    args = _base_args(list_targets=True)

    result = cli.run_prebuilder(args)

    assert result == []
    assert target.name in capsys.readouterr().out


def test_run_prebuilder_no_target_raises_system_exit():
    cli = RuntimePrebuilderCLI.register_targets(_FakeTarget())
    args = _base_args(target=[])

    with pytest.raises(SystemExit, match="No target specified"):
        cli.run_prebuilder(args)


def test_run_prebuilder_unknown_target_raises_system_exit(tmp_path):
    cli = RuntimePrebuilderCLI.register_targets(_FakeTarget())
    args = _base_args(target=["doesnotexist"], gcc_dir=tmp_path, gnat_dir=tmp_path)

    with pytest.raises(SystemExit, match="Unknown target"):
        cli.run_prebuilder(args)


def test_run_prebuilder_resolves_target_via_generator(tmp_path, caplog):
    variant_target = _FakeTarget("fam0")
    gen = _FakeGenerator("fam{0,1}", ["fam0", "fam1"], {"fam0": variant_target})
    cli = RuntimePrebuilderCLI.register_targets(gen)
    # No profile matches the target's profiles: create_runtimes_writers() creates
    # nothing, so install_runtimes() (disk I/O) is never reached.
    args = _base_args(
        target=["fam0"],
        gcc_dir=tmp_path,
        gnat_dir=tmp_path,
        profiles=["dummy-inexistant-profile"],
    )

    with caplog.at_level(logging.WARNING):
        result = cli.run_prebuilder(args)

    assert result == []
    assert "fam0" in caplog.text


@pytest.mark.parametrize(
    "variant_count, selection, expected",
    [
        (11, "--all", ["concrete", "fam0", "fam2", "fam5", "fam7", "fam10"]),
        (3, "--all", ["concrete", "fam0", "fam1", "fam2"]),
        (11, "fam6", ["fam6"]),
    ],
)
def test_target_selection(tmp_path, caplog, variant_count, selection, expected):
    concrete = _FakeTarget("concrete")
    variants = {f"fam{i}": _FakeTarget(f"fam{i}") for i in range(variant_count)}
    generator = _FakeGenerator("fam{0..10}", list(variants), variants)
    cli = RuntimePrebuilderCLI.register_targets(concrete, generator)
    parser = cli.create_parser(prog="p", description="d")
    args = parser.parse_args(
        [
            selection,
            "--profiles",
            "dummy-inexistant-profile",
            "--gnat-dir",
            str(tmp_path),
            "--gcc-dir",
            str(tmp_path),
        ]
    )

    with caplog.at_level(logging.WARNING):
        assert cli.run_prebuilder(args) == []

    skipped = [
        record.args[0]
        for record in caplog.records
        if record.msg == "No runtimes created for target %s"
    ]
    assert skipped == expected


def test_all_rejects_explicit_target_names():
    cli = RuntimePrebuilderCLI.register_targets(_FakeTarget())
    parser = cli.create_parser(prog="p", description="d")
    args = parser.parse_args(["--all", "faketarget"])

    with pytest.raises(SystemExit, match="Use --all or explicit target names"):
        cli.run_prebuilder(args)
