# bb-runtimes

## What does this repository generate?

This repository generates source trees programmatically for AdaCore's restricted
Ada runtimes on bareboard and OS targets.

The `bb` in the name reflects the repository's bareboard origins. Its scope now
includes OS targets.

> [!NOTE]
> If you already have a runtime source tree for a given target and only want to compile it,
> follow that runtime's build instructions. You will not need this repository.

## User guide

If you want to regenerate a restricted runtime source tree for target
already defined in `./support/bb_runtimes_targets_gen/`. Then follow these
steps:

Use Python 3.12 or later. From the repository root, install both packages in a
virtual environment:

```sh
python3 -m venv .venv
. .venv/bin/activate
python -m pip install ./support/rts_prebuilder_project \
    ./support/bb_runtimes_targets_gen
```

List the supported targets:

```sh
python -m bb_runtimes_targets_gen.targets --list-targets
```

Generate the runtimes for one target, such as `zynqmp`:

```sh
python -m bb_runtimes_targets_gen.targets zynqmp \
    --source-search-path src \
    --source-search-path ../gnat \
    --source-search-path ../gcc \
    --output-dir generated
```

This example uses GNAT and GCC source trees in `../gnat` and `../gcc`.
Adjust the paths to match your source locations.
The output contains `generated/light-zynqmp`, `generated/light-tasking-zynqmp`,
and `generated/embedded-zynqmp`.

For more options, run `python -m bb_runtimes_targets_gen.targets --help` or see
the [user guide](https://bb-runtimes.readthedocs.io/en/latest/user_guides/end_users_guide.html).

## Developer guide

The developer guides address two questions:

* [How do I extend target definitions for my board, CPU, or platform?](https://bb-runtimes.readthedocs.io/en/latest/user_guides/extending_with_your_own_target.html)
  You will also need the [Target API reference](https://bb-runtimes.readthedocs.io/en/latest/api/concrete_infrastructure_extenders/target_definition.html)
  as you implement your target.
* [How do I set up my own runtime generator from scratch?](https://bb-runtimes.readthedocs.io/en/latest/user_guides/concrete_infrastructure_builders.html)
  Advanced usage only. Before proceeding, reconsider whether extending an
  existing target definition meets your needs.

For an overview of the project, start with the
[introduction](https://bb-runtimes.readthedocs.io/en/latest/overview/1_functional.html).

