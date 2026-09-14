.. Copyright (C) 2025-2026, AdaCore

===============
End Users Guide
===============

This guide explains how to generate a buildable runtime with ``rts_prebuilder``
and an existing infrastructure, such as ``bb_runtimes_targets_gen``.

.. admonition:: Other guides
   :class: routing-box

   * Extending target definitions: :doc:`extending_with_your_own_target`.
   * Building a new infrastructure from scratch:
     :doc:`concrete_infrastructure_builders`.

Prerequisites
=============

* A concrete infrastructure package, installed and importable. It supplies the
  targets; ``rts_prebuilder`` ships none.
* The directories where the sources live. A target names its sources by relative
  path, and you say where to look, with ``--source-search-path`` once per
  directory.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   ``bb_runtimes_targets_gen`` is a concrete infrastructure package. Say you want
   the runtimes for a ZynqMP board. Somebody has already written that target, so
   there is nothing for you to add: the class declares the board's name, the
   three profiles it offers, its loaders and its sources. How it does that is the
   extender's business, not yours.

   The target names only the files this board adds, and they all sit in the
   bb-runtimes ``src/`` directory. The rest of the runtime comes from the common
   components, which name units the compiler ships. You hand in a directory for
   each::

      python -m bb_runtimes_targets_gen.targets zynqmp \
          --source-search-path src \
          --source-search-path ../gnat

What drives a run
=================

:class:`~rts_prebuilder.engine.RuntimePrebuilderCLI` is the ``rts_prebuilder``
front end.
An infrastructure normally wraps it in an entry point of its own, and that
wrapper is what you run. Run it with ``--help`` for the flags, and with
``--list-targets`` for the targets it registers.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   Every architecture module is runnable, and so is the ``targets`` package that
   collects them all::

      python -m bb_runtimes_targets_gen.targets --list-targets
      python -m bb_runtimes_targets_gen.targets.aarch64 --list-targets
      python -m bb_runtimes_targets_gen.targets.arm --help

   One entry point serves them all: the same small ``__main__.py`` sits in each
   architecture subpackage, and each subpackage exports its own ``TARGETS``. That
   split by architecture is this infrastructure's convention, not something
   ``rts_prebuilder`` requires.

What you get
============

One directory per generated profile, under the output directory. The default name
is ``<profile>-<target>``, which a target may override.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   The ZynqMP board offers three profiles, so the run above writes three
   directories::

      light-zynqmp/
      light-tasking-zynqmp/
      embedded-zynqmp/

   The OS targets override the naming instead. Linux, QNX, LynxOS, Deos, PikeOS,
   VxWorks 7 cert and Windows runtimes install under the legacy
   ``rts-<base_profile>`` name, because that is what their toolchains already
   expect. VxWorks 7 cert appends its RTP suffix to that name.

Written by ``rts_prebuilder``:

* ``gnat/`` and, for a tasking runtime, ``gnarl/``: the Ada and C sources, with
  ``system.ads`` among them.
* ``ld/``: the linker scripts, when the target ships any.
* ``gnat_user/``, ``ld_user/`` and, for a tasking runtime, ``gnarl_user/``:
  created empty, for your own files. ``gnat_user/`` and ``gnarl_user/`` come
  first on the runtime's source path, so a file you drop in overrides the one
  the prebuilder installed.
* ``runtime.xml``: what gprbuild reads to compile and link against this runtime.
* ``target_options.gpr`` and ``runtime_build.gpr``, the projects that build the
  runtime itself, plus ``ravenscar_build.gpr`` for a tasking one.

Written by the infrastructure, through ``rts_prebuilder``: the build scripts,
and any file it installs at the runtime root.

Building what you generated
===========================

Compiling the tree is a separate step, outside ``rts_prebuilder``. How you invoke
it is the infrastructure's decision. You can edit the tree first and rebuild as
often as you like without going back through the prebuilder.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   Each generated runtime ships a ``build.py``. Run it with the right compiler on
   the ``PATH``. Use it for non-default options too: it runs the pre-build step,
   builds every project of the runtime in the right order and cleans up after
   itself, and ``--build-flags`` forwards what you pass to ``gprbuild``.
   ``-XBUILD=Debug`` builds with debug information, ``-XBUILD=Assert`` with
   assertions::

      python embedded-zynqmp/build.py --build-flags=-XBUILD=Debug

Driving it from your own script
===============================

If the infrastructure's entry point does not fit your workflow, build a
:class:`~rts_prebuilder.engine.RuntimePrebuildDriver` per target, call
``create_runtimes_writers()``, then ``install_runtimes(destination)``.

Either way the infrastructure must be registered first: its
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`
instance must have called
:meth:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface.self_register`.
See :ref:`infrastructure_registration`.

The ``rts_prebuilder`` API reference is in :doc:`../api/end_users_api`.
