.. Copyright (C) 2025-2026, AdaCore

===========
Functional
===========

The bb-runtimes repository generates restricted Ada runtime system (RTS)
source trees for bareboard and OS targets. This documentation covers its two
Python packages and how they work together. ``rts_prebuilder`` provides the
generation engine. ``bb_runtimes_targets_gen`` supplies AdaCore's target
definitions and components, which select sources from ``src/`` and other
source directories.

The ``bb`` in the name reflects the repository's bareboard origins. Its scope
now includes OS targets.

What is a Runtime System (RTS)?
===============================

See `GNAT runtime <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/gnat_runtimes.html>`_
and `Customized bareboard runtime libraries <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/customized_run-time_libraries.html#customized-bareboard-run-time-libraries>`_.

Key concepts
============

Six terms appear throughout these pages. They are ordered so each one builds on
the ones above it.

Target
   A Python class describing one concrete target, be it a board, a CPU or an
   operating system on an architecture: its platform, the profiles it offers,
   its hardware traits and its gprbuild target triplet. It is the input the
   whole generation runs from. It also carries its own
   always-on components, on top of those its profiles bring. See
   :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget`.

Component
   A coherent group of runtime sources and switches. Components stay
   profile-unaware. See
   :class:`~rts_prebuilder.abstract_infrastructure.SourcesAndFlagsComponent`.

Base profile
   The runtime family, which fixes the Ada subset a runtime supports: whether
   it has tasking, which exception model it uses, which certification
   constraints apply. The main runtime families are ``light``,
   ``light-tasking`` and ``embedded``
   (:data:`~rts_prebuilder.base_types.ValidBaseProfileType`). It drives which
   components a runtime gets.

Profile
   One runtime a target offers: a base profile made concrete for that one
   target. In Ada terms it is one run-time library, the directory a user
   selects with ``for Runtime ("Ada") use "light-tasking-stm32f4";`` or with
   ``--RTS=``. That runtime is the ``light-tasking`` base profile carrying the
   STM32F4's clock, memory map and interrupt sources, and nothing from any
   other target. Its name is the profile's name plus the target's, so a target
   offering several profiles ships several runtimes. As data, a profile is that
   name, a base profile, a ``system.ads``, and the components that make up the
   runtime. See :class:`~rts_prebuilder.abstract_infrastructure.Profile`.

Platform
   The operating system or bareboard environment a target runs on. One of
   ``bb`` (bareboard, no OS), ``deos``, ``freertos``, ``linux``, ``lynx``,
   ``pikeos``, ``vx7r2cert`` or ``qnx``
   (:data:`~rts_prebuilder.base_types.PlatformIdType`). Components read it to
   pick platform-specific sources.

Infrastructure
   The code that supplies all of the above. ``rts_prebuilder`` ships only the
   abstract infrastructure: the contracts a target, a component and a generator
   must fill. A concrete infrastructure fills them for one family of runtimes.
   ``bb_runtimes_targets_gen``, which prebuilds the AdaCore bareboard runtimes,
   is one such infrastructure. A site with its own boards and its own source
   layout writes another.

Prebuilding, and then building
==============================

Generating and compiling a restricted Ada runtime takes two separate steps.

Prebuilding (``rts_prebuilder``)
   Takes a target description and generates target-specific runtime directories
   with their build configuration. For each profile, the target's components are
   resolved for that profile, and the resulting sources are gathered from the
   source repositories the caller points at.

Building (generated build script)
   Compiles a prebuilt runtime directory using an Ada compiler. The prebuilder
   makes this possible but plays no part in it. A prebuilt tree can equally be
   edited by hand and rebuilt later, without going through the prebuilder
   again.

.. _functional_diagram:
.. mermaid::

   flowchart TB
      subgraph Prebuilding["<b>Prebuilding</b><br><i>rts_prebuilder Python package</i>"]
         direction TB
         Prebuilder["RuntimePrebuildDriver"]
         PrebuiltRuntime@{ shape: docs, label: "Target-specific runtime<br>with its generated .gpr files" }
      end
      subgraph Building["<b>Building</b><br><i>(external tool)</i>"]
         direction TB
         BuildScript["Build script"]
         Runtime@{ shape: docs, label: "Compiled<br>runtime" }
      end
      subgraph TargetDefinitions["<b>bb_runtimes_targets_gen</b><br><i>Python package</i>"]
         Target["Target"]
      end
      Prebuilder --> PrebuiltRuntime
      BuildScript --> Runtime
      DB[("Any source directory")] -- Provides sources --> Prebuilder
      Target -- Configures --> Prebuilder
      PrebuiltRuntime -- Input to --> BuildScript
      TargetSources[("bb-runtimes/src/<br>(target-specific sources)")] -- Provides sources --> Prebuilder
      Target -. Selects sources from .-> DB
      Target -. Selects sources from .-> TargetSources
      style Prebuilding fill:#f0f8ff,stroke:#0066cc,stroke-width:4px
      style Runtime fill:#c5e1a5,stroke:#689f38
      style DB fill:#bbdefb,stroke:#1976d2
      style Target fill:#ffe0b2,stroke:#e65100
      style TargetDefinitions fill:#dbfaf4,stroke:#1abc9c,stroke-width:2px
      style TargetSources fill:#bbdefb,stroke:#1976d2
      style Building fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px

The target definition selects the build scripts. The prebuilder copies them
into the generated runtime directory. Run the build script separately to
compile the runtime.

.. admonition:: Specificity of ``bb_runtimes_targets_gen``
   :class: bb-runtimes-specific

   ``bb_runtimes_targets_gen`` supplies the target definitions shown in the
   diagram. It installs a ``build.py`` and a ``prepare_build.py`` in every
   runtime. Run ``build.py`` to compile the generated tree.

Where do you start with bb-runtimes?
======================================

Answer each question top-down. Click a leaf to jump to the matching guide.

.. mermaid::

   flowchart TD
      Q1{"You already have a target-specific runtime to use as-is, or to edit and rebuild?"}
      Q1 -- Yes --> RB["<b>Runtime builder</b> &nbsp;—&nbsp; Out of scope for rts_prebuilder (see GNAT UG: Customized Run-Time Libraries instead)"]
      Q1 -- No --> Q2{"You want to prebuild a runtime for a target that an existing infrastructure (such as bb_runtimes_targets_gen) already supports?"}
      Q2 -- Yes --> EU["<b>End user</b> &nbsp;—&nbsp; See End Users Guide. You may skip the rest of this documentation."]
      Q2 -- No --> Q3{"Can you meet your needs by extending existing target definitions?"}
      Q3 -- Yes --> EXT["<b>Concrete Infrastructure Extender</b> &nbsp;—&nbsp; See Extending with Your Own Target. Read the rest of the overview first."]
      Q3 -- No --> BLD["<b>Concrete Infrastructure Builder</b> &nbsp;—&nbsp; See Builders Guide. Read the rest of the overview first."]
      click RB "https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/customized_run-time_libraries.html" _blank
      click EU "../user_guides/end_users_guide.html"
      click EXT "../user_guides/extending_with_your_own_target.html"
      click BLD "../user_guides/concrete_infrastructure_builders.html"
      style RB fill:#ffeb3b,stroke:#f57f17,stroke-width:3px
      style EU fill:#c8e6c9,stroke:#388e3c,stroke-width:2px
      style EXT fill:#bbdefb,stroke:#1976d2,stroke-width:2px
      style BLD fill:#ffe0b2,stroke:#e65100,stroke-width:2px
      style Q1 fill:#fffde7,stroke:#f9a825
      style Q2 fill:#fffde7,stroke:#f9a825
      style Q3 fill:#fffde7,stroke:#f9a825
