.. Copyright (C) 2025-2026, AdaCore

===========
Functional
===========

The RTS Prebuilder is a **library** for programmatically assembling restricted runtime system (RTS) source trees and configuring them for specific targets. It provides a flexible framework for generating Ada runtime sources and targeting them to new hardware platforms.

What is a Runtime System (RTS)?
===============================

See `GNAT runtime <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/gnat_runtimes.html>`_

See also `Customized bareboard runtimes libraries <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/customized_run-time_libraries.html#customized-bareboard-run-time-libraries>`_

Note that this library is not limited to bareboard runtimes, it can be used to assemble and target any kind of Ada runtime system.

Key concepts
============

Three terms appear throughout the phase descriptions below:

* **Base profile** — one of ``light``, ``light-tasking``, ``embedded`` ...
  It selects the flavor of the restricted Ada runtime (tasking? exception
  model? certification constraints? etc ...).

* **Platform** — a preselection of source files applicable to a given operating
  system or bareboard environment. One of ``bb`` (bareboard), ``lynx``,
  ``freertos``, ``linux``, etc ...

* **Target** — a Python class describing one concrete board / CPU. Declares its
  platform, supported profiles, build flags, and gprbuild target name. This is
  the main descriptor used to generate the final runtime.

The Three-Phase Process to build a customizable runtime:
========================================================

Assembling and Building a target specific RTS is done in three distinct phases:

**1. Runtime Sources Assembly:**
Collects Ada source files from source repositories and places them in a dedicated RTS source
tree based on a **base profile** and a **platform**  selections. 

**2. Runtime Source Tree Targetization:**
Takes the assembled source tree and a **Target** description, then generates target-specific runtime directories with build configurations.
This phase requires the target to customize the runtime for specific hardware/OS constraints.

**3. Runtime Building:**
Uses the ``build.py`` script to compile the configured runtime. **This phase is outside the scope of the rts_prebuilder package** but is made possible by the generated build.py script.

.. _functional_diagram:
.. mermaid::

  flowchart TB
    subgraph Phase1["<b>Phase 1: Assembly</b>"]
        direction TB
            Assembler["RuntimeAssembler"]
            Tree["Raw sources assembed in a tree<br>+ Metadata JSON <br>(rts-sources.json)"]
      end
    subgraph Phase2["<b>Phase 2: Targetization</b>"]
        direction TB
            Targetizer["RuntimeTargetizer"]
            TargetizedRuntime["Target specific runtime  <br> With ready to build(.gpr files)"]
      end
    subgraph RTSPrebuilder["<b>RTS Prebuilder Scope</b>"]
        direction TB
            Phase1
            Phase2
      end
    subgraph Phase3["<b>Phase 3: Building</b><br><i>(External Tool)</i>"]
        direction TB
            GPRBuild["build.py"]
            Runtime["Compiled<br>Runtime"]
      end
        Assembler --> Tree
        Targetizer --> TargetizedRuntime
        Tree -. Input to .-> Targetizer
        GPRBuild --> Runtime
        DB[("External repositories<br> (gnat, gcc)")] -- Provides sources --> Assembler
        Profile["Highest Base Profile<br>(light, embedded, cert, ...)"] -- Selection argument --> Assembler
        Platform["Platform<br>(bb, pikeos, linux, ...)"] -- Selection argument --> Assembler
        Target["Target<br>(Board Definition)"] -- "Selects sources, both from Assembly output using scenarios, and directly from target repo. Also defines build flags and config files" --> Targetizer
        TargetizedRuntime -- Input to --> GPRBuild
        n1[("Sources from target repository (bb-runtimes, certified-rts ...)")] -- Provides sources --> Targetizer
        n3["Target name"]
        style Phase1 fill:#e1f5ff,stroke:#0288d1,stroke-width:2px
        style Phase2 fill:#fff3e0,stroke:#f57c00,stroke-width:2px
        style Runtime fill:#c5e1a5,stroke:#689f38
        style DB fill:#bbdefb,stroke:#1976d2
        style Profile fill:#c8e6c9,stroke:#388e3c
        style Platform fill:#c8e6c9,stroke:#388e3c
        style Target fill:#ffe0b2,stroke:#e65100
        style n1 fill:#bbdefb,stroke:#1976d2
        style RTSPrebuilder fill:#f0f8ff,stroke:#0066cc,stroke-width:4px,stroke-dasharray: 5 5
        style Phase3 fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px


.. note::

   Historically, Step 1 was done in a `gen-rts-sources.py` script in the `bb-runtimes` repository, while
   step 2 and 3 were done in one step by a `build_rts.py` script (step 3 only with its `--build` option).
   That script has since been removed: this package modularizes and generalizes these steps, targetization
   now runs through the per-arch entry points (`python -m bb_runtimes_targets_gen.targets.<arch>`) and the
   build through each runtime's `build.py`, allowing more flexibility and reuse with different source
   repositories and target definitions repositories.


Do you need this package? And how shall you approach it?
========================================================

Use the decision tree below to find your role and the right entry point.
Answer each question top-down; click the leaf node to jump to the relevant guide.

.. mermaid::

   flowchart TD
       Q1{"You already have a target-specific runtime to use as-is or edit and rebuild?"}
       Q1 -- Yes --> RB["<b>Runtime builder</b> &nbsp;—&nbsp; Out of scope for rts_prebuilder (see GNAT UG: Customized Run-Time Libraries instead)"]
       Q1 -- No --> Q2{"You want to prebuild a runtime for a Target/board that is already supported by an existing targets repository (e.g. bb_runtimes_targets_gen)?"}
       Q2 -- Yes --> EU["<b>End user</b> &nbsp;—&nbsp; See End Users Guide. You may skip the rest of this documentation."]
       Q2 -- No --> Q3{"A repository with existing target configurations already exists, and you only need to add a new target/board?"}
       Q3 -- Yes --> EXT["<b>Concrete Infrastructure Extender</b> &nbsp;—&nbsp; See Extenders Guide. Read the rest of the overview first."]
       Q3 -- No --> BLD["<b>Concrete Infrastructure Builder</b> &nbsp;—&nbsp; specific constraints prevent reuse of existing repos. See Builders Guide. Read the rest of the overview first."]
       click RB "https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx/customized_run-time_libraries.html" _blank
       click EU "../user_guides/end_users_guide.html"
       click EXT "../user_guides/concrete_infrastructure_extenders.html"
       click BLD "../user_guides/concrete_infrastructure_builders.html"
       style RB fill:#ffeb3b,stroke:#f57f17,stroke-width:3px
       style EU fill:#c8e6c9,stroke:#388e3c,stroke-width:2px
       style EXT fill:#bbdefb,stroke:#1976d2,stroke-width:2px
       style BLD fill:#ffe0b2,stroke:#e65100,stroke-width:2px
       style Q1 fill:#fffde7,stroke:#f9a825
       style Q2 fill:#fffde7,stroke:#f9a825
       style Q3 fill:#fffde7,stroke:#f9a825


