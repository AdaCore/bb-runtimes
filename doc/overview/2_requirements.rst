.. Copyright (C) 2025-2026, AdaCore

===========================
rts_prebuilder requirements
===========================

These requirements apply only to ``rts_prebuilder``, the runtime generation
engine. They do not define requirements for the whole bb-runtimes repository
or for ``bb_runtimes_targets_gen``.

The one job of ``rts_prebuilder`` is :doc:`prebuilding <1_functional>`: turn a
target description into buildable runtime source trees, one per profile the
target offers.

Five forces shape everything in the design. The
:doc:`architecture <3_architecture>` page introduces each class as the answer to
one of them, and the guides refer back to these numbers.

.. _requirements_table:

.. list-table::
   :header-rows: 1
   :widths: 5 22 73

   * - #
     - Requirement
     - What it means
   * - R1
     - Source trees, not binaries
     - The output is one source tree per (target, profile): the selected Ada
       sources plus the generated build files (``runtime.xml``, ``.gpr``
       projects, path files). Compiling that tree is out of scope. It is the job
       of ``build.py``, the build script the infrastructure installed alongside
       it.
   * - R2
     - An engine that knows nothing concrete
     - The core engine must be reusable with any source repository and any
       target-definition repository, so it holds no built-in knowledge of file
       layouts, of the target list, or of the component catalog. The on-disk
       location of a source file is known only at run time, and every resolution
       is recorded so the installed tree is auditable.
   * - R3
     - A target description is data, not logic
     - A target description is data, not logic. The mechanisms that consume it
       live in the core engine or in a generator, never on the target itself.
       That is what lets a target be described once, independently of any
       profile, and one target instance serve every profile it offers. The profile is applied on top of the description rather than
       baked into it.
   * - R4
     - Composable, self-varying source groups
     - Runtime sources come in reusable groups shared across families of
       targets, and a given target can substitute any one of them. Each group
       decides its own sources and flags from the ``(target, profile)`` when it
       is queried, rather than freezing them at construction. Which groups make
       up a runtime varies per target and per infrastructure.
   * - R5
     - Every customization goes through the API
     - Whatever a target needs, the API can say it. You pass a value, override a
       property, or add or swap a component. You never modify files that
       ``rts_prebuilder`` generated. The ``runtime.xml`` and ``.gpr`` generators
       take input and produce files, and nothing edits those files afterwards.
       So if you find yourself
       wanting to search and replace in generated text, the API is missing an
       input, and adding that input is the fix.
