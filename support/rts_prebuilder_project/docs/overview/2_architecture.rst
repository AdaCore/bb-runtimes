.. Copyright (C) 2025-2026, AdaCore

============
Architecture
============

The ``rts_prebuilder`` is a python package with two distinct namespaces:

* Engine
* AbstractInfrastructure

Overview
========

.. mermaid::

   ---
   config:
     layout: elk
   ---

   flowchart LR

      subgraph "RTS Prebuilder"
         Engine["<b>Engine</b><br/>Core functionality<br/>RuntimeAssembler<br/>RuntimeTargetizer<br/>CLI helpers"]

         subgraph AbstractInfrastructure["<b>Abstract Infrastructure</b>"]
            subgraph Extender["<b>For Infra extenders</b>"]
               EBase["<b>Base classes</b><br/>AbstractTarget <br/>AbsractArchSupport"]

               DataClasses["<b>Data classes</b><br/>SourceFile <br/> LinkerSwitch <br/> SourcesListing composer..."]
            end
            
            subgraph Interface["<b>For Infra builders</b>"]
               InterfaceObject["<b>Tightly defined interface contract</b><br/>EngineToInfrastructureInterface instance"]
               AbstractComponents["<b>Abstract components</b><br/> SourcePathResolver<br/>AbstractProfileToScenarioGenerator<br/>"]
            end
         end
      end

      subgraph ConcreteInfrastructure["Concrete Infrastructure (such as bb_runtimes_targets_gen)"]
        InitCode["<b>Init code</b><br/>Interface setup"]
        ConcreteTargetA["<b>ConcreteTargetA</b>"]
        ConcreteTargetB["<b>ConcreteTargetB</b>"]
        ConcreteArchSupport["<b>ConcreteArchSupport</b>"]
      end

      User["<b>End User Code or CLI</b>"]
      

      Engine -->|Calls| InterfaceObject
      User -->|Uses| Engine
      Engine -->|Handles| EBase
      EBase -->|Composed of| DataClasses
      InitCode -->|Sets up| InterfaceObject

      %% Inheritance-style (child -> parent)
      ConcreteInfrastructure -.->|implements| AbstractComponents
      ConcreteTargetA -.->|extends| EBase
      ConcreteTargetB -.->|extends| EBase
      ConcreteArchSupport -.->|extends| EBase

      %% Styling
      style Engine fill:#e1f5ff,stroke:#0288d1,stroke-width:3px
      style AbstractInfrastructure fill:#fff9e6,stroke:#ff9800,stroke-width:3px
      style Extender fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
      style Interface fill:#fff3e0,stroke:#f57c00,stroke-width:2px
      style EBase fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
      style AbstractComponents fill:#ffe0b2,stroke:#f57c00,stroke-width:1px
      style InterfaceObject fill:#ffe0b2,stroke:#f57c00,stroke-width:1px
      style InfraCode fill:#e1bee7,stroke:#6a1b9a,stroke-width:3px
      style ConcreteTargetA fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style ConcreteTargetB fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style InitCode fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style ConcreteArchSupport fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style ConcreteInfrastructure fill:#ce93d8,stroke:#8e24aa,stroke-width:2px
      style User fill:#e8f5e9,stroke:#388e3c,stroke-width:3px


The **Engine** provides ready-to-use tools (:class:`~rts_prebuilder.engine.RuntimeAssembler`, :class:`~rts_prebuilder.engine.RuntimeTargetizer`) while the **Abstract Infrastructure** defines the contracts that the infrastructure implementation must fulfill.


Engine
======

The engine provides the core functionality for runtime assembly (through the RuntimeAssembler) and targetization (through the RuntimeTargetizer),
as presented in the :ref:`Functional diagram <functional_diagram>`.

It has no knowledge of any "data", data in this context can mean:

    - Source code files & locations
    - Target definitions
    - Scenarios and profiles...

It ingests that knowledge only through the abstract infrastructure interfaces.

It's abstract because it must be extended by a concrete implementation to link with the actual data.

The engine can interact with the infrastructure only through the class
:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`.

.. warning::

   If no interface is "registered", the engine will raise an exception.
   See :ref:`infrastructure_registration` for more details.

Abstract Infrastructure
=======================

The abstract infrastructure provides 2 things:

1. **For Concrete Infrastructure Builders**: The main interface (:class:`~rts_prebuilder.abstract_infrastructure.EngineToInfrastructureInterface`) for the engine to interact with the infrastructure. A concrete implementation must instantiate this class and fill in the required fields.
   See :doc:`../user_guides/concrete_infrastructure_builders` for more details.

2. **For Concrete Infrastructure Extenders**: The base classes for target definitions to extend. Notably the :class:`~rts_prebuilder.abstract_infrastructure.AbstractTarget` base class to define targets, and the :class:`~rts_prebuilder.abstract_infrastructure.AbsractArchSupport` base class to define architecture support.
   See :doc:`../user_guides/concrete_infrastructure_extenders` for more details.

The diagram above shows how these components work together within the Infrastructure namespace.

