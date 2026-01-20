-- ecs-store.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the main ECS store structure that holds entities and their associated components.
-- This is the public API for the ECS data storage.
-- It uses Ada.Containers to manage dynamic collections of entities and components.

-- The store contains:
--   - An entity map to hold all entities.
--   - Component tables for each component type.
--   - Lookup maps to associate entities with their components.

-- This structure allows for efficient storage and retrieval of entities and their components.
-- No logic is implemented here -- only the data structure definitions.

-- Additional component types and their corresponding tables and lookups can be added as needed.

with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with ECS.Types;
with ECS.Entities;
with ECS.Components;    --use type ECS.Components.Transform_Component;

package ECS.Store is

   -- Hash function for Entity_ID (has to be defined for Ada.Containers.Hashed_Maps)
   -- Needs to be before Entity_Maps for instantiation visibility.
   function Hash_Entity_ID (ID : ECS.Types.Entity_ID) return Ada.Containers.Hash_Type;


   -- Entity Map
   use type ECS.Types.Entity_ID;             -- Make Entity_ID directly visible (for Key_Type)
   use type ECS.Entities.Entity_Record;      -- Make Entity_Record directly visible (for Element_Type)

   package Entity_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => ECS.Types.Entity_ID,               -- Entity ID as key
      Element_Type    => ECS.Entities.Entity_Record,        -- Entity record as value
      Hash            => Hash_Entity_ID,                    -- Hash function
      Equivalent_Keys => "="                                -- Key equivalence function
   );


   -- Component Tables

   -- Transform Component Table
   use type ECS.Components.Transform_Component;       -- Make Transform_Component directly visible

   package Transform_Vectors is new Ada.Containers.Vectors (
      Index_Type   => ECS.Types.Index,                      -- Index type
      Element_Type => ECS.Components.Transform_Component    -- Transform component type
   );


   -- Velocity Component Table
   use type ECS.Components.Velocity_Component;        -- Make Velocity_Component directly visible

   package Velocity_Vectors is new Ada.Containers.Vectors (
      Index_Type   => ECS.Types.Index,                      -- Index type
      Element_Type => ECS.Components.Velocity_Component     -- Velocity component type
   );

   -- xxxx_Vectors can be created for additional component types as needed.


   -- Lookup Maps
   use type ECS.Types.Entity_ID;       -- Make Entity_ID directly visible

   package Lookup_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => ECS.Types.Entity_ID,               -- Entity ID as key
      Element_Type    => ECS.Types.Index,                   -- Index into component table as value
      Hash            => Hash_Entity_ID,                    -- Hash function
      Equivalent_Keys => "="                                -- Key equivalence function
   );



   type Store is record -- The main ECS store structure

      -- Entities
      Entities : Entity_Maps.Map;                  -- Map of all entities
      Next_Entity_ID : ECS.Types.Entity_ID := 0;   -- Next available entity ID


      -- Component tables
      Transform_Table : Transform_Vectors.Vector;   -- Transform components
      Velocity_Table : Velocity_Vectors.Vector;    -- Velocity components


      -- Lookups
      Transform_Lookup : Lookup_Maps.Map;  -- Map from Entity ID to Transform component index
      Velocity_Lookup : Lookup_Maps.Map;   -- Map from Entity ID to Velocity component index
   
   end record;

   ----------------------------------------------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------------------------------------------

   -- INITIALIZATION PROCEDURE: For initializing the ECS store
   procedure Initialize (S : in out Store);  -- Initializes the ECS store

   -- ENTITY OPERATIONS: For manipulating entities in the entity map
   function Create_Entity (S : in out Store) return ECS.Types.Entity_ID;      -- Creates a new entity and returns its ID
   
   procedure Destroy_Entity (S : in out Store; ID : ECS.Types.Entity_ID);     -- Destroys an entity by ID
   
   function Has_Entity (S : Store; ID : ECS.Types.Entity_ID) return Boolean;  -- Checks if an entity exists by its ID
   

   -- TRANSFORM COMPONENT OPERATIONS: For manipulating Transform components in the component table and lookup
   procedure Add_Transform_Component (
      S  : in out Store;
      ID : ECS.Types.Entity_ID;
      X  : Float;
      Y  : Float;
      R  : Float;
      k  : Float
   );  -- Adds a Transform component to an entity

   function Has_Transform_Component (
      S  : Store;
      ID : ECS.Types.Entity_ID
   ) return Boolean;  -- Checks if an entity has a Transform component

   procedure Get_Transform_Component (
      S  : in out Store;
      ID : ECS.Types.Entity_ID;
      Ref: out ECS.Components.Transform_Component  -- Component reference so we can access it (Ada Rule: No raw pointers to container elements)
   ); -- Gets a Transform component for an entity

   procedure Remove_Transform_Component (
      S  : in out Store;
      ID : ECS.Types.Entity_ID
   );  -- Removes a Transform component from an entity

end ECS.Store;