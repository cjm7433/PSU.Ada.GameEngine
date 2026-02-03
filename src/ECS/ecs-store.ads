-- ecs-store.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the main ECS store structure that holds entities and their associated components.
-- It uses Ada.Containers to manage dynamic collections of Entities and Components.

--  The store contains:
--    - An Entity map to hold all entities.
--    - A Table which includes:
--       - Component tables for each component type.
--       - Lookup maps to associate Entities with their Components.

-- This structure allows for efficient storage and retrieval of entities and their components.
-- No logic is implemented here -- only the data structure definitions.

-- Additional component types and their corresponding tables and lookups can be added as needed.

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components;                use ECS.Components;
with ECS.Component_Table;

-- Component Types
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;

package ECS.Store is

   -- Hash function for Entity_ID (has to be defined for Ada.Containers.Hashed_Maps)
   -- Needs to be before Entity_Maps for instantiation visibility.
   function Hash_Entity_ID (ID : Entity_ID) return Ada.Containers.Hash_Type;

   use type Entity_ID;             -- Make Entity_ID directly visible (for Key_Type)

   -- Entity Map
   package Entity_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => Entity_ID,          -- Entity ID as key
      Element_Type    => Entity,             -- Entity record as value
      Hash            => Hash_Entity_ID,     -- Hash function
      Equivalent_Keys => "="                 -- Key equivalence function
   );

   -- Component Tables (Instantiate):
   -- Dense storage for Components by Component type. (Vectors)
   package Transform_Table is new Component_Table
     (Component_Type => Transform,
      Hash           => Hash_Entity_ID);

   package Motion_Table is new Component_Table
     (Component_Type => Motion,
      Hash           => Hash_Entity_ID);

   -- Tables for each Component type need to be added here
   -- TODO: Add other Component Types here!

   -- package [Component_Type]_Table is new Component_Table
   -- (  Component_Type => [Component_Type],
   --    Hash           => Hash_Entity_ID);

   ----------------------------------------------------------------------------------------------------------------------------
   -- The ECS Store
   ----------------------------------------------------------------------------------------------------------------------------

   -- The main ECS store structure
   -- Holds Entity map and Component tables with Lookups for each Component Type
   -- Tables for each Component type need to be instantiated here
   type Store is record 

      -- Entities
      Entities       : Entity_Maps.Map;      -- Map of all entities
      Next_Entity_ID : Entity_ID := 0;       -- Next available entity ID


      -- Component tables (Vectors and Lookups)
      Transform   : Transform_Table.Table;
      Motion      : Motion_Table.Table;

      -- TODO: Add Component tables for each Component type!

   end record;

   ----------------------------------------------------------------------------------------------------------------------------
   -- Store Life Cycle
   ----------------------------------------------------------------------------------------------------------------------------

   -- INITIALIZATION PROCEDURE: Initializes the ECS store
   -- Basically just clears a bunch of stuff
   procedure Initialize (S : in out Store);

   ----------------------------------------------------------------------------------------------------------------------------
   -- ENTITY OPERATIONS: For manipulating entities in the entity map
   ----------------------------------------------------------------------------------------------------------------------------

   -- Creates a new Entity and returns its ID
   function Create_Entity (S : in out Store) return Entity_ID;       


   -- Destroys an Entity by ID
   procedure Destroy_Entity (S : in out Store; ID : Entity_ID);      


   -- Checks if an entity exists by its ID
   function Has_Entity (S : Store; ID : Entity_ID) return Boolean;   

   ----------------------------------------------------------------------------------------------------------------------------
   -- COMPONENT OPERATIONS: For manipulating Components in the component table and lookup
   ----------------------------------------------------------------------------------------------------------------------------

   -- Adds a Component of type Tag to an Entity
   procedure Add_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


   -- Removes a Component of type Tag from an Entity
   procedure Remove_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


-- Checks if an entity has a Component of type Tag
   function Has_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Boolean;


-- Gets a Component for type Tag for an Entity
   function Get_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Component'Class;
   
end ECS.Store;