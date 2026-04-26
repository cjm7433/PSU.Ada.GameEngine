-- ecs-component_table.ads
--
-- Generic component storage template using the "table" pattern.
--    - Provides dense, cache-friendly storage for components of a single type.
--    - Each component type gets its own instantiation of this generic.
--
-- Structure:
--    Table contains two parts:
--       1. Data (Vector) - packed array of component values
--       2. Lookup (Map) - Entity_ID → Index mapping
--
-- Operations:
--    Add            : append component, insert lookup entry
--    Remove         : swap-remove from vector, update moved element's lookup
--    Has            : check if entity has this component type
--    Get            : retrieve component by entity ID
--    Lookup_Index   : get vector index for iteration


with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with ECS.Entities;                  use ECS.Entities;

generic
   type Component_Type is private;        -- Type variable compiler will replace
   
   with function Hash (E : Entity_ID)
     return Ada.Containers.Hash_Type;

-----------------------------------------------------------------
-- Component_Table
-- Generic component table: A vector of components per component type
--    (So each component type table doesnt need its own package)
-- Component Table is meant to be a vector AND a lookup
-----------------------------------------------------------------
package ECS.Component_Table is

   subtype Index is Natural;

   -- A vector of Components (of the same Component Type)
   -- Contiguous in memory
   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Index,
      Element_Type => Component_Type);


   -----------------------------------------------------------------
   -- Lookup_Map
   -- Maps Entity_ID --> Index
   -- Not contiguous in memory
   -----------------------------------------------------------------
   package Lookup_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Entity_ID,
      Element_Type    => Index,
      Hash            => Hash,
      Equivalent_Keys => "=");


   -----------------------------------------------------------------
   -- Table
   -- This is the table: Vector of Components and Entity_ID --> Index map
   -----------------------------------------------------------------
   type Table is record
      Data   : Vectors.Vector;
      Lookup : Lookup_Map.Map;
   end record;


   -----------------------------------------------------------------
   -- Add
   -- Add a component to the table for an entity
   -----------------------------------------------------------------
   procedure Add
     (T : in out Table;
      E : Entity_ID;
      C : Component_Type);


   -----------------------------------------------------------------
   -- Remove (swap remove)
   -- Remove a component from the table for an entity
   -----------------------------------------------------------------
   procedure Remove
     (T : in out Table;
      E : Entity_ID);


   -----------------------------------------------------------------
   -- Has
   -- Check if an entity has a component in the table
   -----------------------------------------------------------------
   function Has
     (T : Table;
      E : Entity_ID) return Boolean;


   -----------------------------------------------------------------
   -- Get
   -- Get the component for an entity
   -----------------------------------------------------------------
   function Get
     (T : Table;
      E : Entity_ID) return Component_Type;


   -----------------------------------------------------------------
   -- Lookup_Index
   -- Get the index of the component for an entity (for iteration)
   -----------------------------------------------------------------
   function Lookup_Index
     (T : Table;
      E : Entity_ID) return Natural;


end ECS.Component_Table;