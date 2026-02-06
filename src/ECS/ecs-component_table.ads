-- ecs-component_table

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines a generic template for Component tables used in ECS-Store for dense Component storage.
-- The resultant Table includes Component vector and Lookup

with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with ECS.Entities; use ECS.Entities;

-- Generic component table: A vector of components per component type
   -- (So each component type table doesnt need its own package)
   -- Component Table is meant to be a vector AND a lookup

generic
   type Component_Type is private;        -- Type variable compiler will replace
   
   with function Hash (E : Entity_ID)
     return Ada.Containers.Hash_Type;

-- Component Table is meant to be a vector AND a lookup
package ECS.Component_Table is

   subtype Index is Natural;

   -- A vector of Components (of the same Component Type)
   -- Contiguous in memory
   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Index,
      Element_Type => Component_Type);

   -- Maps Entity_ID --> Index
   -- Not contiguous in memory.
   package Lookups is new Ada.Containers.Hashed_Maps
     (Key_Type        => Entity_ID,
      Element_Type    => Index,
      Hash            => Hash,
      Equivalent_Keys => "=");

   -- This is the table: Vector of Components and Entity_ID --> Index map
   type Table is record
      Data   : Vectors.Vector;
      Lookup : Lookups.Map;
   end record;

   -- Generic Remove Component Utility (for Remove_Component())
   procedure Remove
     (T : in out Table;
      E : Entity_ID);

end ECS.Component_Table;