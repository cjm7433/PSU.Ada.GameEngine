-- Central ECS storage structure - holds all entities and components.
-- * Additional component types and their corresponding tables and lookups can be added as needed.

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components;                use ECS.Components;
with ECS.Component_Table;

-- Component Types
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with ECS.Components_Collision;      use ECS.Components_Collision;
with ECS.Components_Paddle;         use ECS.Components_Paddle;
with ECS.Components_Ball;           use ECS.Components_Ball;
with ECS.Components_Brick;          use ECS.Components_Brick;
with ECS.Components_Renderable;     use ECS.Components_Renderable;
-- TODO: Add component types here


package ECS.Store is
   -- Converts Entity_ID to Hash_Type
   function Hash_Entity_ID (ID : Entity_ID) return Ada.Containers.Hash_Type;


   use type Entity_ID;              -- Make Entity_ID directly visible (for Key_Type)
   package Entity_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => Entity_ID,          -- Entity ID as key
      Element_Type    => Entity,             -- Entity record as value
      Hash            => Hash_Entity_ID,     -- Hash function
      Equivalent_Keys => "="                 -- Key equivalence function
   );


   -- Dense storage for Components
   package Transform_Table is new Component_Table
      (Component_Type => Transform_Component,
      Hash           => Hash_Entity_ID);

   package Motion_Table is new Component_Table
     (Component_Type => Motion,
      Hash           => Hash_Entity_ID);

   package Collision_Table is new Component_Table
      (Component_Type => Collision,
      Hash           => Hash_Entity_ID);

   package Paddle_Table is new Component_Table
      (Component_Type   => Paddle,
      Hash           => Hash_Entity_ID);

   package Ball_Table is new Component_Table
      (Component_Type   => Ball,
      Hash           => Hash_Entity_ID);

   package Brick_Table is new Component_Table
      (Component_Type   => Brick,
      Hash           => Hash_Entity_ID);

   package Renderable_Table is new Component_Table
      (Component_Type   => Renderable,
      Hash           => Hash_Entity_ID);

   -- Tables for each Component type need to be added here
   -- TODO: Add other Component Types here!


   -- Entities
   type Entity_ID_Array is array (Natural range <>) of Entity_ID;
   type Entity_ID_Array_Access is access Entity_ID_Array;


   -- The main ECS store structure
   -- Holds Entity map and Component tables with Lookups for each Component Type
   type Store is record
      -- Entities
      Entities       : Entity_Maps.Map;      -- Map of all entities
      Next_Entity_ID : Entity_ID := 0;       -- Next available entity ID

      -- Component tables (Vectors and Lookups)
      Transform   : Transform_Table.Table;
      Motion      : Motion_Table.Table;
      Collision   : Collision_Table.Table;
      Paddle      : Paddle_Table.Table;
      Ball        : Ball_Table.Table;
      Brick       : Brick_Table.Table;
      Renderable  : Renderable_Table.Table;

      -- TODO: Add Component tables for each Component type!
   end record;


   -- Initializes the ECS store
   -- NOTE: This procedure should be called before using the ECS store
   procedure Initialize (S : in out Store);


   -- Creates a new entity in the ECS store and returns its Entity_ID
   function Create_Entity (S : in out Store) return Entity_ID;


   -- Removes an Entity from the ECS Store
   procedure Destroy_Entity (S : in out Store; ID : Entity_ID);


   -- Returns whether or not the ECS Store contains an Entity of the given Entity_ID
   function Has_Entity (S : Store; ID : Entity_ID) return Boolean;


   -- Creates a new component of the passed type and attaches it to the given Entity_ID
   procedure Add_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


   -- Removes a component of the passed type from the given Entity_ID
   procedure Remove_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


   -- Returns whether or not the given Entity_ID contains an instance of the passed component type
   function Has_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Boolean;


   -- Returns an instance of the passed component type on the given Entity_ID
   function Get_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Component'Class;


   --------------------------------------------------------------------------------
   -- Get_Entities_With
   -- Gets an array of Entity IDs that have all the specified component tags
   --------------------------------------------------------------------------------
   function Get_Entities_With
  (   S    : Store;
      Tags : Component_Tag_Array) return Entity_ID_Array_Access;


   -------------------------------------------------------------------------------
   -- Get_Entity_IDs
   -- Return a dynamically allocated array containing all entity IDs
   --    that own a specific component type.
   -- This function iterates through all entities in the store and
   --    checks if they have the specified component tag.
   -------------------------------------------------------------------------------
   function Get_Entity_IDs
   (  S   : Store;
      Tag : Component_Tag) return Entity_ID_Array_Access;


end ECS.Store;