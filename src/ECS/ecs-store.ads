-- ecs-store.ads

-- Central ECS storage structure - holds all entities and components.
--    - The Store is the "database" of the ECS. 
--    - Everything the game knows about entities lives here:
--       * Entity map (ID → Entity record)
--       * Component tables (one per component type). Includes:
--          - Component tables for each component type.
--          - Lookup maps to associate Entities with their Components.
--       * Query functions (find entities with specific components)
--    - This structure allows for efficient storage and retrieval of entities and their components.
--    - No logic is implemented here -- only the data structure definitions.
--
-- * Additional component types and their corresponding tables and lookups can be added as needed.

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components;                use ECS.Components;
with ECS.Component_Table;

-- Component Types
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with ECS.Components_Collision;      use ECS.Components_Collision;
with ECS.Components_Paddle;         use ECS.Components_Paddle;
with ECS.Components_Ball;           use ECS.Components_Ball;
with ECS.Components_Brick;          use ECS.Components_Brick;
with ECS.Components_Renderable;     use ECS.Components_Renderable;
-- TODO: Add component types here

package ECS.Store is

   -------------------------------------------------------------------------------
   -- Hash_Entity_ID
   -- Hash function for Entity_ID (has to be defined for Ada.Containers.Hashed_Maps)
   -- Needs to be before Entity_Maps for instantiation visibility.
   -------------------------------------------------------------------------------
   function Hash_Entity_ID (ID : Entity_ID) return Ada.Containers.Hash_Type;

   -------------------------------------------------------------------------------
   -- Entity Map
   -------------------------------------------------------------------------------

   use type Entity_ID;              -- Make Entity_ID directly visible (for Key_Type)
   
   package Entity_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => Entity_ID,          -- Entity ID as key
      Element_Type    => Entity,             -- Entity record as value
      Hash            => Hash_Entity_ID,     -- Hash function
      Equivalent_Keys => "="                 -- Key equivalence function
   );

   -------------------------------------------------------------------------------
   -- Component Tables (Instantiate):
   -- Dense storage for Components by Component type. (Vectors)
   -- TODO: Component Types need to be manually added here!
   -------------------------------------------------------------------------------
   package Transform_Table is new Component_Table
      (Component_Type => Transform,
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

   -- package [Component_Type]_Table is new Component_Table
   -- (  Component_Type => [Component_Type],
   --    Hash           => Hash_Entity_ID);

   --================================================================
   -- Entity ID array support
   --================================================================
   
   -- Array of Entity_IDs for Systems to query
   type Entity_ID_Array is array (Natural range <>) of Entity_ID;

   -- Access type for Entity_ID_Array (for returning from Get_Entity_IDs)
   type Entity_ID_Array_Access is access Entity_ID_Array;


   --~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
   -- The ECS Store
   -- The main ECS store structure
   -- Holds Entity map and Component tables with Lookups for each Component Type
   -- TODO: Tables for each Component type need to be instantiated here
   --~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
   
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

      -- *****************************************************
      -- TODO: Add Component tables for each Component type!
      -- *****************************************************

   end record;


   --================================================================
   -- Store Life Cycle
   --================================================================

   -------------------------------------------------------------------------------
   -- Initialize
   -- Initializes the ECS store
   -- TODO: Component Types need to be manually added here!
   -------------------------------------------------------------------------------
   procedure Initialize (S : in out Store);


   --================================================================
   -- ENTITY OPERATIONS: For manipulating entities in the entity map
   --================================================================

   -------------------------------------------------------------------------------
   -- Create_Entity
   -- Creates a new Entity and returns its ID
   -------------------------------------------------------------------------------
   function Create_Entity (S : in out Store) return Entity_ID;       


   -------------------------------------------------------------------------------
   -- Destroy_Entity
   -- Destroys an Entity by ID
   -- TODO: Component Types need to be manually added here!
   -------------------------------------------------------------------------------
   procedure Destroy_Entity (S : in out Store; ID : Entity_ID);      


   -- Checks if an entity exists by its ID
   function Has_Entity (S : Store; ID : Entity_ID) return Boolean;   


   --================================================================
   -- Component Storage Subprograms
   --================================================================

   --------------------------------------------------------------------------------
   -- Add_Component
   -- Adds Component of Component type Tag to Entity with Entity_ID E
   -- TODO: Component Types need to be manually added here!
   --------------------------------------------------------------------------------
   procedure Add_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


   -------------------------------------------------------------------------------
   -- Remove_Component
   -- Removes Component of type Tag from Entity of Entity_ID E
   -- This procedure uses Component_Table's Remove()
   -- TODO: Component Types need to be manually added here!
   -------------------------------------------------------------------------------
   procedure Remove_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag  );


   --------------------------------------------------------------------------------
   -- Has_Component
   -- Checks Entity of Entity_ID E of Component type Tag
   -- TODO: Component Types need to be manually added here!
   --------------------------------------------------------------------------------
   function Has_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Boolean;


   -------------------------------------------------------------------------------
   -- Get_Component
   -- Returns Component of type Tag of Entity with Entity_ID E
   -- TODO: Component Types need to be manually added here!
   --------------------------------------------------------------------------------
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