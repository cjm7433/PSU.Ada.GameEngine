--  ecs-store.adb
--  This file is part of the PSU Ada Game Engine (PAGE).
--  This file defines the main ECS store structure that holds entities and their associated components.
--  This is the public API for the ECS data storage.
--  It uses Ada.Containers to manage dynamic collections of entities and components.

--  The store contains:
--    - An entity map to hold all entities.
--    - Component tables for each component type.
--    - Lookup maps to associate entities with their components.

--  This structure allows for efficient storage and retrieval of entities and their components.
--  No logic is implemented here -- only the data structure definitions.
--  Additional component types and their corresponding tables and lookups can be added as needed.

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with ECS.Types;
with ECS.Entities;
with ECS.Components;
with Ada.Containers.Hashed_Maps;

package body ECS.Store is

   -- Hash function for Entity_ID
   -- Simple hash function that converts Entity_ID to Hash_Type
   -- Used (and needed) for Ada.Containers.Hashed_Maps
   function Hash_Entity_ID (ID : ECS.Types.Entity_ID) return Ada.Containers.Hash_Type is
   
   begin
   
      return Ada.Containers.Hash_Type(ID);
   
   end Hash_Entity_ID;


   -- Procedure to initialize the ECS store
   --    - Clears all entities and component tables
   --    - Resets the next available entity ID
   --    - This procedure should be called before using the ECS store.
   procedure Initialize (S : in out Store) is   -- Initialize the ECS store

   begin
      Entity_Maps.Clear (S.Entities);              -- Clear the entity map

      Transform_Vectors.Clear (S.Transform_Table);  -- Clear the component tables
      Velocity_Vectors.Clear (S.Velocity_Table);
      -- etc. for other component tables
      -- e.g., Collider_Vectors.Clear (S.Collider_Table);

      Lookup_Maps.Clear (S.Transform_Lookup);   -- Clear the lookup maps
      Lookup_Maps.Clear (S.Velocity_Lookup);
      -- etc. for other lookup maps
      -- e.g., Lookup_Maps.Clear (S.Collider_Lookup);

   end Initialize;


-- Function to create a new entity in the ECS store
-- This function should be called to add new entities to the ECS store.
-- This function creates an entity with unique ID and no associated components.
--    - Assigns a unique entity ID
--    - Initializes the entity record
--    - Inserts the entity into the entity map
--    - Returns the new entity ID
   function Create_Entity (S : in out Store) return ECS.Types.Entity_ID is
   
   New_ID : ECS.Types.Entity_ID;             -- New entity ID
   ER     : ECS.Entities.Entity_Record;      -- New entity record

   begin
   -- 'Succ' (Successor) is a built-in attribute for discrete types (like integers, characters, or enumerated types) that returns the value immediately following the given input, essentially input + 1 in sequence
      S.Next_Entity_ID := ECS.Types.Entity_ID'Succ (S.Next_Entity_ID);  -- Increment the next available entity ID
      New_ID := S.Next_Entity_ID;                                       -- Assign the new entity ID

      ER.ID := New_ID;                             -- Set the entity ID
      ER.Tags := (others => False);                -- Initialize component presence set

      S.Entities.Insert (Key => New_ID, New_Item => ER);    -- Insert the new entity into the entity map

      return New_ID;                -- Return the new entity ID

   end Create_Entity;


-- Procedure to destroy an entity in the ECS store
-- This procedure removes an entity and its associated components from the ECS store.
--    - Removes the entity from the entity map
--    - Removes associated components from component tables and lookups
--    - Note: Component removal logic will be added later.
procedure Destroy_Entity (S : in out Store; ID : ECS.Types.Entity_ID) is
   
   begin

      if not S.Entities.Contains (ID) then
         
         return;        -- Entity does not exist, do nothing
      
      end if;

      -- NOTE:
      -- Component removal will be added later.

      S.Entities.Delete (ID);  -- Remove the entity from the entity map

   end Destroy_Entity;



-- Function to check if an entity exists in the ECS store
function Has_Entity (S : Store; ID : ECS.Types.Entity_ID) return Boolean is

   begin

      return S.Entities.Contains (ID); -- Check if the entity map contains the given entity ID (True or False)

   end Has_Entity;


-------------------------------------------------------------------------------------------------------------------------------

   -- Transform component is used here -- similar procedures/functions can be created for other component types.

   -- This procedure adds a Transform component to the specified entity ID
   -- It updates the component table and lookup map accordingly.
   -- Systems will use this to add components to entities as needed.
   --    - Prevents adding if the component already exists for the entity.
   --    - Initializes the component with provided values.
   --    - Marks the entity as having the Transform component in its component presence set.

   -- Note: Similar procedures can be created for other component types.
   procedure Add_Transform_Component (
      
      S  : in out Store;         -- ECS store that holds entities and components 
      ID : ECS.Types.Entity_ID;  -- Entity ID of the entity to add the component to

      X  : Float;    -- Position coordinates
      Y  : Float;

      R  : Float;    -- Rotation in degrees
      k  : Float     -- Uniform scale factor; Does this lowercase violate Ada convention


   ) is

      E: ECS.Components.Transform_Component;  -- New Transform component

      begin

         -- Validate that the entity exists
         if not S.Entities.Contains (ID) then
            
            return;        -- Entity does not exist, do nothing
         
         end if;

         -- Prevent adding if already exists
         if S.Transform_Lookup.Contains (ID) then

            return;  -- Component already exists for this entity, do nothing
         
         end if;

         -- Create and populate the Transform component
         E.Owner := ID; -- Set owner to entity ID
         E.X := X;      -- Set position
         E.Y := Y;
         E.Rotation := R;  -- Set rotation
         E.Scale := k;     -- Set scale
         -- Add the component to the Transform table
         S.Transform_Table.Append (E);

         -- Get the index of the newly added component
         S.Transform_Lookup.Insert (
            Key      => ID,                           -- Entity ID
            New_Item => S.Transform_Table.Last_Index  -- Index of the newly added component
         );

         -- Mark that the entity has a Transform component
         S.Entities (ID).Tags (ECS.Types.Transform) := True;

      --           To reduce verbosity without use, you may do:
      --  C : constant ECS.Types.Component_Type := ECS.Types.Transform;
      --  S.Entities (ID).Tags (C) := True;
      --  Useful when working with multiple component types in a loop.

      end Add_Transform_Component;


   -- Check if an entity has a Transform component
   -- This is what systems will use to check for component presence (filter)
   function Has_Transform_Component (
      S  : Store;                -- ECS store
      ID : ECS.Types.Entity_ID   -- Entity ID
   ) return Boolean is

      begin

         return S.Transform_Lookup.Contains (ID);  -- Check if the Transform lookup contains the given entity ID (True or False)

      end Has_Transform_Component;


   -- Get a Transform component for a given entity ID
   -- Returns an access to the Transform component, or null if not found
   -- Systems will use this to retrieve component data for processing
   --    - Systems can mutate component data
   --    - Systems cannot modify storage structure

   -- Note: Caller must handle null access case
   -- Note: Similar procedures can be created for other component types.
   procedure Get_Transform_Component (
      S  : in out Store;         -- ECS store
      ID : ECS.Types.Entity_ID;   -- Entity ID
      Ref: out ECS.Components.Transform_Component  -- Component reference so we can access it (Ada Rule: No raw pointers to container elements)
   )is

      Idx : ECS.Types.Index;  -- Index into the Transform component table

   begin

      if not S.Transform_Lookup.Contains (ID) then  -- If Component does not exist
         
          raise Constraint_Error with "Entity has no Transform component";
         --return null; -- Return null access --> caller must handle    -- Cannot return anything with procedure
      
      end if;

      Idx := S.Transform_Lookup (ID);     -- Get the index of the Transform component for the given entity ID
      Ref := S.Transform_Table (Idx);     -- Set the output reference to the Transform component

      -- Can't do this in Ada:
      --return S.Transform_Table (Idx)'Access;    -- Return access to the Transform component

   end Get_Transform_Component;


   -- Remove a Transform component for a given entity ID
   -- This procedure removes the Transform component associated with the given entity ID
   -- It maintains the integrity of the component table and lookup map (retains dense storage) by using a "swap and pop" method.
   -- Systems will use this to remove components when needed (when an entity is destroyed or a component is no longer needed)

   -- The removal process involves:
   --    - Finding the index of the component to remove
   --    - Swapping it with the last component in the table (if not the last)
   --    - Updating the lookup map for the moved component
   --    - Deleting the last component from the table
   --    - Removing the entry from the lookup map
   --    - Updating the entity's component presence set

   -- Note: Similar procedures can be created for other component types.
   procedure Remove_Transform_Component (
      S  : in out Store;         -- ECS store
      ID : ECS.Types.Entity_ID   -- Entity ID   
   ) is

      Remove_Index : ECS.Types.Index;        -- Index to remove
      Last_Index   : ECS.Types.Index;        -- Last index in the component table
      Last_EID     : ECS.Types.Entity_ID;    -- Entity ID of the last component

   begin

      if not S.Transform_Lookup.Contains (ID) then     -- If Component does not exist

         return;      -- Do nothing
         -- Possibly raise Constraint_Error with "Entity has no Transform component";

      end if;

      Remove_Index := S.Transform_Lookup (ID);         -- Get index of component to remove
      Last_Index   := S.Transform_Table.Last_Index;    -- Get index of last element in table

      if Remove_Index /= Last_Index then              -- If not removing last element, we need to swap

         S.Transform_Table (Remove_Index) := S.Transform_Table (Last_Index);  -- Move last to removed slot

         Last_EID := S.Transform_Table (Last_Index).Owner;  -- Get the entity ID of the owner of the moved component

         S.Transform_Lookup.Replace (     -- Update lookup for moved entity
            Key      => Last_EID,         -- Entity ID of moved component
            New_Item => Remove_Index      -- New index after move
         );
      
      end if;

      S.Transform_Table.Delete_Last;                            -- Remove last element from component table
      S.Transform_Lookup.Delete (ID);                           -- Remove from lookup map
      S.Entities (ID).Tags (ECS.Types.Transform) := False;      -- Update

   end Remove_Transform_Component;

end ECS.Store;
