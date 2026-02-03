--  ecs-store.adb

--  This file defines the main ECS store structure that holds entities and their associated components.
--  It uses Ada.Containers to manage dynamic collections of Entities and Components.

--  The store contains:
--    - An Entity map to hold all entities.
--    - A Table which includes:
--       - Component tables for each component type.
--       - Lookup maps to associate Entities with their Components.

--  This structure allows for efficient storage and retrieval of entities and their components.
--  "No logic" is implemented here -- only the data structure definitions.
--    - "The Store can have *a little* logic, as a treat." (Add, Remove, Has, Get)
--  Additional Component types and their corresponding tables + lookups can be added as needed.
--    - Search for "TODO" to find where!

with Ada.Tags;                      use type Ada.Tags.Tag;

-- Component Types
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;

package body ECS.Store is

   -- Hash function for Entity_ID
   -- Simple hash function that converts Entity_ID to Hash_Type
   -- Used (and needed) for Ada.Containers.Hashed_Maps
   function Hash_Entity_ID (ID : Entity_ID) return Ada.Containers.Hash_Type is
   
   begin
      return Ada.Containers.Hash_Type(ID);
   end Hash_Entity_ID;


   -- Procedure to initialize the ECS store
   --    - Clears all entities and component tables
   --    - Resets the next available entity ID
   --    - This procedure should be called before using the ECS store.
   procedure Initialize (S : in out Store) is   -- Initialize the ECS store

   begin

      Entity_Maps.Clear (S.Entities);

      Transform_Table.Vectors.Clear (S.Transform.Data);
      Transform_Table.Lookups.Clear (S.Transform.Lookup);

      Motion_Table.Vectors.Clear (S.Motion.Data);
      Motion_Table.Lookups.Clear (S.Motion.Lookup);

      S.Next_Entity_ID := 0;

   end Initialize;


-------------------------------------------------------------------------------------------------------------------------------
-- Entity Storage Subprograms
-------------------------------------------------------------------------------------------------------------------------------

-- Function to create a new entity in the ECS store
-- This function creates an entity with unique ID and no associated components.
--    - Initializes the entity
--    - Assigns a unique entity ID
--    - Inserts the entity into the entity map
--    - Returns the new entity ID
   function Create_Entity (S : in out Store) return Entity_ID is

   E        : Entity;                  -- New Entity
   New_ID   : Entity_ID;               -- New entity ID

   begin
      
      -- 'Succ' (Successor) is a built-in attribute for discrete types (like integers, characters, or enumerated types) that returns the value immediately following the given input, essentially input + 1 in sequence
      S.Next_Entity_ID := Entity_ID'Succ (S.Next_Entity_ID);      -- Increment the next available entity ID
      New_ID := S.Next_Entity_ID;                                 -- Assign the new entity ID

      E.ID := New_ID;                             -- Set the entity ID

      -- Insert the new Entity into the Entity map
      S.Entities.Insert (New_ID, E);     

      return New_ID;                -- Return the new entity ID

   end Create_Entity;


-- Procedure to destroy an entity in the ECS store
-- This procedure removes an Entity and its associated Components from the ECS store.
--    - Removes the Entity from the Entity map
--    - Removes associated Components from Component tables and lookups
   procedure Destroy_Entity (S : in out Store; ID : Entity_ID) is

   begin

      if not S.Entities.Contains (ID) then

         return;        -- Entity does not exist, do nothing

      end if;

      -- Component Removal
      if S.Transform.Lookup.Contains (ID) then
         Transform_Table.Remove (S.Transform, ID);
      end if;

      if S.Motion.Lookup.Contains (ID) then
         Motion_Table.Remove (S.Motion, ID);
      end if;

      -- TODO: Add other Component tables here!

      S.Entities.Delete (ID);    -- -- Remove the entity from the entity map
   
   end Destroy_Entity;


   -- Function to check if an Entity of given Entity_ID exists in the ECS store
   function Has_Entity (S : Store; ID : Entity_ID) return Boolean is

   begin

      return S.Entities.Contains (ID);    -- Check if the entity map contains the given entity ID (True or False)

   end Has_Entity;


   -------------------------------------------------------------------------------------------------------------------------------
   -- Component Storage Subprograms
   -------------------------------------------------------------------------------------------------------------------------------
   
   -- Adds Component of Component type Tag to Entity with Entity_ID E
   procedure Add_Component
  (   S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag) is
   
   begin

      -- Runtime Component_Type Checks

      if Tag = Transform'Tag then
         if not S.Transform.Lookup.Contains (E) then
            S.Transform.Lookup.Insert
               (E, Transform_Table.Index (S.Transform.Data.Length));

            S.Transform.Data.Append
              (Transform'
                 (Position => (0.0, 0.0),
                  Rotation => 0.0,
                  Scale    => (1.0, 1.0)));
         end if;

      elsif Tag = Motion'Tag then
         if not S.Motion.Lookup.Contains (E) then
            S.Motion.Lookup.Insert
              (E, Motion_Table.Index (S.Motion.Data.Length));

            S.Motion.Data.Append
              (Motion'
                 (others => (0.0, 0.0)));
         end if;

      else
         raise Program_Error with "Unknown component tag";
      end if;
   
   end Add_Component;



   -- Removes Component of type Tag from Entity of Entity_ID E
   -- This procedure uses Component_Table's Remove()
   procedure Remove_Component
   (S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag) is
      
   begin

      -- Steps to Remove_Component: --> These are handled in Component_Table's Remove()
            --  Find index of component to remove
            --  Swap last element into that index
            --  Update the swapped entity’s index
            --  Remove last vector element
            --  Remove entity from lookup
            --  This is O(1) and cache-friendly.

      -- Runtime Component_Type Checks

      -- Learning this:
      --    Rule of thumb:
      --    Whenever you pass a record aggregate to a container operation, qualify it with Type'(...).

      if Tag = Transform'Tag then
         if S.Transform.Lookup.Contains (E) then
            Transform_Table.Remove (S.Transform, E);
         end if;

      elsif Tag = Motion'Tag then
         if S.Motion.Lookup.Contains (E) then
            Motion_Table.Remove (S.Motion, E);
         end if;

      -- TODO: Add other Component types here!

      else
         raise Program_Error with "Unknown component tag";
      end if;
      
   end Remove_Component;


   -- Checks Entity of Entity_ID E of Component type Tag
   function Has_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag  ) return Boolean is
   
   begin

      -- Runtime Component_Type Checks

      if Tag = Transform'Tag then
         return S.Transform.Lookup.Contains (E);

      elsif Tag = Motion'Tag then
         return S.Motion.Lookup.Contains (E);

      -- TODO: Add other Component types here!

      else
         return False;
      end if;
   
   end Has_Component;


   -- Returns Component of type Tag of Entity with Entity_ID E
   function Get_Component
  (   S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag) return Component'Class is
   
   begin

      -- Runtime Component_Type Checks
      
      if Tag = Transform'Tag then
         return S.Transform.Data
         (S.Transform.Lookup (E));

      elsif Tag = Motion'Tag then
         return S.Motion.Data
         (S.Motion.Lookup (E));

      -- TODO: Add other Component types here!

      else
         raise Program_Error with "Unknown Component tag";
      end if;
   
   end Get_Component;

end ECS.Store;