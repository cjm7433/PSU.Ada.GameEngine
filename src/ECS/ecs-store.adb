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
      Transform_Table.Lookup_Map.Clear (S.Transform.Lookup);

      Motion_Table.Vectors.Clear (S.Motion.Data);
      Motion_Table.Lookup_Map.Clear (S.Motion.Lookup);

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

      -- The following each add a Component to Component Vector and Lookup like:
      -- If the Tag matches the Component type:
      --    If the Lookup does not contain the Entity_ID:
      --       Add it to the end
      --       Add the Component data to the vector of Components

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

      -- TODO: Add Component Types here

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

      -- A fun fact I am learning (aka trying to learn) about Ada:
      --    Rule of thumb:
      --    Whenever you pass a record aggregate to a container operation, qualify it with Type'(...).

      -- The following each remove a Component from its Lookup and Component Vector like:
      -- If the Tag matches the Component type:
      --    If the Entity_ID is present in the Lookup
      --       Remove the Component from Component vector and Lookup (using Component_Table Remove())

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

      -- The following each return a Boolean as if it were:
      -- Boolean := Does this Entity_ID exist within the Lookup?

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
      
      -- The following each return a Component as if it were:
      -- Component := Component_Vector(Index of Entity in Lookup)

      if Tag = Transform'Tag then
         return S.Transform.Data(S.Transform.Lookup (E));

      elsif Tag = Motion'Tag then
         return S.Motion.Data(S.Motion.Lookup (E));

      -- TODO: Add other Component types here!

      else
         raise Program_Error with "Unknown Component tag";
      end if;
   
   end Get_Component;


   -- Gets an array of Entity IDs that have all the specified component tags
   function Get_Entities_With
  (   S    : Store;
      Tags : Component_Tag_Array) return Entity_ID_Array_Access
   is
      Base_List : Entity_ID_Array_Access;
   begin

      -- If list is empty, do nothing
      if Tags'Length = 0 then
         return null;
      end if;

      -- Use first tag as base
      Base_List := Get_Entity_IDs (S, Tags (Tags'First));

      -- If list is empty, do nothing
      if Base_List = null then
         return null;
      end if;

      declare
         Temp : Entity_ID_Array(0 .. Base_List'Length - 1);
         Count : Natural := 0;
      
      begin

         -- For each entity in the base list, check if it has all the other tags
         for I in Base_List'Range loop

            declare
               E : constant Entity_ID := Base_List (I);
               Match : Boolean := True;
            
            begin

               -- Check remaining tags
               for J in Tags'First + 1
                        .. Tags'Last
               loop
                  if not Has_Component
                  (S, E, Tags (J))
                  then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  Temp (Count) := E;
                  Count := Count + 1;
               end if;

            end;

         end loop;

         -- If there are no matches, return null, do nothing
         if Count = 0 then
            return null;
         end if;

         -- Create a new array of the correct size and return it
         declare Result : Entity_ID_Array_Access := new Entity_ID_Array (0 .. Count - 1);
         begin
            for K in 0 .. Count - 1 loop
               Result (K) := Temp (K);
            end loop;

            return Result;
         end;
      end;

   end Get_Entities_With;


   -- Return a dynamically allocated array containing all entity IDs that own a specific component type.
    -- This function iterates through all entities in the store and checks if they have the specified component tag.
   function Get_Entity_IDs
  (   S   : Store;
      Tag : Component_Tag) return Entity_ID_Array_Access
   is
     
      Count  : Natural := 0;
      Cursor : Entity_Maps.Cursor := S.Entities.First;

   begin

      -- First pass: count matches
      while Entity_Maps.Has_Element (Cursor) loop
         declare
            E : constant Entity_ID :=
            Entity_Maps.Key (Cursor);
         begin
            if Has_Component (S, E, Tag) then
               Count := Count + 1;
            end if;
         end;

         Cursor := Entity_Maps.Next (Cursor);
      end loop;

      -- If there are no matches, return null, do nothing
      if Count = 0 then
         return null;
      end if;

      -- Allocate result array
      declare
         Result : Entity_ID_Array_Access :=
            new Entity_ID_Array (0 .. Count - 1);

         Index  : Natural := 0;

         Cursor2 : Entity_Maps.Cursor :=
         S.Entities.First;
      
      begin

         -- Second pass: fill array
         while Entity_Maps.Has_Element (Cursor2) loop
            declare
               E : constant Entity_ID :=
               Entity_Maps.Key (Cursor2);
            begin
               if Has_Component (S, E, Tag) then
                  Result (Index) := E;
                  Index := Index + 1;
               end if;
            end;

            Cursor2 := Entity_Maps.Next (Cursor2);
         end loop;

         return Result;
      end;

   end Get_Entity_IDs;

end ECS.Store;