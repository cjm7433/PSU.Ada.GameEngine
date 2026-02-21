-- Implementation of Store operations.

-- This file defines the main ECS store structure that holds entities and their associated components.

-- The store contains:
--   - An Entity map to hold all entities.
--   - A Table which includes:
--      - Component tables for each component type.
--      - Lookup maps to associate Entities with their Components.

-- Additional Component types and their corresponding tables + lookups can be added as needed.
--   - Search for "TODO" to find where!

with Ada.Tags; use type Ada.Tags.Tag;


package body ECS.Store is
   -- Converts Entity_ID to Hash_Type
   function Hash_Entity_ID (
      ID : Entity_ID
   ) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(ID);
   end Hash_Entity_ID;


   -- Initializes the ECS store
   -- NOTE: This procedure should be called before using the ECS store
   procedure Initialize (
      S : in out Store
   ) is
   begin
      S.Entities.Clear;

      S.Transform.Data.Clear;
      S.Transform.Lookup.Clear;

      S.Motion.Data.Clear;
      S.Motion.Lookup.Clear;

      S.Collider.Data.Clear;
      S.Collider.Lookup.Clear;

      S.Render.Data.Clear;
      S.Render.Lookup.Clear;

      S.Paddle.Data.Clear;
      S.Paddle.Lookup.Clear;

      S.Ball.Data.Clear;
      S.Ball.Lookup.Clear;

      S.Brick.Data.Clear;
      S.Brick.Lookup.Clear;

      -- TODO: Add Component Types here
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      S.Next_Entity_ID := 0;
   end Initialize;


   -- Creates a new entity in the ECS store and returns its Entity_ID
   function Create_Entity (
      S : in out Store
   ) return Entity_ID is
      E : Entity;
   begin
      -- Increment Next_Entity_ID
      S.Next_Entity_ID := Entity_ID'Succ (S.Next_Entity_ID);

      E.ID := S.Next_Entity_ID;
      S.Entities.Insert (E.ID, E);

      return E.ID;
   end Create_Entity;


   -- Removes an Entity from the ECS Store
   procedure Destroy_Entity (
      S  : in out Store;
      ID :        Entity_ID
   ) is
   begin
      -- Guard if non-existent
      if not S.Has_Entity (ID) then
         return;
      end if;

      -- Component Removal
      if S.Transform.Lookup.Contains (ID) then
         Transform_Table.Remove (S.Transform, ID);
      end if;

      if S.Motion.Lookup.Contains (ID) then
         Motion_Table.Remove (S.Motion, ID);
      end if;

      if S.Collider.Lookup.Contains (ID) then
         Collider_Table.Remove (S.Collider, ID);
      end if;

      if S.Paddle.Lookup.Contains (ID) then
         Paddle_Table.Remove (S.Paddle, ID);
      end if;

      if S.Ball.Lookup.Contains (ID) then
         Ball_Table.Remove (S.Ball, ID);
      end if;

      if S.Brick.Lookup.Contains (ID) then
         Brick_Table.Remove (S.Brick, ID);
      end if;

      if S.Render.Lookup.Contains (ID) then
         Render_Table.Remove (S.Render, ID);
      end if;

      -- TODO: Add Component Types here
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      S.Entities.Delete (ID);
   end Destroy_Entity;


   -- Returns whether or not the ECS Store contains an Entity of the given Entity_ID
   function Has_Entity (
      S  : Store;
      ID : Entity_ID
   ) return Boolean is
   begin
      return S.Entities.Contains (ID);
   end Has_Entity;


   -- Creates a new component of the passed type and attaches it to the given Entity_ID
   procedure Add_Component (
      S   : in out Store;
      E   :        Entity_ID;
      Tag :        Component_Tag
   ) is
   begin
      -- Runtime Component_Type Checks

      -- The following each add a Component to Component Vector and Lookup like:
      -- If the Tag matches the Component type:
      --    If the Lookup does not contain the Entity_ID:
      --       Add it to the end
      --       Add the Component data to the vector of Components

      --TODO: This looks rough... also remove default values from factory. Defaults should live in definition.
      if Tag = Transform_Component'Tag then
         if not S.Transform.Lookup.Contains (E) then
            S.Transform.Lookup.Insert
               (E, Transform_Table.Index (S.Transform.Data.Length));

            S.Transform.Data.Append
              (Transform_Component'
                 (   Position => (0.0, 0.0),
                     Rotation => 0.0,
                     Scale    => (1.0, 1.0)));
         end if;

      elsif Tag = Motion_Component'Tag then
         if not S.Motion.Lookup.Contains (E) then
            S.Motion.Lookup.Insert
              (E, Motion_Table.Index (S.Motion.Data.Length));

            S.Motion.Data.Append
              (Motion_Component'
                 (   others => (0.0, 0.0)));
         end if;

      elsif Tag = Collider_Component'Tag then
         if not S.Collider.Lookup.Contains (E) then
            S.Collider.Lookup.Insert
               (E, Collider_Table.Index (S.Collider.Data.Length));

            S.Collider.Data.Append(Collider_Component'(others => <>));
         end if;

      elsif Tag = Paddle_Component'Tag then
         if not S.Paddle.Lookup.Contains (E) then
            S.Paddle.Lookup.Insert
               (E, Paddle_Table.Index (S.Paddle.Data.Length));

            S.Paddle.Data.Append
            (Paddle_Component'
               (  Move_Speed => 500.0,
                  Min_X      => 50.0,
                  Max_X      => 750.0,
                  Move_Left  => False,
                  Move_Right => False));
         end if;

      elsif Tag = Ball_Component'Tag then
         if not S.Ball.Lookup.Contains (E) then
            S.Ball.Lookup.Insert
               (E, Ball_Table.Index (S.Ball.Data.Length));
                  S.Transform.Data.Clear;
      S.Transform.Lookup.Clear;

      S.Motion.Data.Clear;
      S.Motion.Lookup.Clear;

      S.Collider.Data.Clear;
      S.Collider.Lookup.Clear;

      S.Paddle.Data.Clear;
      S.Paddle.Lookup.Clear;

      S.Ball.Data.Clear;
      S.Ball.Lookup.Clear;

      S.Brick.Data.Clear;
      S.Brick.Lookup.Clear;

      S.Render.Data.Clear;
      S.Render.Lookup.Clear;
            S.Ball.Data.Append
            (Ball_Component'
               (  Min_Speed        => 200.0,
                  Max_Speed        => 800.0,
                  Base_Speed       => 400.0,
                  Bounce_Damping   => 1.0,
                  Is_Attached      => True,
                  Attach_Offset_X  => 0.0));
         end if;

      elsif Tag = Brick_Component'Tag then
         if not S.Brick.Lookup.Contains (E) then
            S.Brick.Lookup.Insert
               (E, Brick_Table.Index (S.Brick.Data.Length));

            S.Brick.Data.Append
            (Brick_Component'
               (  Brick_Kind  => Normal,
                  Health      => 1,
                  Max_Health  => 1,
                  Points      => 10,
                  Is_Dying    => False,
                  Death_Timer => 0.0));
         end if;

      elsif Tag = Render_Component'Tag then
         if not S.Render.Lookup.Contains (E) then
            S.Render.Lookup.Insert
               (E, Render_Table.Index (S.Render.Data.Length));

            S.Render.Data.Append
            (Render_Component'
               (  Shape   => Rectangle,
                  Tint    => (1.0, 1.0, 1.0, 1.0),
                  Layer   => 0,
                  Visible => True));
         end if;

      -- TODO: Add Component Types here
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      else
         raise Program_Error with "Unknown component tag"; --TODO: raise with info on which tag caused the exception
      end if;
   end Add_Component;


   -- Removes a component of the passed type from the given Entity_ID
   procedure Remove_Component (
      S   : in out Store;
      E   : Entity_ID;
      Tag : Component_Tag
   ) is
   begin
      -- Steps to Remove_Component: --> These are handled in Component_Table's Remove()
            --  Find index of component to remove
            --  Swap last element into that index
            --  Update the swapped entity’s index
            --  Remove last vector element
            --  Remove entity from lookup

      -- Runtime Component_Type Checks

      -- A fun fact I am learning (aka trying to learn) about Ada:
      --    Rule of thumb:
      --    Whenever you pass a record aggregate to a container operation, qualify it with Type'(...).

      -- The following each remove a Component from its Lookup and Component Vector like:
      -- If the Tag matches the Component type:
      --    If the Entity_ID is present in the Lookup
      --       Remove the Component from Component vector and Lookup (using Component_Table Remove())

      if Tag = Transform_Component'Tag then
         if S.Transform.Lookup.Contains (E) then
            Transform_Table.Remove (S.Transform, E);
         end if;

      elsif Tag = Motion_Component'Tag then
         if S.Motion.Lookup.Contains (E) then
            Motion_Table.Remove (S.Motion, E);
         end if;

      elsif Tag = Collider_Component'Tag then
         if S.Collider.Lookup.Contains (E) then
            Collider_Table.Remove (S.Collider, E);
         end if;

      elsif Tag = Paddle_Component'Tag then
         if S.Paddle.Lookup.Contains (E) then
            Paddle_Table.Remove (S.Paddle, E);
         end if;

      elsif Tag = Ball_Component'Tag then
         if S.Ball.Lookup.Contains (E) then
            Ball_Table.Remove (S.Ball, E);
         end if;

      elsif Tag = Brick_Component'Tag then
         if S.Brick.Lookup.Contains (E) then
            Brick_Table.Remove (S.Brick, E);
         end if;

      elsif Tag = Render_Component'Tag then
         if S.Render.Lookup.Contains (E) then
            Render_Table.Remove (S.Render, E);
         end if;

      -- TODO: Add other Component types here!
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      else
         raise Program_Error with "Unknown component tag"; --TODO: raise with info on which tag caused the exception
      end if;
   end Remove_Component;


   -- Returns whether or not the given Entity_ID contains an instance of the passed component type
   function Has_Component (
      S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag
   ) return Boolean is
   begin
      -- Runtime Component_Type Checks

      -- The following each return a Boolean as if it were:
      -- Boolean := Does this Entity_ID exist within the Lookup?

      if Tag = Transform_Component'Tag then
         return S.Transform.Lookup.Contains (E);

      elsif Tag = Motion_Component'Tag then
         return S.Motion.Lookup.Contains (E);

      elsif Tag = Collider_Component'Tag then
         return S.Collider.Lookup.Contains (E);

      elsif Tag = Paddle_Component'Tag then
         return S.Paddle.Lookup.Contains (E);

      elsif Tag = Ball_Component'Tag then
         return S.Ball.Lookup.Contains (E);

      elsif Tag = Brick_Component'Tag then
         return S.Brick.Lookup.Contains (E);

      elsif Tag = Render_Component'Tag then
         return S.Render.Lookup.Contains (E);

      -- TODO: Add other Component types here!
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      else
         return False;
      end if;
   end Has_Component;


   -- Returns an instance of the passed component type on the given Entity_ID
   function Get_Component (
      S   : Store;
      E   : Entity_ID;
      Tag : Component_Tag
   ) return Component'Class is
   begin
      -- Runtime Component_Type Checks

      -- The following each return a Component as if it were:
      -- Component := Component_Vector(Index of Entity in Lookup)

      if Tag = Transform_Component'Tag then
         return S.Transform.Data(S.Transform.Lookup (E));

      elsif Tag = Motion_Component'Tag then
         return S.Motion.Data(S.Motion.Lookup (E));

      elsif Tag = Collider_Component'Tag then
         return S.Collider.Data(S.Collider.Lookup (E));

      elsif Tag = Paddle_Component'Tag then
         return S.Paddle.Data(S.Paddle.Lookup (E));

      elsif Tag = Ball_Component'Tag then
         return S.Ball.Data(S.Ball.Lookup (E));

      elsif Tag = Brick_Component'Tag then
         return S.Brick.Data(S.Brick.Lookup (E));

      elsif Tag = Render_Component'Tag then
         return S.Render.Data(S.Render.Lookup (E));

      -- TODO: Add other Component types here!
      -- TODO: Can we avoid this manual Component Type input? (generic/automate?)

      else
         raise Program_Error with "Unknown Component tag"; --TODO: raise with info on which tag caused the exception
      end if;
   end Get_Component;


   --------------------------------------------------------------------------------
   -- Get_Entities_With
   -- Gets an array of Entity IDs that have all the specified component tags
   --------------------------------------------------------------------------------
   function Get_Entities_With
  (   S    : Store;
      Tags : Component_Tag_Array) return Entity_ID_Array_Access is

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
            -- Copy matches from temp to result
            for K in 0 .. Count - 1 loop
               Result (K) := Temp (K);
            end loop;

            return Result;

         end;

      end;

   end Get_Entities_With;


   -------------------------------------------------------------------------------
   -- Get_Entity_IDs
   -- Return a dynamically allocated array containing all entity IDs
   --    that own a specific component type.
   -- This function iterates through all entities in the store and
   --    checks if they have the specified component tag.
   -------------------------------------------------------------------------------
   function Get_Entity_IDs
  (   S   : Store;
      Tag : Component_Tag) return Entity_ID_Array_Access is

      Count  : Natural := 0;
      Cursor : Entity_Maps.Cursor := S.Entities.First;

   begin

      -- First pass: count matches
      -- While there are entities in Entity_Map ...
      while Entity_Maps.Has_Element (Cursor) loop

         declare
            -- E is the entity cursor points to
            E : constant Entity_ID := Entity_Maps.Key (Cursor);

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
         -- Result is a new array of Entity_IDs with size equal to the count of matches
         Result : Entity_ID_Array_Access := new Entity_ID_Array (0 .. Count - 1);

         Index  : Natural := 0;

         Cursor2 : Entity_Maps.Cursor := S.Entities.First;

      begin

         -- Second pass: fill array
         while Entity_Maps.Has_Element (Cursor2) loop

            declare
               -- E is the entity cursor points to
               E : constant Entity_ID := Entity_Maps.Key (Cursor2);

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