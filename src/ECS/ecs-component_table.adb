-- ecs-component_table.adb

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines a generic template for Component tables used in ECS-Store for dense Component storage.
-- This .adb file specifically defines the Remove() subprogram used in ECS-Store: Remove_Component().

package body ECS.Component_Table is

    -- Generic Component Table: Remove Utility
   procedure Remove
     (T : in out Table;             -- Component table to remove Component from
      E : Entity_ID) is

      -- Steps to Remove_Component:
         --    Find index of component to remove
         --    Swap last element into that index
         --    Update the swapped Entity’s index
         --    Remove last vector element
         --    Remove entity from lookup

      --    Find index of component to remove
      Remove_Index : constant Index := T.Lookup (E);

      -- Find the last element
      Last_Index   : constant Index := Index (T.Data.Last_Index);

      -- Tracker for which Entity gets swapped
      Swapped_Entity : Entity_ID;

   begin
      
      -- Swap last element into that index

      -- If not last element, swap last element and element to be removed
      if Remove_Index /= Last_Index then

         -- Overwrite removed Component with last Component
         -- This keeps the Component vector dense (contiguous)
         T.Data (Remove_Index) := T.Data (Last_Index);

         -- Find which entity owned the last Component
         -- Reverse search (Index -> Entity_ID)
         -- This is not O(1) but O(n)
         for Cursor in T.Lookup.Iterate loop
            -- Cursor gives the index
            -- If Cursor == Last_Index, we have found the owner
            if Lookups.Element (Cursor) = Last_Index then
               Swapped_Entity := Lookups.Key (Cursor);
               exit;
            end if;
         end loop;

         -- Update swapped entity index
         T.Lookup.Replace (Swapped_Entity, Remove_Index);
      end if;

      -- Remove last element
      T.Data.Delete_Last;

      -- Remove Entity mapping
      T.Lookup.Delete (E);
      
   end Remove;

end ECS.Component_Table;
