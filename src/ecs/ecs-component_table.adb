-- ecs-component_table.adb

-- Implementation of component table operations.
--    - This .adb file specifically defines the Remove() subprogram used in ECS-Store: Remove_Component().

package body ECS.Component_Table is

   -----------------------------------------------------------------
   -- Add
   -- Add a component to the table for an entity
   -----------------------------------------------------------------
    procedure Add
     (T : in out Table;
      E : Entity_ID;
      C : Component_Type) is
      
      Index : Natural;
   
   begin

      if T.Lookup.Contains (E) then
         return;
      end if;

      T.Data.Append (C);

      Index := T.Data.Last_Index;

      T.Lookup.Insert (E, Index);

   end Add;


   -----------------------------------------------------------------
   -- Remove (swap remove)
   -- Remove a component from the table for an entity
   -----------------------------------------------------------------
   procedure Remove
     (T : in out Table;
      E : Entity_ID) is

      Index      : Natural;
      Last_Index : Natural;
      Cursor     : Lookup_Map.Cursor;

   begin

      if not T.Lookup.Contains (E) then
         return;
      end if;

      Index      := T.Lookup.Element (E);
      Last_Index := T.Data.Last_Index;

      -- Swap last element into removed slot
      T.Data (Index) := T.Data (Last_Index);

      -- Update lookup for moved element
      Cursor := T.Lookup.First;

      while Lookup_Map.Has_Element (Cursor) loop
         if Lookup_Map.Element (Cursor) = Last_Index then
            T.Lookup.Replace
              (Lookup_Map.Key (Cursor), Index);
            exit;
         end if;

         Cursor := Lookup_Map.Next (Cursor);
      end loop;

      T.Data.Delete_Last;

      T.Lookup.Delete (E);

   end Remove;


   -----------------------------------------------------------------
   -- Has
   -- Check if an entity has a component in the table
   -----------------------------------------------------------------
   function Has
     (T : Table;
      E : Entity_ID) return Boolean is
   
   begin
      return T.Lookup.Contains (E);
   end Has;


   -----------------------------------------------------------------
   -- Get
   -- Get the component for an entity
   -----------------------------------------------------------------
   function Get
     (T : Table;
      E : Entity_ID) return Component_Type is
      Index : Natural;
   
   begin
   
      Index := T.Lookup.Element (E);
      return T.Data (Index);
   
   end Get;


   -----------------------------------------------------------------
   -- Lookup_Index
   -- Get the index of the component for an entity (for iteration)
   -----------------------------------------------------------------
   function Lookup_Index
     (T : Table;
      E : Entity_ID) return Natural is

   begin
      return T.Lookup.Element (E);
   end Lookup_Index;

end ECS.Component_Table;
