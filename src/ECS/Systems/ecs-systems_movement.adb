--  ecs-systems_movement.adb

-- Concrete behavior for Movement System
-- Some of these with/use might not be needed (lookin at you, numerics)

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Ada.Numerics;                  use Ada.Numerics;

package body ECS.Systems_Movement is

   -- Components_Needed is the list of required components 
   -- (Transform and Motion)
   overriding
   function Components_Needed
     (Self : Movement_System)
      return ECS.Components.Component_Tag_Array
   is
   begin
      return (0 => ECS.Components_Transform.Transform'Tag,
              1 => ECS.Components_Motion.Motion'Tag);
   end Components_Needed;


   -- Update will execute logic for the system
   overriding
   procedure Update
     (Self : in out Movement_System;
      S    : in out Store.Store;
      DT   : Float)
   is
      Entities : Entity_ID_Array_Access;

   begin

      ---------------------------------------------------------
      -- Store performs filtering using declared requirements
      ---------------------------------------------------------

      -- Get entities with the required components (Transform and Motion)
      Entities := S.Get_Entities_With (Self.Components_Needed);

      -- If there are no entities with that component type set, do nothing
      if Entities = null then
         return;
      end if;

      for I in Entities'Range loop

         declare

            -- Get the entity ID from the array of entities returned by the store's filtering function
            E : constant Entity_ID := Entities (I);

            -- Get the components for the entity (Transform and Motion )
            Index_T : constant Transform_Table.Index := S.Transform.Lookup (E);
            T : Transform renames S.Transform.Data (Index_T);

            Index_M : constant Motion_Table.Index := S.Motion.Lookup (E);
            M : Motion renames S.Motion.Data (Index_M);
         
         begin

            -- DO THE LOGIC HERE!

            -- Integrate acceleration --> velocity
            M.Linear_Velocity    := M.Linear_Velocity + M.Linear_Acceleration * DT;

            M.Angular_Velocity   := M.Angular_Velocity + M.Angular_Acceleration * DT;
      
            -- Integrate velocity --> transform
            T.Position  :=   T.Position + M.Linear_Velocity * Dt;

            -- Integrate and normalize rotation (TODO: hopefully X is the right scalar component!)
            T.Rotation := (T.Rotation + M.Angular_Velocity.X * DT);
            -- would be nice to do: T.Rotation := (T.Rotation + M.Angular_Velocity.X * DT) mod (2.0 * Pi); instead
            
            -- Rotation should probably be wrapped between 0 a 2pi
            -- TODO: Here or elsewhere (like in a Movement_Normalize system)?
            -- TODO: Do I need to make 2pi a constant (would it help reduce overhead)?

            -- Wrap rotation between 0 and 2pi
            if T.Rotation >= (2.0 * Pi) then
               T.Rotation := T.Rotation - (2.0 * Pi);
            
            elsif T.Rotation < 0.0 then
               T.Rotation := T.Rotation + (2.0 * Pi);
            
            end if;
         end;
      end loop;

   end Update;

end ECS.Systems_Movement;