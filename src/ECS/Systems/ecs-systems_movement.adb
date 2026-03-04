-- ecs-systems_movement.adb
--
-- Implementation of Movement System.
-- Performs explicit Euler integration of acceleration → velocity → position.
--
-- Integration steps per entity:
--   1. velocity += acceleration * dt
--   2. position += velocity * dt
--   3. rotation += angular_velocity * dt
--   4. rotation = wrap(rotation, 0, 2π)
--
-- The wrapping step (4) prevents rotation from growing unbounded.
-- Without it, rotation would eventually lose precision or overflow.

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Ada.Numerics;                  use Ada.Numerics;

package body ECS.Systems_Movement is
   
   ---------------------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- (Transform and Motion)
   ---------------------------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Movement_System)
      return ECS.Components.Component_Tag_Array
   is
   begin
      return (0 => ECS.Components.Transform.Transform_Component'Tag,
              1 => ECS.Components.Motion.Motion_Component'Tag);
   end Components_Needed;


   ---------------------------------------------------------------------------
   -- Update
   -- Update will execute movement logic
   ---------------------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Movement_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;     -- Array of Entity_IDs for Systems to query

   begin

      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- Store performs filtering using declared requirements
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
            T : Transform_Component renames S.Transform.Data (Index_T);

            Index_M : constant Motion_Table.Index := S.Motion.Lookup (E);
            M : Motion_Component renames S.Motion.Data (Index_M);
         
         begin

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- DO THE LOGIC HERE!
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            -- Integrate acceleration --> velocity
            M.Linear_Velocity    := M.Linear_Velocity + M.Linear_Acceleration * DT;

            M.Angular_Velocity   := M.Angular_Velocity + M.Angular_Acceleration * DT;
      
            -- Integrate velocity --> transform
            T.Position  :=   T.Position + M.Linear_Velocity * Dt;

            -- Integrate and normalize rotation (TODO: hopefully X is the right scalar component!)
            -- TODO: Normalize with LinAlg library instead
            T.Rotation := (T.Rotation + M.Angular_Velocity.X * DT);
            -- would be nice to do: T.Rotation := (T.Rotation + M.Angular_Velocity.X * DT) mod (2.0 * Pi); instead
            
            -- Rotation should probably be wrapped between 0 a 2pi
            -- TODO: Here or elsewhere (like in a Movement_Normalize system)?
            -- TODO: Do I need to make 2pi a constant (would it help reduce overhead)?
            -- TODO: Normalize with LinAlg library instead

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