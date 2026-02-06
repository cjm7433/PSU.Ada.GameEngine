--  ecs-systems_movement.adb

-- Concrete behavior for Movement System

with ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Ada.Numerics;                  use Ada.Numerics;

package body ECS.Systems_Movement is

   overriding 
   procedure Update
     (Self : in out Movement_System;
      S    : in out ECS.Store.Store;
      DT   : Float) is
   
   begin
   
      -- Iterate over Transform-owning Entities
      for Cursor in S.Transform.Lookup.Iterate loop
      
         declare

         -- Here is the ID of the Entity that "owns" the current Transform Component
         --    in the iteration of Transform Components in the Transform Lookup
         E : constant Entity_ID := ECS.Store.Transform_Table.Lookups.Key (Cursor);

         -- TODO: ^ Could we do this with CompTable Remove()?
   
         begin

            -- AND-filter happens here! (Motion && Transform Components)
            -- TODO: How to write this with >2 Comp Types?
            -- Could be like if abc.Contains(E) and then lmnop.Contains(E) and then xyz.Contains(E) then 
            -- TODO: Or do we need to put these types in a vector?

            -- Hash lookup within Motion Lookup
            if S.Motion.Lookup.Contains (E) then
         
               declare
               T : Transform  renames S.Transform.Data (S.Transform.Lookup (E));
               M : Motion     renames S.Motion.Data (S.Motion.Lookup (E));
         
               begin

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
            end if;
         end;
      end loop;
   
   end Update;

end ECS.Systems_Movement;