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
with Ada.Text_IO;                   use Ada.Text_IO;   --DEBUG
with ECS.Components.Ball;           use ECS.Components.Ball; --DEBUG
with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with ECS.Components.Collider;       use ECS.Components.Collider;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Math.Physics.AABBs;            use Math.Physics.AABBs;
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

			-- Optional collider
			Index_C : Collider_Table.Index;

			-- Soonest fraction along motion which collision occurred and reflected motion
			Motion_Fraction : Float := Float'Last;
			Reflection : Vector2 := (0.0, 0.0);
         
         begin

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- DO THE LOGIC HERE!
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            -- Integrate acceleration --> velocity
            M.Linear_Velocity    := M.Linear_Velocity + M.Linear_Acceleration * DT;
            M.Angular_Velocity   := M.Angular_Velocity + M.Angular_Acceleration * DT;

            -- Resolve collision if the entity has a collider
            if S.Has_Component(E, ECS.Components.Collider.Collider_Component'Tag) then
               Index_C := S.Collider.Lookup (E);
               C : Collider_Component renames S.Collider.Data (Index_C);
               
               Colliders : Entity_ID_Array_Access := S.Get_Entities_With((0 => ECS.Components.Collider.Collider_Component'Tag));
               
               for J in Colliders'Range loop
                  declare
                     F : constant Entity_ID := Colliders (J);
                     New_Fraction: Float := Float'Last;
                  begin
                     if E /= F then
                        Index_D : constant Collider_Table.Index := S.Collider.Lookup (F);
                        D : Collider_Component renames S.Collider.Data (Index_D);

                        -- Check for collision between this entity and the collider in world space along the motion path
                        New_Fraction := Collision_Sweep(C.Bounding_Box, D.Bounding_Box, M.Linear_Velocity * DT);
                        if New_Fraction <= 1.0 and New_Fraction < Motion_Fraction then
                           Motion_Fraction := New_Fraction;
                           Reflection := Reflect(M.Linear_Velocity, Get_Aligned_Normal(D.Bounding_Box, C.Bounding_Box));
                        end if;
                     end if;
                  end;
               end loop;
            end if;

            -- Simulate bounce collision
            -- TODO:
            --		Will need to be adapted to recursively do collision sweeps for the entirety of the motion path.
            --		i.e. it currently can only bounce one time per frame.
            if Motion_Fraction <= 1.0 then
               -- Translate up to moment of collision
               -- Integrate velocity --> transform
               T.Position  :=   T.Position + M.Linear_Velocity * DT * Motion_Fraction;
               -- Then translate remainder using reflection
               T.Position := T.Position + Reflection * DT * (1.0 - Motion_Fraction);
               M.Linear_Velocity := Reflection;
            else
               -- Integrate velocity --> transform
               T.Position  :=   T.Position + M.Linear_Velocity * DT;
            end if;

            -- Integrate and normalize rotation
            T.Rotation := Rotate(T.Rotation, M.Angular_Velocity * DT);
         end;
      end loop;
   end Update;

end ECS.Systems_Movement;
