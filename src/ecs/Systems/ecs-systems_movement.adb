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
with ECS.Components.Paddle;         use ECS.Components.Paddle;
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
            E : constant Entity_ID := Entities(I);

            Index_T : constant Transform_Table.Index := S.Transform.Lookup(E);
            T : Transform_Component renames S.Transform.Data(Index_T);

            Index_M : constant Motion_Table.Index := S.Motion.Lookup(E);
            M : Motion_Component renames S.Motion.Data(Index_M);

            Motion_Fraction : Float := Float'Last;
            Reflection      : Vector2 := (0.0, 0.0);
            Collidee        : Entity_ID;

         begin
            -- Integrate acceleration
            M.Linear_Velocity  := M.Linear_Velocity + M.Linear_Acceleration * DT;
            M.Angular_Velocity := M.Angular_Velocity + M.Angular_Acceleration * DT;

            if S.Has_Component(E, ECS.Components.Collider.Collider_Component'Tag) then
               declare
                  Index_C : constant Collider_Table.Index := S.Collider.Lookup(E);
                  C : Collider_Component renames S.Collider.Data(Index_C);

                  Colliders : Entity_ID_Array_Access :=
                  S.Get_Entities_With((0 => ECS.Components.Collider.Collider_Component'Tag));
               begin
                  -- ALWAYS sync collider before sweep
                  C.Bounding_Box.Center := T.Position;

                  -- First collision sweep
                  for J in Colliders'Range loop
                     declare
                        F : constant Entity_ID := Colliders(J);
                        New_Fraction : Float := Float'Last;
                     begin
                        if E /= F then
                           declare
                              Index_D : constant Collider_Table.Index := S.Collider.Lookup(F);
                              D : Collider_Component renames S.Collider.Data(Index_D);
                           begin
                              New_Fraction :=
                              Collision_Sweep(C.Bounding_Box, D.Bounding_Box, M.Linear_Velocity * DT);

                              if New_Fraction <= 1.0 and then New_Fraction < Motion_Fraction then
                                 Motion_Fraction := New_Fraction;
                                 Collidee := F;
                                 Reflection :=
                                 Reflect(M.Linear_Velocity,
                                          Get_Aligned_Normal(D.Bounding_Box, C.Bounding_Box));
                              end if;
                           end;
                        end if;
                     end;
                  end loop;

                  if Motion_Fraction <= 1.0 then
                     C.Collided_Entities.Append(Collidee);

                     -- Move to first collision
                     T.Position := T.Position + M.Linear_Velocity * DT * Motion_Fraction;

                     -- Remaining time
                     declare
                        Remaining : Float := 1.0 - Motion_Fraction;
                     begin
                        -- Apply first reflection immediately
                        M.Linear_Velocity := Reflection;

                        --TODO recursive collision sweeps to entities can collide more than once per frame
                        -- HACK double sweep collision.
                        if not S.Has_Component(E, ECS.Components.Paddle.Paddle_Component'Tag) then
                           -- Sync collider again
                           C.Bounding_Box.Center := T.Position;
                           declare
                              Second_Fraction : Float := Float'Last;
                              Second_Hit      : Boolean := False;
                              Second_Target   : Entity_ID;
                           begin
                              for J in Colliders'Range loop
                                 declare
                                    F : constant Entity_ID := Colliders(J);
                                    New_Fraction : Float := Float'Last;
                                 begin
                                    if E /= F then
                                       declare
                                          Index_D : constant Collider_Table.Index := S.Collider.Lookup(F);
                                          D : Collider_Component renames S.Collider.Data(Index_D);
                                       begin
                                          New_Fraction :=
                                          Collision_Sweep(
                                             C.Bounding_Box,
                                             D.Bounding_Box,
                                             M.Linear_Velocity * DT * Remaining
                                          );

                                          if New_Fraction <= 1.0 and then New_Fraction < Second_Fraction then
                                             Second_Fraction := New_Fraction;
                                             Second_Target   := F;
                                             Second_Hit      := True;
                                          end if;
                                       end;
                                    end if;
                                 end;
                              end loop;

                              if Second_Hit then
                                 C.Collided_Entities.Append(Second_Target);

                                 -- Move to second collision
                                 T.Position :=
                                 T.Position + M.Linear_Velocity * DT * (Remaining * Second_Fraction);

                                 -- Reflect AGAIN using current velocity
                                 declare
                                    Index_D : constant Collider_Table.Index := S.Collider.Lookup(Second_Target);
                                    D : Collider_Component renames S.Collider.Data(Index_D);
                                 begin
                                    M.Linear_Velocity :=
                                    Reflect(M.Linear_Velocity,
                                             Get_Aligned_Normal(D.Bounding_Box, C.Bounding_Box));
                                 end;

                                 Remaining := Remaining * (1.0 - Second_Fraction);
                              end if;

                              -- Final movement
                              T.Position := T.Position + M.Linear_Velocity * DT * Remaining;
                           end;
                        end if;
                        -- HACK end
                     end;
                  else
                     -- No collision
                     T.Position := T.Position + M.Linear_Velocity * DT;
                  end if;
               end;
            else
               -- No collider
               T.Position := T.Position + M.Linear_Velocity * DT;
            end if;

            -- Rotation
            T.Rotation := Rotate(T.Rotation, M.Angular_Velocity * DT);
         end;
      end loop;
   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Movement_System) return String is
   begin
      return "Movement";
   end Name;

end ECS.Systems_Movement;
