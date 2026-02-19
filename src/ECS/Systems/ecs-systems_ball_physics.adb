-- ecs-systems_ball_physics.adb
--
-- Implementation of Ball Physics System
-- Handles ball speed limits and attachment to paddle

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with ECS.Components_Ball;           use ECS.Components_Ball;
with ECS.Components_Paddle;         use ECS.Components_Paddle;
with Math.Linear_Algebra;           use Math.Linear_Algebra;

package body ECS.Systems_Ball_Physics is

   ------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Ball, Transform, Motion
   ------------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Ball_Physics_System)
      return ECS.Components.Component_Tag_Array is
   
   begin
      -- Ball Physics System requires: Ball, Transform, Motion
      return (0 => ECS.Components_Ball.Ball'Tag,
              1 => ECS.Components_Transform.Transform'Tag,
              2 => ECS.Components_Motion.Motion'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update: Process ball physics
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Ball_Physics_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;
      Paddle_Entities : Entity_ID_Array_Access;
   
   begin
   
      -- Get entities with the required components
      Entities := S.Get_Entities_With (Self.Components_Needed);

      if Entities = null then
         return;
      end if;

      -- Get paddle entity (for attachment logic)
      -- Query for entities with Paddle + Transform
      Paddle_Entities := S.Get_Entities_With (
         (0 => ECS.Components_Paddle.Paddle'Tag,
          1 => ECS.Components_Transform.Transform'Tag));

      -- Process each ball
      for I in Entities'Range loop
         
         declare
            E : constant Entity_ID := Entities (I);

            Index_Ball : constant Natural := S.Ball.Lookup (E);
            B : Ball renames S.Ball.Data (Index_Ball);

            Index_Transform : constant Natural := S.Transform.Lookup (E);
            T : Transform renames S.Transform.Data (Index_Transform);

            Index_Motion : constant Natural := S.Motion.Lookup (E);
            M : Motion renames S.Motion.Data (Index_Motion);

         begin

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Handling Ball attachment to Paddle
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if B.Is_Attached then
               
               -- Ball is stuck to paddle, follow paddle position               
               if Paddle_Entities /= null and then Paddle_Entities'Length > 0 then
                  
                  declare
                     Paddle_E : constant Entity_ID := Paddle_Entities (0);
                     Index_Paddle_Transform : constant Natural := 
                        S.Transform.Lookup (Paddle_E);
                     Paddle_T : Transform renames S.Transform.Data (Index_Paddle_Transform);
                  
                  begin
                     -- Position ball above paddle center (with offset)
                     T.Position.X := Paddle_T.Position.X + B.Attach_Offset_X;
                     T.Position.Y := Paddle_T.Position.Y + 20.0;  -- 20 units above paddle
                     
                     -- Keep ball stationary while attached
                     M.Linear_Velocity := (0.0, 0.0);
                  end;
               end if;

               -- Note: Launch logic (setting Is_Attached to False) would be
               -- handled by input system or game logic elsewhere
               
            else
               -- Ball is free, apply physics constraints
               
               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               -- Clamp speed to min/max limits
               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               declare
                  Current_Speed : constant Float := Length (M.Linear_Velocity);
               
               begin
                  if Current_Speed > B.Max_Speed then
                     -- Too fast, normalize and scale to max speed
                     M.Linear_Velocity := Normalize (M.Linear_Velocity) * B.Max_Speed;
                     
                  elsif Current_Speed < B.Min_Speed and Current_Speed > 0.01 then
                     -- Too slow, normalize and scale to min speed
                     M.Linear_Velocity := Normalize (M.Linear_Velocity) * B.Min_Speed;
                  end if;
               end;

               -- Note: Bouncing logic is handled by Collision System
               -- This system only enforces speed limits
            end if;

         end;
      end loop;

   end Update;

end ECS.Systems_Ball_Physics;
