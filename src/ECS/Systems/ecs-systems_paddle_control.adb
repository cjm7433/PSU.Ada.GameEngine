-- ecs-systems_paddle_control.adb
--
-- Implementation of Paddle Control System
-- Reads input state and updates paddle velocity
with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with ECS.Components.Paddle;         use ECS.Components.Paddle;
with Math.Linear_Algebra;           use Math.Linear_Algebra;

package body ECS.Systems_Paddle_Control is

   --------------------------------------------------------
   -- Components_Required
   -- Components_Needed is the list of required components 
   -- Components required: Paddle, Transform, Motion
   --------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Paddle_Control_System)
      return ECS.Components.Component_Tag_Array is
   begin
      -- Paddle Control System requires: Paddle, Transform, Motion
      return (0 => ECS.Components.Paddle.Paddle_Component'Tag,
              1 => ECS.Components.Transform.Transform_Component'Tag,
              2 => ECS.Components.Motion.Motion_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Update: Process paddle input and update motion
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Paddle_Control_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;

   begin
      -- Get entities with the required components
      -- (Paddle, Transform, and Motion components)
      Entities := S.Get_Entities_With (Self.Components_Needed);

      -- If no paddle entities exist, nothing to do
      if Entities = null then
         return;
      end if;

      -- Process each paddle entity (typically just one)
      for I in Entities'Range loop

         declare
            E : constant Entity_ID := Entities (I);

            -- Get components for this paddle entity
            Index_Paddle : constant Natural := S.Paddle.Lookup (E);
            P : Paddle_Component renames S.Paddle.Data (Index_Paddle);

            Index_Transform : constant Natural := S.Transform.Lookup (E);
            T : Transform_Component renames S.Transform.Data (Index_Transform);

            Index_Motion : constant Natural := S.Motion.Lookup (E);
            M : Motion_Component renames S.Motion.Data (Index_Motion);

         begin

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Handle input and set velocity
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            -- Reset velocity (no input = no movement)
            M.Linear_Velocity.X := 0.0;
            M.Linear_Velocity.Y := 0.0;

            -- Apply movement based on input state
            if P.Move_Left and not P.Move_Right then
               M.Linear_Velocity.X := -P.Move_Speed;

            elsif P.Move_Right and not P.Move_Left then
               M.Linear_Velocity.X := P.Move_Speed;

            -- If both pressed or neither pressed, velocity stays 0
            end if;

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Enforce horizontal boundaries (keep paddle on screen)
            -- This prevents the paddle from moving outside Min_X/Max_X
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            if T.Position.X < P.Min_X then
               T.Position.X        := P.Min_X;
               M.Linear_Velocity.X := 0.0;   -- Stop movement at boundary

            elsif T.Position.X > P.Max_X then
               T.Position.X        := P.Max_X;
               M.Linear_Velocity.X := 0.0;   -- Stop movement at boundary
            end if;

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Enforce vertical position (keep paddle on its home row)
            -- Collision resolution can push the paddle's Y position by a
            -- small amount each frame when the ball hits it.  Clamping
            -- T.Position.Y to P.Home_Y here prevents that drift from
            -- accumulating over time and moving the paddle off screen.
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            T.Position.Y        := P.Home_Y;
            M.Linear_Velocity.Y := 0.0;

            -- Note: The Movement System will then integrate this velocity
            -- into the Transform.Position in the next frame

         end;
      end loop;

   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Paddle_Control_System) return String is
   begin
      return "Paddle Control";
   end Name;

end ECS.Systems_Paddle_Control;
