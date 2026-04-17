-- ecs-systems_ball_physics.adb
--
-- Implementation of Ball Physics System.
-- Handles ball speed limits, attachment to paddle, and paddle deflection.
--
-- Changes from original:
--   - Added classic Arkanoid 5-zone paddle deflection.
--     When Ball_Component.Hit_Paddle is True (set by the Collision System),
--     the ball's outgoing direction is replaced by a fixed angle determined
--     by where on the paddle the ball made contact.  Speed magnitude is
--     preserved.  Hit_Paddle flag is cleared after processing so it fires for
--     exactly one frame.
--
-- Paddle zones (left edge = 0.0, right edge = 1.0):
--   Zone 1  0.00 - 0.20   -60 degrees from vertical  (sharp left)
--   Zone 2  0.20 - 0.40   -30 degrees from vertical  (shallow left)
--   Zone 3  0.40 - 0.60     0 degrees from vertical  (straight up)
--   Zone 4  0.60 - 0.80   +30 degrees from vertical  (shallow right)
--   Zone 5  0.80 - 1.00   +60 degrees from vertical  (sharp right)

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with ECS.Components.Ball;           use ECS.Components.Ball;
with ECS.Components.Paddle;         use ECS.Components.Paddle;
with ECS.Components.Collider;       use ECS.Components.Collider;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Ada.Numerics;                  use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body ECS.Systems_Ball_Physics is

   ------------------------------------------------------------
   -- Paddle_Zone_Angle
   -- Helper: Return the outgoing angle (radians, measured
   --   clockwise from straight up / negative-Y) for a given
   --   normalised hit position on the paddle (0.0 = left edge,
   --   1.0 = right edge).
   --
   -- Zone boundaries and angles:
   --   0.00 - 0.20  ->  -60 deg  (-Pi/3)
   --   0.20 - 0.40  ->  -30 deg  (-Pi/6)
   --   0.40 - 0.60  ->    0 deg  (straight up)
   --   0.60 - 0.80  ->  +30 deg  (+Pi/6)
   --   0.80 - 1.00  ->  +60 deg  (+Pi/3)
   ------------------------------------------------------------
   function Paddle_Zone_Angle (Hit_T : Float) return Float is
      -- Clamp to [0, 1] for safety
      T : constant Float := Float'Min (1.0, Float'Max (0.0, Hit_T));
   begin
      if T < 0.20 then
         return -Pi / 3.0;       -- -60 degrees: sharp left
      elsif T < 0.40 then
         return -Pi / 6.0;       -- -30 degrees: shallow left
      elsif T < 0.60 then
         return 0.0;             --   0 degrees: straight up
      elsif T < 0.80 then
         return Pi / 6.0;        -- +30 degrees: shallow right
      else
         return Pi / 3.0;        -- +60 degrees: sharp right
      end if;
   end Paddle_Zone_Angle;


   ------------------------------------------------------------
   -- Components_Needed
   -- Components required: Ball, Transform, Motion
   ------------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Ball_Physics_System)
      return ECS.Components.Component_Tag_Array is
   begin
      return (0 => ECS.Components.Ball.Ball_Component'Tag,
              1 => ECS.Components.Transform.Transform_Component'Tag,
              2 => ECS.Components.Motion.Motion_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update: Process ball physics
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Ball_Physics_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities        : Entity_ID_Array_Access;
      Paddle_Entities : Entity_ID_Array_Access;

   begin

      Entities := S.Get_Entities_With (Self.Components_Needed);
      if Entities = null then
         return;
      end if;

      -- Get paddle entity (for attachment and deflection logic)
      Paddle_Entities := S.Get_Entities_With (
         (0 => ECS.Components.Paddle.Paddle_Component'Tag,
          1 => ECS.Components.Transform.Transform_Component'Tag));

      -- Process each ball
      for I in Entities'Range loop

         declare
            E : constant Entity_ID := Entities (I);

            Index_Ball : constant Natural := S.Ball.Lookup (E);
            B : Ball_Component renames S.Ball.Data (Index_Ball);

            Index_Transform : constant Natural := S.Transform.Lookup (E);
            T : Transform_Component renames S.Transform.Data (Index_Transform);

            Index_Motion : constant Natural := S.Motion.Lookup (E);
            M : Motion_Component renames S.Motion.Data (Index_Motion);

         begin

            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Ball attachment to paddle
            --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if B.Is_Attached then

               if Paddle_Entities /= null and then Paddle_Entities'Length > 0 then
                  declare
                     Paddle_E : constant Entity_ID := Paddle_Entities (0);
                     Index_Paddle_T : constant Natural :=
                        S.Transform.Lookup (Paddle_E);
                     Paddle_T : Transform_Component renames
                        S.Transform.Data (Index_Paddle_T);
                  begin
                     T.Position.X := Paddle_T.Position.X + B.Attach_Offset_X;
                     T.Position.Y := Paddle_T.Position.Y + B.Attach_Offset_Y;
                     M.Linear_Velocity := (X => 0.0, Y => 0.0);
                  end;
               end if;

            else

               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               -- Paddle deflection (zone-based)
               -- Fires exactly once per paddle hit,
               -- then Hit_Paddle is cleared.
               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               if B.Hit_Paddle then
                  B.Hit_Paddle := False;

                  if Paddle_Entities /= null and then Paddle_Entities'Length > 0 then
                     declare
                        Paddle_E : constant Entity_ID := Paddle_Entities (0);

                        Index_Paddle_T : constant Natural :=
                           S.Transform.Lookup (Paddle_E);
                        Paddle_T : Transform_Component renames
                           S.Transform.Data (Index_Paddle_T);

                        Index_Paddle_C : constant Natural :=
                           S.Collider.Lookup (Paddle_E);
                        Paddle_C : Collider_Component renames
                           S.Collider.Data (Index_Paddle_C);

                        -- Normalised hit position: 0.0 = left edge, 1.0 = right edge
                        Paddle_Left  : constant Float :=
                           Paddle_T.Position.X - Paddle_C.Bounding_Box.Half_Size.X;
                        Paddle_Width : constant Float :=
                           Paddle_C.Bounding_Box.Half_Size.X * 2.0;
                        Hit_T        : constant Float :=
                           (T.Position.X - Paddle_Left) / Paddle_Width;

                        -- Zone angle and current speed
                        Angle        : constant Float := Paddle_Zone_Angle (Hit_T);
                        Speed        : constant Float := Length (M.Linear_Velocity);
                        Use_Speed    : constant Float :=
                           (if Speed < B.Min_Speed then B.Base_Speed else Speed);

                     begin
                        -- Apply outgoing direction: angle is from straight up (-Y)
                        -- so X = sin(angle), Y = -cos(angle)
                        M.Linear_Velocity :=
                           (X =>  Sin (Angle) * Use_Speed,
                            Y => -Cos (Angle) * Use_Speed);
                     end;
                  end if;

               end if;

               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               -- Clamp speed to min/max limits
               --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               declare
                  Current_Speed : constant Float := Length (M.Linear_Velocity);
               begin
                  if Current_Speed > B.Max_Speed then
                     M.Linear_Velocity :=
                        Normalize (M.Linear_Velocity) * B.Max_Speed;

                  elsif Current_Speed < B.Min_Speed and Current_Speed > 0.01 then
                     M.Linear_Velocity :=
                        Normalize (M.Linear_Velocity) * B.Min_Speed;
                  end if;
               end;

            end if;

         end;
      end loop;

   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Ball_Physics_System) return String is
   begin
      return "Ball Physics";
   end Name;

end ECS.Systems_Ball_Physics;
