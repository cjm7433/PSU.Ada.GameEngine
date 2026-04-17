-- ecs-systems_collision.adb
--
-- Implementation of Collision System.
-- AABB collision detection and resolution using enum-based layer filtering.
--
-- 4/2/26
-- Changes from original:
--   - Ball now resolves only its single deepest collision per frame.
--     Previously every overlapping pair was resolved independently in the
--     same pass, which caused the ball to hit two adjacent bricks
--     simultaneously, damaging both and potentially double-flipping the
--     velocity back to its original direction.
--   - Sets Ball_Component.Hit_Paddle = True when the ball's deepest
--     collision is with the paddle, so Ball_Physics_System can apply
--     zone-based paddle deflection that frame.
--   - Non-ball solid pairs (e.g. paddle vs wall) are still resolved
--     immediately as before.
--   - Resolve_Collision now uses one-sided separation so that static or
--     semi-static entities (walls, paddle) are never displaced by a
--     collision.  Only the truly moving entity absorbs the full overlap.
--     See Resolve_Collision for the detailed rules.
--   - Ball-vs-paddle collisions are resolved with Resolve_Ball_Paddle
--     instead of the general Resolve_Collision.  The general resolver
--     picks the axis of least penetration, which flips between X and Y
--     when the ball grazes a paddle corner, trapping the ball in an
--     oscillation.  Resolve_Ball_Paddle always separates on Y only,
--     which is correct for a flat horizontal surface and eliminates
--     the corner-sticking behaviour entirely.

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with ECS.Components.Collider;       use ECS.Components.Collider;
with ECS.Components.Ball;           use ECS.Components.Ball;
with ECS.Components.Brick;          use ECS.Components.Brick;
with ECS.Components.Paddle;         use ECS.Components.Paddle;
with Math.Linear_Algebra;           use Math.Linear_Algebra;
with Math.Physics;                  use Math.Physics;
with Audio;                         use Audio;
with Ada.Text_IO;                   use Ada.Text_IO;     -- DEBUG

package body ECS.Systems_Collision is

   Score : Integer := 0;

   procedure Reset_Score is
   begin
      Score := 0;
   end Reset_Score;

   function Get_Score return Integer is
   begin
      return Score;
   end Get_Score;

   ------------------------------------------------------------
   -- Layer_In_Mask
   -- Helper: Check if a layer appears anywhere in a mask.
   ------------------------------------------------------------
   function Layer_In_Mask
     (Layer : Collision_Layer;
      Mask  : Collision_Mask) return Boolean is
   begin
      if Layer = Layer_None then
         return False;
      end if;

      for I in Mask'Range loop
         if Mask (I) = Layer then
            return True;
         end if;
      end loop;

      return False;
   end Layer_In_Mask;


   ------------------------------------------------------------
   -- Entities_Can_Collide
   -- Helper: Two entities can collide if either's layer
   --    is in the other's mask.
   ------------------------------------------------------------
   function Entities_Can_Collide (C1, C2 : Collider_Component) return Boolean is
   begin
      return Layer_In_Mask (C1.Layer, C2.Mask) or
             Layer_In_Mask (C2.Layer, C1.Mask);
   end Entities_Can_Collide;


   procedure Update_Score (Points : Integer) is
   begin
      Score := Score + Points;
   end Update_Score;


   ------------------------------------------------------------
   -- Apply_Brick_Damage
   -- Helper: Damage a brick when hit by ball.
   -- TODO: Should this be handled elsewhere?
   ------------------------------------------------------------
   procedure Apply_Brick_Damage (S : in out Store.Store; Brick_Entity : Entity_ID) is

      Index_B : constant Natural := S.Brick.Lookup (Brick_Entity);
      B : Brick_Component renames S.Brick.Data (Index_B);

   begin
      if not B.Is_Dying and B.Health > 0 then
         B.Health := B.Health - 1;

         if B.Health = 0 then
            B.Is_Dying := True;
            Play_Audio("sfx/Arkanoid SFX (2).wav", False);
         end if;
      end if;

   end Apply_Brick_Damage;


   ---------------------------------------------------------
   -- Components_Needed
   ---------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Collision_System)
      return ECS.Components.Component_Tag_Array is
   begin
      return (0 => ECS.Components.Collider.Collider_Component'Tag,
              1 => ECS.Components.Transform.Transform_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Two-pass collision resolution:
   --
   -- Pass 1 (ball collisions):
   --   For each entity that has a Ball component, scan all other
   --   collidable entities and record every overlapping hit.
   --   Dying bricks are excluded from the scan so the ball passes
   --   through them cleanly after the killing hit.
   --   Resolve only the single deepest overlap so that grazing a
   --   corner shared by two adjacent bricks causes exactly one
   --   bounce and damages exactly one brick.
   --   Paddle hits use Resolve_Ball_Paddle (Y-axis only) to avoid
   --   corner sticking.  All other hits use Resolve_With_Motion.
   --
   -- Pass 2 (non-ball solid pairs):
   --   Resolve all remaining solid-vs-solid overlaps that do not
   --   involve the ball (e.g. paddle sliding into a wall).
   --   These are low-frequency and safe to resolve all at once.
   --
   -- In both passes, separation is absorbed entirely by whichever
   -- entity is moving; static entities (walls, bricks) never move.
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Collision_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;

   begin

      Entities := S.Get_Entities_With (Self.Components_Needed);
      if Entities = null then
         return;
      end if;

      for I in Entities'Range loop
         declare
            E : constant Entity_ID := Entities (I);
         begin
            Index_C : constant Natural := S.Collider.Lookup (E);
            C : Collider_Component renames S.Collider.Data (Index_C);
            Index_T : constant Natural := S.Transform.Lookup (E);
            T : Transform_Component renames S.Transform.Data (Index_T);

            -- Snap collider to transform
            C.Bounding_Box.Center := T.Position;

            -- Signal brick destruction
            if S.Has_Component(E, ECS.Components.Ball.Ball_Component'Tag) then
               if not C.Collided_Entities.Is_Empty then
                  Play_Audio ("sfx/ball_hit.wav", False);
               end if;

               for J of C.Collided_Entities loop
                  begin
                     if S.Has_Component (J, ECS.Components.Brick.Brick_Component'Tag) then
                        Apply_Brick_Damage (S, J);
                     end if;
                  end;
               end loop;
            end if;

            -- Clear colliding entities this frame
            C.Collided_Entities.Clear;
         end;
      end loop;
   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Collision_System) return String is
   begin
      return "Collision";
   end Name;

end ECS.Systems_Collision;