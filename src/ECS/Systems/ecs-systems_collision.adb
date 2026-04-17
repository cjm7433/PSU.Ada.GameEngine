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


   ------------------------------------------------------------
   -- AABB_Overlap
   -- Helper: Check if two AABBs overlap.
   ------------------------------------------------------------
   function AABB_Overlap
     (Pos1 : Vector2; Half1 : Vector2;
      Pos2 : Vector2; Half2 : Vector2) return Boolean is

      Separated_X : constant Boolean :=
         (Pos1.X + Half1.X < Pos2.X - Half2.X) or
         (Pos1.X - Half1.X > Pos2.X + Half2.X);

      Separated_Y : constant Boolean :=
         (Pos1.Y + Half1.Y < Pos2.Y - Half2.Y) or
         (Pos1.Y - Half1.Y > Pos2.Y + Half2.Y);

   begin
      return not (Separated_X or Separated_Y);
   end AABB_Overlap;


   ------------------------------------------------------------
   -- Penetration_Depth
   -- Helper: Return the scalar penetration depth of the
   --   axis of least penetration between two AABBs.
   -- A larger value means the objects are more deeply overlapped.
   -- Used to select the single deepest ball collision per frame.
   ------------------------------------------------------------
   function Penetration_Depth
     (Pos1 : Vector2; Half1 : Vector2;
      Pos2 : Vector2; Half2 : Vector2) return Float is

      Overlap_X : constant Float :=
         (Half1.X + Half2.X) - abs (Pos1.X - Pos2.X);

      Overlap_Y : constant Float :=
         (Half1.Y + Half2.Y) - abs (Pos1.Y - Pos2.Y);

   begin
      return Float'Min (Overlap_X, Overlap_Y);
   end Penetration_Depth;


   ------------------------------------------------------------
   -- Resolve_Collision
   -- Helper: Resolve an AABB collision.
   --
   -- Separation rules (to keep static entities in place):
   --
   --   E1 is the "mover" (ball, or the moving entity in a
   --   non-ball pair).  E2 is the "static" entity (wall,
   --   brick, or paddle).
   --
   --   If only E1 has motion  -> E1 absorbs 100% of overlap,
   --                             E2 does not move.
   --   If only E2 has motion  -> E2 absorbs 100% of overlap,
   --                             E1 does not move.
   --   If both have motion    -> split 50/50 as before.
   --   If neither has motion  -> no positional change.
   --
   -- This prevents the paddle from drifting downward when hit
   -- by the ball, and prevents walls from being pushed off
   -- screen when the paddle slides into them.
   --
   -- Has_M1 / Has_M2 still control whether velocity is changed,
   -- independently of the positional separation above.
   ------------------------------------------------------------
   procedure Resolve_Collision
     (T1 : in out Transform_Component; C1 : Collider_Component;
      T2 : in out Transform_Component; C2 : Collider_Component;
      Has_M1 : Boolean; M1 : in out Motion_Component;
      Has_M2 : Boolean; M2 : in out Motion_Component) is

      Overlap_X : constant Float :=
         (C1.Bounding_Box.Half_Size.X + C2.Bounding_Box.Half_Size.X) -
            abs (T1.Position.X - T2.Position.X);

      Overlap_Y : constant Float :=
         (C1.Bounding_Box.Half_Size.Y + C2.Bounding_Box.Half_Size.Y) -
            abs (T1.Position.Y - T2.Position.Y);

      -- Separation fractions.
      -- If only one entity moves, it absorbs the full overlap (1.0 / 0.0).
      -- If both move, they share it equally (0.5 / 0.5).
      -- If neither moves, both fractions are 0.0.
      -- This is what keeps the paddle and walls in place during collisions.
      F1 : constant Float :=
         (if Has_M1 and Has_M2 then 0.5
          elsif Has_M1         then 1.0
          else                      0.0);

      F2 : constant Float :=
         (if Has_M1 and Has_M2 then 0.5
          elsif Has_M2         then 1.0
          else                      0.0);

   begin

      if Overlap_X < Overlap_Y then

         -- Separate along X axis
         if T1.Position.X < T2.Position.X then
            T1.Position.X := T1.Position.X - Overlap_X * F1;
            T2.Position.X := T2.Position.X + Overlap_X * F2;
         else
            T1.Position.X := T1.Position.X + Overlap_X * F1;
            T2.Position.X := T2.Position.X - Overlap_X * F2;
         end if;

         if Has_M1 then M1.Linear_Velocity.X := -M1.Linear_Velocity.X; end if;
         if Has_M2 then M2.Linear_Velocity.X := -M2.Linear_Velocity.X; end if;

      else

         -- Separate along Y axis
         if T1.Position.Y < T2.Position.Y then
            T1.Position.Y := T1.Position.Y - Overlap_Y * F1;
            T2.Position.Y := T2.Position.Y + Overlap_Y * F2;
         else
            T1.Position.Y := T1.Position.Y + Overlap_Y * F1;
            T2.Position.Y := T2.Position.Y - Overlap_Y * F2;
         end if;

         if Has_M1 then M1.Linear_Velocity.Y := -M1.Linear_Velocity.Y; end if;
         if Has_M2 then M2.Linear_Velocity.Y := -M2.Linear_Velocity.Y; end if;

      end if;

   end Resolve_Collision;


   ------------------------------------------------------------
   -- Resolve_Ball_Paddle
   -- Helper: Resolve a ball-vs-paddle collision.
   --
   -- The paddle is a flat horizontal surface.  Using the general
   -- axis-of-least-penetration logic causes the ball to stick at
   -- the paddle corners: when Overlap_X and Overlap_Y are nearly
   -- equal (corner graze) the chosen axis flips between frames,
   -- trapping the ball in an oscillation until the player shakes
   -- it loose.
   --
   -- Fix: always separate on Y only, pushing the ball straight up
   -- away from the top surface of the paddle regardless of the X
   -- overlap.  This is physically correct for a flat horizontal
   -- platform and eliminates corner sticking entirely.
   --
   -- The ball absorbs 100% of the Y separation; the paddle does not
   -- move (consistent with the one-sided separation rules).
   -- The ball's Y velocity is forced upward (negated if positive).
   -- The ball's X velocity is left untouched here; Ball_Physics_System
   -- will replace the full direction via zone-based deflection when
   -- it sees Hit_Paddle = True.
   --
   -- Parameters:
   --   T_Ball      : ball Transform (position updated in place)
   --   Half_Ball_Y : ball AABB half-size on Y axis
   --   C_Pad       : paddle Collider (half-sizes read, not modified)
   --   T_Pad       : paddle Transform (position read, not modified)
   --   M_Ball      : ball Motion (velocity updated in place)
   ------------------------------------------------------------
   procedure Resolve_Ball_Paddle
     (T_Ball      : in out Transform_Component;
      Half_Ball_Y :        Float;
      C_Pad       :        Collider_Component;
      T_Pad       :        Transform_Component;
      M_Ball      : in out Motion_Component) is

      -- True Y penetration depth
      Overlap_Y : constant Float :=
         (Half_Ball_Y + C_Pad.Bounding_Box.Half_Size.Y) -
            abs (T_Ball.Position.Y - T_Pad.Position.Y);

      -- Safety: ensure we push at least 1 pixel even on a zero-overlap frame
      Push : constant Float := Float'Max (Overlap_Y, 1.0);

   begin
      -- Push ball away from the paddle surface (ball is above paddle
      -- centre when hitting the top face, so T_Ball.Y < T_Pad.Y)
      if T_Ball.Position.Y <= T_Pad.Position.Y then
         T_Ball.Position.Y := T_Ball.Position.Y - Push;
      else
         T_Ball.Position.Y := T_Ball.Position.Y + Push;
      end if;

      -- Reflect Y velocity upward regardless of corner geometry.
      -- Only flip if the ball is currently moving toward the paddle
      -- (positive Y = downward in screen space) to avoid double-flipping
      -- on consecutive frames when the ball is already moving away.
      if M_Ball.Linear_Velocity.Y > 0.0 then
         M_Ball.Linear_Velocity.Y := -M_Ball.Linear_Velocity.Y;
      end if;

   end Resolve_Ball_Paddle;


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
         end if;
      end if;

   end Apply_Brick_Damage;


   ------------------------------------------------------------
   -- Resolve_With_Motion
   -- Helper: Resolve motion for a colliding pair.
   -- Pulls each entity's Motion out of the store individually
   -- to avoid conditional renames (which Ada does not permit).
   --
   -- Note: Has_M1 / Has_M2 are passed into Resolve_Collision,
   -- which uses them both to determine separation fractions
   -- (keeping static entities in place) and to gate velocity
   -- reflection.  See Resolve_Collision for details.
   ------------------------------------------------------------
   procedure Resolve_With_Motion
     (S  : in out Store.Store;
      E1 : Entity_ID; T1 : in out Transform_Component; C1 : Collider_Component;
      E2 : Entity_ID; T2 : in out Transform_Component; C2 : Collider_Component) is

      Has_M1 : constant Boolean := S.Has_Component (E1, Motion_Component'Tag);
      Has_M2 : constant Boolean := S.Has_Component (E2, Motion_Component'Tag);

      Dummy_M1 : Motion_Component;
      Dummy_M2 : Motion_Component;

   begin

      if Has_M1 and Has_M2 then
         declare
            M1 : Motion_Component renames S.Motion.Data (S.Motion.Lookup (E1));
            M2 : Motion_Component renames S.Motion.Data (S.Motion.Lookup (E2));
         begin
            Resolve_Collision (T1, C1, T2, C2, True, M1, True, M2);
         end;

      elsif Has_M1 and not Has_M2 then
         declare
            M1 : Motion_Component renames S.Motion.Data (S.Motion.Lookup (E1));
         begin
            Resolve_Collision (T1, C1, T2, C2, True, M1, False, Dummy_M2);
         end;

      elsif not Has_M1 and Has_M2 then
         declare
            M2 : Motion_Component renames S.Motion.Data (S.Motion.Lookup (E2));
         begin
            Resolve_Collision (T1, C1, T2, C2, False, Dummy_M1, True, M2);
         end;

      else
         Resolve_Collision (T1, C1, T2, C2, False, Dummy_M1, False, Dummy_M2);
      end if;

   end Resolve_With_Motion;


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

      -- ================================================================
      -- Pass 1: Ball collisions — resolve deepest non-dying hit only
      -- ================================================================
      for I in Entities'Range loop
         declare
            E_Ball : constant Entity_ID := Entities (I);
         begin

            if not S.Has_Component (E_Ball, Ball_Component'Tag) then
               goto Next_Ball_Candidate;
            end if;

            declare
               Index_CB   : constant Natural := S.Collider.Lookup (E_Ball);
               C_Ball     : Collider_Component renames S.Collider.Data (Index_CB);
               Index_TB   : constant Natural := S.Transform.Lookup (E_Ball);
               T_Ball     : Transform_Component renames S.Transform.Data (Index_TB);
               Index_Ball : constant Natural := S.Ball.Lookup (E_Ball);
               B          : Ball_Component renames S.Ball.Data (Index_Ball);

               Best_Depth     : Float   := -1.0;
               Best_J         : Integer := -1;
               Best_Is_Brick  : Boolean := False;
               Best_Is_Paddle : Boolean := False;

            begin

               for J in Entities'Range loop

                  if J = I then
                     goto Next_J;
                  end if;

                  declare
                     E_Other  : constant Entity_ID := Entities (J);
                     Index_CO : constant Natural   := S.Collider.Lookup (E_Other);
                     C_Other  : Collider_Component renames S.Collider.Data (Index_CO);
                     Index_TO : constant Natural   := S.Transform.Lookup (E_Other);
                     T_Other  : Transform_Component renames S.Transform.Data (Index_TO);
                  begin

                     -- Exclude dying bricks from collision candidates.
                     -- Once a brick's health has reached zero and Is_Dying is
                     -- True, the ball should pass through it cleanly rather
                     -- than bouncing a second time against a brick that is
                     -- already marked for destruction.
                     if S.Has_Component (E_Other, Brick_Component'Tag) then
                        declare
                           Idx_B : constant Natural := S.Brick.Lookup (E_Other);
                        begin
                           if S.Brick.Data (Idx_B).Is_Dying then
                              goto Next_J;
                           end if;
                        end;
                     end if;

                     if Entities_Can_Collide (C_Ball, C_Other)
                        and then C_Ball.Collider_Form  = Solid
                        and then C_Other.Collider_Form = Solid
                        and then AABB_Overlap
                           (T_Ball.Position,  C_Ball.Bounding_Box.Half_Size,
                            T_Other.Position, C_Other.Bounding_Box.Half_Size)
                     then
                        declare
                           Depth : constant Float := Penetration_Depth
                              (T_Ball.Position,  C_Ball.Bounding_Box.Half_Size,
                               T_Other.Position, C_Other.Bounding_Box.Half_Size);
                        begin
                           if Depth > Best_Depth then
                              Best_Depth     := Depth;
                              Best_J         := J;
                              Best_Is_Brick  :=
                                 S.Has_Component (E_Other, Brick_Component'Tag);
                              Best_Is_Paddle :=
                                 S.Has_Component (E_Other, Paddle_Component'Tag);
                           end if;
                        end;
                     end if;

                  end;

                  <<Next_J>>
               end loop;

               -- Resolve the single deepest hit (if any)
               if Best_J >= 0 then
                  declare
                     E_Best   : constant Entity_ID := Entities (Best_J);
                     Index_CO : constant Natural   := S.Collider.Lookup (E_Best);
                     C_Best   : Collider_Component renames S.Collider.Data (Index_CO);
                     Index_TO : constant Natural   := S.Transform.Lookup (E_Best);
                     T_Best   : Transform_Component renames S.Transform.Data (Index_TO);
                  begin

                     if Best_Is_Paddle then
                        -- Use Y-only paddle resolver to prevent corner sticking.
                        -- Resolve_Ball_Paddle always pushes the ball upward off
                        -- the paddle surface regardless of which overlap axis is
                        -- smaller, eliminating the oscillation that occurs when
                        -- Overlap_X and Overlap_Y are nearly equal at corners.
                        declare
                           M_Ball : Motion_Component renames
                              S.Motion.Data (S.Motion.Lookup (E_Ball));
                        begin
                           Resolve_Ball_Paddle
                              (T_Ball      => T_Ball,
                               Half_Ball_Y => C_Ball.Bounding_Box.Half_Size.Y,
                               C_Pad       => C_Best,
                               T_Pad       => T_Best,
                               M_Ball      => M_Ball);
                        end;
                        B.Hit_Paddle := True;
                        Audio.Play_Audio("sfx/ball_hit.wav", False);
                     else
                        -- General resolver for bricks and walls
                        Resolve_With_Motion
                           (S,
                            E_Ball, T_Ball, C_Ball,
                            E_Best, T_Best, C_Best);

                        if Best_Is_Brick then
                           Apply_Brick_Damage (S, E_Best);
                           Update_Score (S.Brick.Data (S.Brick.Lookup (E_Best)).Points);
                           Audio.Play_Audio("sfx/ball_hit.wav", False);
                        end if;

                     end if;

                  end;
               end if;

            end;

         end;
         <<Next_Ball_Candidate>>
      end loop;

      -- ================================================================
      -- Pass 2: Non-ball solid pairs (e.g. paddle vs wall)
      -- Resolve_With_Motion gives the full displacement to whichever
      -- entity has a Motion component, so walls (no Motion) never move
      -- and the paddle only moves horizontally as driven by its own
      -- control system.
      -- ================================================================
      for I in Entities'Range loop
         for J in I + 1 .. Entities'Last loop
            declare
               E1 : constant Entity_ID := Entities (I);
               E2 : constant Entity_ID := Entities (J);
            begin

               if S.Has_Component (E1, Ball_Component'Tag) or
                  S.Has_Component (E2, Ball_Component'Tag)
               then
                  goto Next_Non_Ball_Pair;
               end if;

               declare
                  Index_C1 : constant Natural := S.Collider.Lookup (E1);
                  C1 : Collider_Component renames S.Collider.Data (Index_C1);
                  Index_T1 : constant Natural := S.Transform.Lookup (E1);
                  T1 : Transform_Component renames S.Transform.Data (Index_T1);

                  Index_C2 : constant Natural := S.Collider.Lookup (E2);
                  C2 : Collider_Component renames S.Collider.Data (Index_C2);
                  Index_T2 : constant Natural := S.Transform.Lookup (E2);
                  T2 : Transform_Component renames S.Transform.Data (Index_T2);
               begin
                  if Entities_Can_Collide (C1, C2)
                     and then C1.Collider_Form = Solid
                     and then C2.Collider_Form = Solid
                     and then AABB_Overlap
                        (T1.Position, C1.Bounding_Box.Half_Size,
                         T2.Position, C2.Bounding_Box.Half_Size)
                  then
                     Resolve_With_Motion (S, E1, T1, C1, E2, T2, C2);
                  end if;
               end;

            end;
            <<Next_Non_Ball_Pair>>
         end loop;
      end loop;

   end Update;

end ECS.Systems_Collision;