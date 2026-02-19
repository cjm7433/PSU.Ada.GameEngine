-- ecs-systems_collision.adb
--
-- Implementation of Collision System.
-- AABB collision detection and resolution using enum-based layer filtering.

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with ECS.Components_Collision;      use ECS.Components_Collision;
with ECS.Components_Ball;           use ECS.Components_Ball;
with ECS.Components_Brick;          use ECS.Components_Brick;
with Math.Linear_Algebra;           use Math.Linear_Algebra;

package body ECS.Systems_Collision is

   ------------------------------------------------------------
   -- Layer_In_Mask
   -- Helper: Check if a layer appears anywhere in a mask.
   ------------------------------------------------------------
   function Layer_In_Mask
     (Layer : Collision_Layer;
      Mask  : Collision_Mask) return Boolean is 
      
      begin

      -- Layer_None is a special case that means "no layers" and should never 
      --    collide with anything, even if it's technically in the mask.
      if Layer = Layer_None then
         return False;
      end if;
      
      -- Check each layer in the mask for a match.
      -- We have to do this manually since Ada doesn't allow 
      --    iterating over enum values.
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
   -- This allows for one-way collisions (e.g. ball triggers on paddle but not vice versa).
   function Entities_Can_Collide (C1, C2 : Collision) return Boolean is
   
   begin
      -- TODO: Do we need to worry about Layer_None here?
      -- If either entity has Layer_None, it should not collide with anything ...

      return Layer_In_Mask (C1.Layer, C2.Mask) or     -- Entity 1's layer is in Entity 2's mask
             Layer_In_Mask (C2.Layer, C1.Mask);       -- Entity 2's layer is in Entity 1's mask
   
   end Entities_Can_Collide;


   ------------------------------------------------------------
   -- AABB_Overlap
   -- Helper: Check if two AABBs overlap.
   -- TODO: Something similar already in math-physics library
   ------------------------------------------------------------
   function AABB_Overlap
     (Pos1 : Vector2; Half1 : Vector2;
      Pos2 : Vector2; Half2 : Vector2) return Boolean is
      
      -- Two boxes do not overlap if they are separated on either the X or Y axis.
      -- If boxes aren't separated on either axis, they overlap.

      Separated_X : constant Boolean :=               -- Separated on X if right edge of Box 1 is left of left edge of Box 2, or left edge of Box 1 is right of right edge of Box 2
         (Pos1.X + Half1.X < Pos2.X - Half2.X) or
         (Pos1.X - Half1.X > Pos2.X + Half2.X);
      
      
      Separated_Y : constant Boolean :=               -- Separated on Y if bottom edge of Box 1 is above top edge of Box 2, or top edge of Box 1 is below bottom edge of Box 2
         (Pos1.Y + Half1.Y < Pos2.Y - Half2.Y) or
         (Pos1.Y - Half1.Y > Pos2.Y + Half2.Y);
   
   begin
      return not (Separated_X or Separated_Y);
   end AABB_Overlap;


   ------------------------------------------------------------
   -- Resolve_Collision
   -- Helper: Resolve an AABB collision.
   -- Separates entities along axis of least penetration and
   -- reflects velocity. 
   -- M1/M2 are always valid references.
   -- Has_M1/Has_M2 control whether velocity is actually changed.
   ------------------------------------------------------------
   procedure Resolve_Collision
     (T1 : in out Transform; C1 : Collision;
      T2 : in out Transform; C2 : Collision;
      Has_M1 : Boolean; M1 : in out Motion;
      Has_M2 : Boolean; M2 : in out Motion) is

      -- Calculate overlap on each axis (positive means penetration)
      Overlap_X : constant Float :=
         (C1.Half_Width + C2.Half_Width) - abs (T1.Position.X - T2.Position.X);
      
      Overlap_Y : constant Float :=
         (C1.Half_Height + C2.Half_Height) - abs (T1.Position.Y - T2.Position.Y);
   
   begin
      
      -- Separate along axis of least penetration
      if Overlap_X < Overlap_Y then
         
         -- To avoid "sticking," we split the overlap in half and move each
         -- entity by that amount. The one on the left moves left, the one on the right moves right.
         
         if T1.Position.X < T2.Position.X then

            T1.Position.X := T1.Position.X - Overlap_X / 2.0;
            T2.Position.X := T2.Position.X + Overlap_X / 2.0;
         
         else

            T1.Position.X := T1.Position.X + Overlap_X / 2.0;
            T2.Position.X := T2.Position.X - Overlap_X / 2.0;

         end if;
         
         -- Reflect velocity on X axis (only if entity has Motion)
         if Has_M1 then M1.Linear_Velocity.X := -M1.Linear_Velocity.X; end if;
         
         if Has_M2 then M2.Linear_Velocity.X := -M2.Linear_Velocity.X; end if;
      
      else
         -- Same separation logic but on Y axis. The one on top moves up, the one on bottom moves down.
         if T1.Position.Y < T2.Position.Y then
            T1.Position.Y := T1.Position.Y - Overlap_Y / 2.0;
            T2.Position.Y := T2.Position.Y + Overlap_Y / 2.0;
      
         else
            T1.Position.Y := T1.Position.Y + Overlap_Y / 2.0;
            T2.Position.Y := T2.Position.Y - Overlap_Y / 2.0;
         end if;
      
         if Has_M1 then M1.Linear_Velocity.Y := -M1.Linear_Velocity.Y; end if;
         if Has_M2 then M2.Linear_Velocity.Y := -M2.Linear_Velocity.Y; end if;
      
      end if;

   end Resolve_Collision;


   ------------------------------------------------------------
   -- Apply_Brick_Damage
   -- Helper: Damage a brick when hit by ball.
   -- TODO: Should this be handled elsewhere?
   ------------------------------------------------------------
   procedure Apply_Brick_Damage (S : in out Store.Store; Brick_Entity : Entity_ID) is
      
      Index_B : constant Natural := S.Brick.Lookup (Brick_Entity);
      B : Brick renames S.Brick.Data (Index_B);
   
   begin
      
      -- Only apply damage if brick isn't already dying and has health left.
      if not B.Is_Dying and B.Health > 0 then
         B.Health := B.Health - 1;
         
         if B.Health = 0 then
            B.Is_Dying    := True;
            B.Death_Timer := 0.2;
         end if;
      
      end if;
   
   end Apply_Brick_Damage;


   ------------------------------------------------------------
   -- Resolve_With_Motion
   -- Helper: Resolve motion for a colliding pair.
   -- Pulls each entity's Motion out of the store individually
   -- to avoid conditional renames (which Ada does not permit).
   ------------------------------------------------------------
   procedure Resolve_With_Motion
     (S  : in out Store.Store;
      E1 : Entity_ID; T1 : in out Transform; C1 : Collision;
      E2 : Entity_ID; T2 : in out Transform; C2 : Collision) is

      -- Check for Motion component on each entity. We have to do this before renaming
      Has_M1 : constant Boolean := S.Has_Component (E1, Motion'Tag);
      Has_M2 : constant Boolean := S.Has_Component (E2, Motion'Tag);

      -- Dummy records stand in when entity has no Motion component.
      -- Resolve_Collision only writes to them when Has_M1/Has_M2 is True,
      -- so no incorrect state is ever saved.
      Dummy_M1 : Motion;
      Dummy_M2 : Motion;

   begin

      -- Each branch gives us a plain variable renames, which is always valid.
      if Has_M1 and Has_M2 then
         
         declare
            M1 : Motion renames S.Motion.Data (S.Motion.Lookup (E1));
            M2 : Motion renames S.Motion.Data (S.Motion.Lookup (E2));
         
         begin
            -- Both entities have Motion; resolve with full velocity updates
            Resolve_Collision (T1, C1, T2, C2, True, M1, True, M2);
         end;

      elsif Has_M1 and not Has_M2 then
         
         declare
            M1 : Motion renames S.Motion.Data (S.Motion.Lookup (E1));
         
         begin
            -- Entity 1 has Motion but Entity 2 doesn't; only update Entity 1's velocity
            Resolve_Collision (T1, C1, T2, C2, True, M1, False, Dummy_M2);
         end;

      elsif not Has_M1 and Has_M2 then
         
         declare
            M2 : Motion renames S.Motion.Data (S.Motion.Lookup (E2));
         
         begin
            -- Entity 2 has Motion but Entity 1 doesn't; only update Entity 2's velocity
            Resolve_Collision (T1, C1, T2, C2, False, Dummy_M1, True, M2);
         end;

      else
         -- Neither entity has Motion; no velocity to update   (probably impossible)
         Resolve_Collision (T1, C1, T2, C2, False, Dummy_M1, False, Dummy_M2);
      end if;
   
   end Resolve_With_Motion;


   ---------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Collision, Transform
   ---------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Collision_System)
      return ECS.Components.Component_Tag_Array is

   begin

      -- Collision needs Collision and Transform Components
      return (0 => ECS.Components_Collision.Collision'Tag,
              1 => ECS.Components_Transform.Transform'Tag);
   
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Update executes collision detection and resolution
   --    (Detect and resolve all collisions this frame.)
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Collision_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;     -- All entities with Collision + Transform
   
   begin
      
      -- Get entities with the required components
      Entities := S.Get_Entities_With (Self.Components_Needed);
      if Entities = null then
         return;
      end if;

      -- Check each unique pair once (I < J avoids A-vs-B and B-vs-A)
      for I in Entities'Range loop
         for J in I + 1 .. Entities'Last loop
            declare

               E1 : constant Entity_ID := Entities (I);
               E2 : constant Entity_ID := Entities (J);

               -- Who doesn't like a nickname?
               Index_C1 : constant Natural := S.Collision.Lookup (E1);
               C1 : Collision renames S.Collision.Data (Index_C1);
               
               Index_T1 : constant Natural := S.Transform.Lookup (E1);
               T1 : Transform renames S.Transform.Data (Index_T1);

               Index_C2 : constant Natural := S.Collision.Lookup (E2);
               C2 : Collision renames S.Collision.Data (Index_C2);
               
               Index_T2 : constant Natural := S.Transform.Lookup (E2);
               T2 : Transform renames S.Transform.Data (Index_T2);

            begin

               -- Short-circuit: only check for AABB overlap if layers/masks allow collision
               if Entities_Can_Collide (C1, C2) and then AABB_Overlap (          
                  T1.Position, (C1.Half_Width, C1.Half_Height),
                  T2.Position, (C2.Half_Width, C2.Half_Height))
               
               then
                  
                  -- Separate and bounce (triggers are detected but not resolved)
                  if not C1.Is_Trigger and not C2.Is_Trigger then
                     Resolve_With_Motion (S, E1, T1, C1, E2, T2, C2);
                  end if;

                  -- Apply brick damage when ball is one of the colliders
                  if S.Has_Component (E1, Ball'Tag) and
                     S.Has_Component (E2, Brick'Tag)
                  
                  then
                     Apply_Brick_Damage (S, E2);

                  elsif S.Has_Component (E2, Ball'Tag) and
                        S.Has_Component (E1, Brick'Tag)
                  then
                     Apply_Brick_Damage (S, E1);
                  end if;

               end if;
            end;
         end loop;
      end loop;

   end Update;

end ECS.Systems_Collision;
