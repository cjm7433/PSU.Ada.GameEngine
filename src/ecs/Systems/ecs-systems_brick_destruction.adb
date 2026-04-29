-- ecs-systems_brick_destruction.adb
--
-- Implementation of Brick Destruction System
-- Handles brick death timers and entity removal.
--
-- IMPORTANT - collect-then-destroy pattern:
--    Ada.Containers vectors raise Program_Error ("tamper with cursors")
--    if you modify the vector while a reference into it is live, or
--    while iterating over a derived structure.
--    (This error was raised previously but routed out)
--    TODO: Maybe delete this section
--
--   The fix is two passes:
--     Pass 1: iterate the entity list, update timers, collect IDs
--             whose timers have expired into a local To_Destroy array.
--     Pass 2: after the loop finishes (all renames released), destroy
--             each collected entity.
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Brick;          use ECS.Components.Brick;
with ECS.Systems_Collision;
with ECS.Components.Transform;      use ECS.Components.Transform;
with ECS.Components.Motion;         use ECS.Components.Motion;
with ECS.Components.Collider;       use ECS.Components.Collider;
with ECS.Components.Render;         use ECS.Components.Render;
with ECS.Components.Ball;           use ECS.Components.Ball;
with ECS.Components.Audio;          use ECS.Components.Audio;

package body ECS.Systems_Brick_Destruction is

   --------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Brick
   --------------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Brick_Destruction_System)
      return ECS.Components.Component_Tag_Array
   is
   begin
      return (0 => ECS.Components.Brick.Brick_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Update executes brick destruction logic
   -- Update: tick death timers, then destroy expired bricks
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Brick_Destruction_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;
   
   begin
      
      -- Get entities with the required components
      Entities := S.Get_Entities_With (Self.Components_Needed);

      if Entities = null then
         return;
      end if;

      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- PASS 1: Update timers and collect expired entity IDs.
      --
      -- We declare To_Destroy with the same length as Entities
      --    (worst case: every brick expires this frame) and track
      --    how many are actually filled with Destroy_Count.
      --
      -- All renames of vector elements are released when this
      --    declare block exits, which is before Pass 2 runs.
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      declare
         To_Destroy    : Entity_ID_Array (Entities'Range);
         Destroy_Count : Natural := 0;
      
      begin

         for I in Entities'Range loop
            
            declare
               E : constant Entity_ID := Entities (I);
               Idx : constant Natural := S.Brick.Lookup (E);
               B : Brick_Component renames S.Brick.Data (Idx);
            
            begin
               if B.Is_Dying then
                  B.Death_Timer := B.Death_Timer - DT;

                  if B.Death_Timer <= 0.0 then
                     ECS.Systems_Collision.Update_Score (B.Points);

                     -- Spawn extra ball
                     if B.Brick_Kind = Special and S.Has_Component (E, ECS.Components.Transform.Transform_Component'Tag) and not Is_Equal_Approximate (B.Spawn_Angle.Length_Squared, 0.0) then
                        Index_T : constant Transform_Table.Index := S.Transform.Lookup(E);
                        T : constant Transform_Component := S.Transform.Data(Index_T);

                        Ball_E : Entity_ID := Create_Entity (S);

                        Add_Component (S, Ball_E, Transform_Component'Tag);
                        Add_Component (S, Ball_E, Motion_Component'Tag);
                        Add_Component (S, Ball_E, Collider_Component'Tag);
                        Add_Component (S, Ball_E, Render_Component'Tag);
                        Add_Component (S, Ball_E, Ball_Component'Tag);
                        Add_Component (S, Ball_E, Audio_Component'Tag);

                        S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := T.Position;

                        S.Collider.Data (S.Collider.Lookup (Ball_E)).Bounding_Box :=
                           (Center    => T.Position,
                           Half_Size => (X => Float (3),
                                          Y => Float (3)));
                        S.Collider.Data (S.Collider.Lookup (Ball_E)).Layer :=
                           Layer_Ball;
                        S.Collider.Data (S.Collider.Lookup (Ball_E)).Mask :=
                           (Layer_Paddle, Layer_Brick, Layer_Wall, Layer_None);
                        S.Collider.Data (S.Collider.Lookup (Ball_E)).Collider_Form :=
                           Solid;
                        S.Collider.Data (S.Collider.Lookup (Ball_E)).Name :=
                           "BALL";

                        S.Render.Data (S.Render.Lookup (Ball_E)).Shape   := Circle;
                        S.Render.Data (S.Render.Lookup (Ball_E)).Tint    :=
                           (R => 0.8, G => 0.8, B => 0.8, A => 1.0);   -- Light Gray
                        S.Render.Data (S.Render.Lookup (Ball_E)).Layer   := 1;
                        S.Render.Data (S.Render.Lookup (Ball_E)).Visible := True;

                        S.Audio.Data (S.Audio.Lookup (Ball_E)).File_Path := To_Unbounded_String("sfx/ball_hit.wav");
                        S.Audio.Data (S.Audio.Lookup (Ball_E)).Volume := 0.075;
                        S.Audio.Data (S.Audio.Lookup (Ball_E)).Playing := False;

                        S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached     := False;
                        S.Ball.Data (S.Ball.Lookup (Ball_E)).Attach_Offset_X := 0.0;
                        S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := -B.Spawn_Angle * S.Ball.Data (S.Ball.Lookup (Ball_E)).Base_Speed / 2.0;
                     end if;

                     To_Destroy (Destroy_Count) := E;
                     Destroy_Count := Destroy_Count + 1;
                  end if;
               end if;
            end;
            -- The renames 'B' is released here, before we ever
            -- call Destroy_Entity.
         end loop;

         --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         -- PASS 2: Destroy collected entities.
         --
         -- No renames or cursors into the vector are live here,
         --    so Destroy_Entity can safely modify it.
         --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for J in 0 .. Destroy_Count - 1 loop
            Destroy_Entity (S, To_Destroy (J));
         end loop;

      end; -- To_Destroy declared block

   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Brick_Destruction_System) return String is
   begin
      return "Brick Destruction";
   end Name;

end ECS.Systems_Brick_Destruction;
