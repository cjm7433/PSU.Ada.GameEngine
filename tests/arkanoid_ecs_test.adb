-- arkanoid_ecs_test.adb
--
-- Test suite for the Arkanoid-specific ECS components and systems.
--
-- Tests cover:
--   1. Collision component - layer/mask setup and filtering
--   2. Paddle component - default values and boundary logic
--   3. Ball component - speed clamping and attachment
--   4. Brick component - health, damage, death timer
--   5. Renderable component - defaults
--   6. Collision System - AABB overlap detection and resolution
--   7. Ball Physics System - speed clamping
--   8. Brick Destruction System - death timer and entity removal
--   9. Integration - ball hitting bricks

with Ada.Text_IO;                      use Ada.Text_IO;
with ECS.Entities;                     use ECS.Entities;
with ECS.Components;                   use ECS.Components;
with ECS.Components_Transform;         use ECS.Components_Transform;
with ECS.Components_Motion;            use ECS.Components_Motion;
with ECS.Components_Collision;         use ECS.Components_Collision;
with ECS.Components_Ball;              use ECS.Components_Ball;
with ECS.Components_Brick;             use ECS.Components_Brick;
with ECS.Components_Paddle;            use ECS.Components_Paddle;
with ECS.Components_Renderable;        use ECS.Components_Renderable;
with ECS.Store;                        use ECS.Store;
with ECS.Systems_Collision;            use ECS.Systems_Collision;
with ECS.Systems_Ball_Physics;         use ECS.Systems_Ball_Physics;
with ECS.Systems_Brick_Destruction;    use ECS.Systems_Brick_Destruction;
with Math.Linear_Algebra;              use Math.Linear_Algebra;

procedure Arkanoid_ECS_Test is

   ------------------------------------------------------------
   -- Test state
   ------------------------------------------------------------
   Total  : Natural := 0;     -- Test counters (run/passed/failed)
   Passed : Natural := 0;
   Failed : Natural := 0;


   ------------------------------------------------------------
   -- Helper: assert a condition and track pass/fail
   ------------------------------------------------------------
   procedure Assert (Condition : Boolean; Name : String) is
   
   begin
      
      Total := Total + 1;                 -- accumulate tests run
      
      if Condition then
         Put_Line ("[PASS] " & Name);
         Passed := Passed + 1;            -- accumulate tests passed
      
      else
         Put_Line ("[FAIL] " & Name);
         Failed := Failed + 1;            -- -- accumulate tests failed
      end if;
   
   end Assert;


   ------------------------------------------------------------
   -- Test fixture: Store instance (reset before each section)
   ------------------------------------------------------------
   S : Store;


begin

   Put_Line ("========================================");
   Put_Line ("Arkanoid ECS Component & System Tests");
   Put_Line ("========================================");
   New_Line;


   ------------------------------------------------------------
   -- SECTION 1: Component defaults
   -- Verifies that Add_Component initializes each component
   -- with sensible default values as defined in ecs-store.adb.
   ------------------------------------------------------------
   Put_Line ("--- Section 1: Component Defaults ---");
   
   Initialize (S);
   
   declare
      E : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Add Collision Component (defaults) to Entity E
      Add_Component (S, E, Collision'Tag);
   
      declare
         -- We have to cast the generic Component C to a Collision to access its fields
         C : ECS.Components_Collision.Collision :=
            ECS.Components_Collision.Collision(Get_Component (S, E, Collision'Tag));
   
      begin
         Assert (C.Half_Width = 0.5,   "Collision default Half_Width");
         Assert (C.Half_Height = 0.5,  "Collision default Half_Height");
         Assert (C.Layer = Layer_None, "Collision default Layer is Layer_None");
         Assert (not C.Is_Trigger,     "Collision default Is_Trigger is False");
      end;

      -- Add Paddle Component(defaults) to Entity E
      Add_Component (S, E, Paddle'Tag);
   
      declare
         -- We have to cast the generic Component P to a Paddle to access its fields
         P : ECS.Components_Paddle.Paddle :=
            ECS.Components_Paddle.Paddle(Get_Component (S, E, Paddle'Tag));
   
      begin
         Assert (P.Move_Speed = 500.0, "Paddle default Move_Speed");
         Assert (not P.Move_Left,      "Paddle default Move_Left is False");
         Assert (not P.Move_Right,     "Paddle default Move_Right is False");
      end;

      -- Add Ball Component (defaults) to Entity E
      Add_Component (S, E, Ball'Tag);
      
      declare
         -- We have to cast the generic Component B to a Ball to access its fields
         B : ECS.Components_Ball.Ball :=
            ECS.Components_Ball.Ball(Get_Component (S, E, Ball'Tag));
      
      begin
         Assert (B.Min_Speed = 200.0,  "Ball default Min_Speed");
         Assert (B.Max_Speed = 800.0,  "Ball default Max_Speed");
         Assert (B.Is_Attached,        "Ball default Is_Attached is True");
      end;

      -- Add Brick Component (defaults) to Entity E
      Add_Component (S, E, Brick'Tag);
      
      declare
         -- We have to cast the generic Component K to a Brick to access its fields
         K : ECS.Components_Brick.Brick :=
            ECS.Components_Brick.Brick(Get_Component (S, E, Brick'Tag));
      
      begin
         Assert (K.Health = 1,         "Brick default Health");
         Assert (K.Points = 10,        "Brick default Points");
         Assert (not K.Is_Dying,       "Brick default Is_Dying is False");
         Assert (K.Brick_Kind = Normal, "Brick default Kind is Normal");
      end;

      -- Add Ball Component (defaults) to Entity E
      Add_Component (S, E, Renderable'Tag);

      declare
         -- We have to cast the generic Component R to a Renderable to access its fields
         R : ECS.Components_Renderable.Renderable :=
            ECS.Components_Renderable.Renderable (Get_Component (S, E, Renderable'Tag));
      
      begin
         Assert (R.Visible,            "Renderable default Visible is True");
         Assert (R.Shape = Rectangle,  "Renderable default Shape is Rectangle");
         Assert (R.Layer = 0,          "Renderable default Layer is 0");
      end;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SECTION 2: Collision layer/mask filtering
   -- Verifies that the enum-based layer system correctly
   -- determines which entity pairs are allowed to collide.
   -- This is based on Collision Layer with Collision Mask
   ------------------------------------------------------------
   Put_Line ("--- Section 2: Collision Layer/Mask Filtering ---");
   
   Initialize (S);
   
   declare
      Ball_E   : constant Entity_ID := Create_Entity (S);
      Paddle_E : constant Entity_ID := Create_Entity (S);
      Wall_E   : constant Entity_ID := Create_Entity (S);
      Brick_E  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Set up ball: layer=Layer_Ball, collides with Paddle/Brick/Wall
      Add_Component (S, Ball_E, Collision'Tag);
   
      declare
         -- Get index of Ball's Collision component
         Idx : constant Natural := S.Collision.Lookup (Ball_E);      
         C : ECS.Components_Collision.Collision renames S.Collision.Data (Idx);
   
      begin
         C.Layer := Layer_Ball;
         C.Mask  := (Layer_Paddle, Layer_Brick, Layer_Wall, Layer_None);
         C.Half_Width  := 8.0;
         C.Half_Height := 8.0;
      end;

      -- Set up paddle: layer=Layer_Paddle, collides with Ball
      Add_Component (S, Paddle_E, Collision'Tag);
      
      declare
         -- Get index of Paddle's Collision component
         Idx : constant Natural := S.Collision.Lookup (Paddle_E);
         C : ECS.Components_Collision.Collision renames S.Collision.Data (Idx);
      
      begin
         C.Layer := Layer_Paddle;
         C.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width  := 50.0;
         C.Half_Height := 10.0;
      end;

      -- Set up wall: layer=Layer_Wall, collides with Ball
      Add_Component (S, Wall_E, Collision'Tag);
      
      declare
         -- Get index of Wall's's Collision component
         Idx : constant Natural := S.Collision.Lookup (Wall_E);
         C : ECS.Components_Collision.Collision renames S.Collision.Data (Idx);
      
      begin
         C.Layer := Layer_Wall;
         C.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
      end;

      -- Set up brick: layer=Layer_Brick, collides with Ball
      Add_Component (S, Brick_E, Collision'Tag);
      
      declare
         -- Get index of Brick's Collision component
         Idx : constant Natural := S.Collision.Lookup (Brick_E);
         C : ECS.Components_Collision.Collision renames S.Collision.Data (Idx);
      
      begin
         C.Layer := Layer_Brick;
         C.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width  := 30.0;
         C.Half_Height := 15.0;
      end;

      -- TODO: Maybe should have renamed these different names and not all just C think emoji

      -- Verify layer assignments
      Assert (S.Collision.Data (S.Collision.Lookup (Ball_E)).Layer = Layer_Ball,
              "Ball entity has Layer_Ball");
      Assert (S.Collision.Data (S.Collision.Lookup (Paddle_E)).Layer = Layer_Paddle,
              "Paddle entity has Layer_Paddle");
      Assert (S.Collision.Data (S.Collision.Lookup (Wall_E)).Layer = Layer_Wall,
              "Wall entity has Layer_Wall");
      Assert (S.Collision.Data (S.Collision.Lookup (Brick_E)).Layer = Layer_Brick,
              "Brick entity has Layer_Brick");
   end;
   New_Line;


   ------------------------------------------------------------
   -- SECTION 3: Collision System - AABB overlap and bounce
   -- Places two entities with overlapping boxes and runs the
   -- collision system. Verifies separation and velocity bounce.
   ------------------------------------------------------------
   Put_Line ("--- Section 3: Collision System AABB ---");
   
   Initialize (S);
   
   declare
      Sys       : Collision_System;
      Ball_E    : constant Entity_ID := Create_Entity (S);
      Paddle_E  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Ball: moving downward, positioned overlapping paddle
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      Add_Component (S, Ball_E, Ball'Tag);

      -- Paddle: stationary
      Add_Component (S, Paddle_E, Transform'Tag);
      Add_Component (S, Paddle_E, Collision'Tag);

      -- Place ball overlapping paddle (ball at Y=100, paddle at Y=105)
      -- with half heights of 8 and 10, overlap = 18 - 5 = 13
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position   := (400.0, 100.0);
      S.Transform.Data (S.Transform.Lookup (Paddle_E)).Position := (400.0, 105.0);

      -- Ball moving down
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, -300.0);

      -- Set collision layers
      declare
         -- Ball collision
         BC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));

         -- Paddle collision
         PC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Paddle_E));
      
      begin
         BC.Layer := Layer_Ball;
         BC.Mask  := (Layer_Paddle, Layer_None, Layer_None, Layer_None);
         BC.Half_Width  := 8.0;
         BC.Half_Height := 8.0;

         PC.Layer := Layer_Paddle;
         PC.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         PC.Half_Width  := 50.0;
         PC.Half_Height := 10.0;
      end;

      -- Run collision system
      Sys.Update (S, 0.016);     -- 60fps

      -- Ball Y velocity should now be positive (bounced upward)
      Assert (S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity.Y > 0.0,
              "Ball bounced upward after hitting paddle");

      -- Entities should be separated (no longer overlapping)
      declare
         Ball_Y   : constant Float :=
            S.Transform.Data (S.Transform.Lookup (Ball_E)).Position.Y;
         Paddle_Y : constant Float :=
            S.Transform.Data (S.Transform.Lookup (Paddle_E)).Position.Y;
      
      begin
         Assert (abs (Ball_Y - Paddle_Y) >= 17.0,
                 "Ball and paddle separated after collision");
      end;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SECTION 4: Brick damage and destruction
   -- Ball collides with brick, brick takes damage.
   -- After Health reaches 0, Is_Dying becomes True.
   -- Brick Destruction System removes it after timer expires.
   ------------------------------------------------------------
   Put_Line ("--- Section 4: Brick Damage and Destruction ---");
   
   Initialize (S);
   
   declare
      Col_Sys  : Collision_System;
      Dest_Sys : Brick_Destruction_System;

      -- We create these entities as constants so we can reference them in the assertions later.
      Ball_E   : constant Entity_ID := Create_Entity (S);      -- We need a ball to collide with the brick and trigger damage.
      Brick_E  : constant Entity_ID := Create_Entity (S);      --    and we need a brick
   
   begin
      -- Set up ball
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      Add_Component (S, Ball_E, Ball'Tag);

      -- Set up brick
      Add_Component (S, Brick_E, Transform'Tag);
      Add_Component (S, Brick_E, Collision'Tag);
      Add_Component (S, Brick_E, Brick'Tag);

      -- Overlap positions
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position  := (200.0, 200.0);
      S.Transform.Data (S.Transform.Lookup (Brick_E)).Position := (200.0, 205.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, -300.0);

      -- Collision layers
      declare
         -- Ball collision
         BC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));

         -- Brick Collision
         KC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Brick_E));
      
      begin
         BC.Layer := Layer_Ball;
         BC.Mask  := (Layer_Brick, Layer_None, Layer_None, Layer_None);
         BC.Half_Width  := 8.0;
         BC.Half_Height := 8.0;
         KC.Layer := Layer_Brick;
         KC.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         KC.Half_Width  := 30.0;
         KC.Half_Height := 15.0;
      end;

      -- Run collision system - should damage the brick
      Col_Sys.Update (S, 0.016);       -- 60fps (should maybe be a global)

      -- Brick health should be 0 and Is_Dying should be True
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      
      begin
         Assert (B.Health = 0,    "Brick health reduced to 0");
         Assert (B.Is_Dying,      "Brick Is_Dying is True");
         Assert (B.Death_Timer > 0.0, "Brick death timer started");
      end;

      -- Run destruction system with large DT to expire the timer
      Dest_Sys.Update (S, 1.0);

      -- Brick entity should now be destroyed
      Assert (not Has_Entity (S, Brick_E), "Brick entity destroyed after timer");
   end;
   New_Line;


   ------------------------------------------------------------
   -- SECTION 5: Strong brick requires multiple hits
   ------------------------------------------------------------
   Put_Line ("--- Section 5: Strong Brick (2 hits) ---");
   
   Initialize (S);
   
   declare
      Col_Sys : Collision_System;
      Ball_E  : constant Entity_ID := Create_Entity (S);
      Brick_E : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      Add_Component (S, Ball_E, Ball'Tag);
      Add_Component (S, Brick_E, Transform'Tag);
      Add_Component (S, Brick_E, Collision'Tag);
      Add_Component (S, Brick_E, Brick'Tag);

      -- Make it a strong brick (2 hits)
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      
      begin
         B.Health     := 2;
         B.Max_Health := 2;
         B.Brick_Kind := Strong;
      end;

      -- First hit
      -- Position ball to overlap brick and move downward
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position  := (300.0, 300.0);
      
      -- We have to position the brick slightly below the ball so that they actually overlap given their half heights 
      --  (ball half height = 8, brick half height = 15, so we need at least 23 units of vertical distance for them to just touch. 
      --  We want them to overlap, so we can position the brick at 305).
      S.Transform.Data (S.Transform.Lookup (Brick_E)).Position := (300.0, 305.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, -300.0);
      
      
      declare
         -- Ball Collision
         BC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
         -- Brick Collision
         KC : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Brick_E));
      
      -- Set collision layers and boxes
      begin
         BC.Layer := Layer_Ball;
         BC.Mask  := (Layer_Brick, Layer_None, Layer_None, Layer_None);
         BC.Half_Width := 8.0; BC.Half_Height := 8.0;
         KC.Layer := Layer_Brick;
         KC.Mask  := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         KC.Half_Width := 30.0; KC.Half_Height := 15.0;
      end;
      
      Col_Sys.Update (S, 0.016);

      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      
      begin
         Assert (B.Health = 1,    "Strong brick at 1 health after first hit");
         Assert (not B.Is_Dying,  "Strong brick not dying after first hit");
      end;

      -- Second hit: reposition to overlap again
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position  := (300.0, 300.0);
      S.Transform.Data (S.Transform.Lookup (Brick_E)).Position := (300.0, 305.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, -300.0);
      Col_Sys.Update (S, 0.016);

      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      begin
         Assert (B.Health = 0,   "Strong brick at 0 health after second hit");
         Assert (B.Is_Dying,     "Strong brick dying after second hit");
      end;
   end;
   New_Line;


   ------------------------------------------------------------
   -- SECTION 6: Ball Physics System speed clamping
   -- Verifies that Ball_Physics_System keeps ball speed
   -- within Min_Speed and Max_Speed bounds.
   ------------------------------------------------------------
   Put_Line ("--- Section 6: Ball Speed Clamping ---");
   
   Initialize (S);
   
   declare
      Phys_Sys : Ball_Physics_System;
      Ball_E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Ball'Tag);

      -- Mark ball as launched (not attached)
      S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached := False;

      -- Test: overspeed clamping
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (900.0, 0.0);
      Phys_Sys.Update (S, 0.016);
      
      declare
         V : constant Vector2 :=
            S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity;
         
         Speed : constant Float := abs V.X;
      
      begin
         Assert (Speed <= 800.0, "Ball speed clamped to Max_Speed (800)");
      end;

      -- Test: underspeed clamping
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (50.0, 0.0);
      Phys_Sys.Update (S, 0.016);
      
      declare
         V : constant Vector2 :=
            S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity;
         Speed : constant Float := abs V.X;
      
      begin
         Assert (Speed >= 200.0, "Ball speed clamped to Min_Speed (200)");
      end;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- Final Test Result Summary Readout
   ------------------------------------------------------------
   Put_Line ("========================================");
   Put_Line ("Arkanoid Test Summary:");
   Put_Line ("  Total:  " & Natural'Image (Total));
   Put_Line ("  Passed: " & Natural'Image (Passed));
   Put_Line ("  Failed: " & Natural'Image (Failed));
   
   if Failed = 0 then
      Put_Line ("========================================");
      Put_Line ("ALL TESTS PASSED! :)");
      Put_Line ("========================================");
   
   else
      Put_Line ("========================================");
      Put_Line ("SOME TESTS FAILED :( ");
      Put_Line ("========================================");
   end if;

   New_Line;
   New_Line;

end Arkanoid_ECS_Test;
