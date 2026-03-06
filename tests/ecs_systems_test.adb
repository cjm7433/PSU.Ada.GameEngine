-- ecs_systems_test.adb
--
-- Comprehensive test suite for all ECS systems.
-- Tests each system in isolation to verify correct behavior.
--
-- Test coverage:
--   1. Movement System - velocity/acceleration integration
--   2. Paddle Control System - input handling and boundaries
--   3. Ball Physics System - attachment, speed clamping
--   4. Collision System - AABB detection, separation, bounce
--   5. Brick Destruction System - death timer, entity removal
--   6. System interaction - multi-system integration scenarios
--
--   Each test creates a minimal scenario (few entities, simple setup)
--   to isolate the system under test. This makes failures easy to
--   diagnose. Integration tests at the end verify systems work together.

with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Numerics;                     use Ada.Numerics;
with ECS.Entities;                     use ECS.Entities;
with ECS.Components;                   use ECS.Components;
with ECS.Components_Transform;         use ECS.Components_Transform;
with ECS.Components_Motion;            use ECS.Components_Motion;
with ECS.Components_Collision;         use ECS.Components_Collision;
with ECS.Components_Ball;              use ECS.Components_Ball;
with ECS.Components_Brick;             use ECS.Components_Brick;
with ECS.Components_Paddle;            use ECS.Components_Paddle;
with ECS.Store;                        use ECS.Store;
with ECS.Systems_Movement;             use ECS.Systems_Movement;
with ECS.Systems_Collision;            use ECS.Systems_Collision;
with ECS.Systems_Paddle_Control;       use ECS.Systems_Paddle_Control;
with ECS.Systems_Ball_Physics;         use ECS.Systems_Ball_Physics;
with ECS.Systems_Brick_Destruction;    use ECS.Systems_Brick_Destruction;
with Math.Linear_Algebra;              use Math.Linear_Algebra;

procedure ECS_Systems_Test is

   ------------------------------------------------------------
   -- Test state tracking
   ------------------------------------------------------------
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;


   ------------------------------------------------------------
   -- Helper: assert a condition and track pass/fail
   ------------------------------------------------------------
   procedure Assert (Condition : Boolean; Test_Name : String) is
   
   begin
      Total_Tests := Total_Tests + 1;
      
      if Condition then
         Put_Line ("[PASS] " & Test_Name);
         Passed_Tests := Passed_Tests + 1;
      
      else
         Put_Line ("[FAIL] " & Test_Name);
         Failed_Tests := Failed_Tests + 1;
      end if;
   
   end Assert;


   ------------------------------------------------------------
   -- Test fixture: Store instance (reset before each section)
   ------------------------------------------------------------
   S : Store;

begin
   Put_Line ("========================================");
   Put_Line ("ECS Systems Test Suite");
   Put_Line ("========================================");
   Put_Line ("Testing all game logic systems in isolation.");
   New_Line;


   ------------------------------------------------------------
   -- SECTION 1: Movement System
   -- 
   -- The Movement System integrates velocity and acceleration into
   -- position and rotation. It's the core physics integration step.
   --
   -- Tests verify:
   --   - Linear velocity --> position integration
   --   - Linear acceleration --> velocity integration
   --   - Angular velocity --> rotation integration
   --   - Rotation wrapping (keeps rotation in [0, 2pi))
   ------------------------------------------------------------
   Put_Line ("=== Section 1: Movement System ===");
   Put_Line ("Purpose: Integrate velocity/acceleration into Transform");
   New_Line;
   

   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 1.1: Linear velocity integration
   -- Setup: entity at origin with velocity (100, 50)
   -- Expectation: after 0.1s, position = (10, 5)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 1.1: Linear velocity integration");
   Initialize (S);
   
   declare
      Sys : Movement_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Add required components
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);

      -- Set initial state: at origin, moving right-up
      S.Transform.Data (S.Transform.Lookup (E)).Position := (0.0, 0.0);
      S.Motion.Data (S.Motion.Lookup (E)).Linear_Velocity := (100.0, 50.0);

      -- Run system for 0.1 seconds
      -- Expected: position = (0,0) + (100,50) * 0.1 = (10, 5)
      Sys.Update (S, 0.1);

      -- Verify position updated correctly using library's approximate equality
      declare
         T        : constant Transform := S.Transform.Data (S.Transform.Lookup (E));
         Expected : constant Vector2 := (10.0, 5.0);
      
      begin
         Assert (Is_Equal_Approximate (T.Position, Expected),
                 "Position X = velocity.X * dt");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 1.2: Linear acceleration integration
   -- Setup: entity at rest with acceleration (200, 0)
   -- Expectation: after 0.5s, velocity = (100, 0), position = (50, 0)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 1.2: Linear acceleration integration");
   Initialize (S);
   
   declare
      Sys : Movement_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);

      -- Start at rest with constant acceleration
      S.Motion.Data (S.Motion.Lookup (E)).Linear_Acceleration := (200.0, 0.0);

      -- Run for 0.5 seconds
      -- For explicit Euler in single step:
      --   v_new = 0 + 200*0.5 = 100
      --   x_new = 0 + 100*0.5 = 50
      Sys.Update (S, 0.5);

      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
         T : constant Transform := S.Transform.Data (S.Transform.Lookup (E));
         Expected_Vel : constant Vector2 := (100.0, 0.0);
         Expected_Pos : constant Vector2 := (50.0, 0.0);
      
      begin
         Assert (Is_Equal_Approximate (M.Linear_Velocity, Expected_Vel),
                 "Velocity = acceleration * dt");
         Assert (Is_Equal_Approximate (T.Position, Expected_Pos),
                 "Position integrates new velocity in same frame");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 1.3: Rotation wrapping
   -- Setup: entity with high angular velocity spinning multiple times
   -- Expectation: rotation stays wrapped in [0, 2pi)
   -- TODO: Test Currently fails (2/18/26)
   -- Replace with Math package normalization
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 1.3: Rotation wrapping");
   Initialize (S);
   
   declare
      Sys : Movement_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);

      -- Set high angular velocity (20 rad/s = ~3.2 rotations/sec)
      -- After 1 second: rotation = 20 radians = ~3.18 full circles
      -- Wrapped result should be 20 mod (2pi) ≈ 1.15 radians
      S.Motion.Data (S.Motion.Lookup (E)).Angular_Velocity := (20.0, 0.0);

      Sys.Update (S, 1.0);

      declare
         T : constant Transform := S.Transform.Data (S.Transform.Lookup (E));
      
      begin
         -- Rotation should be positive and less than 2pi
         Assert (T.Rotation >= 0.0 and T.Rotation < 2.0 * Pi,
                 "Rotation wrapped to [0, 2pi)");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 1.4: Multiple entities move independently
   -- Setup: three entities with different velocities
   -- Expectation: each integrates correctly without interference
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 1.4: Multiple entities move independently");
   Initialize (S);
   
   declare
      Sys : Movement_System;
      E1  : constant Entity_ID := Create_Entity (S);
      E2  : constant Entity_ID := Create_Entity (S);
      E3  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Entity 1: moves right
      Add_Component (S, E1, Transform'Tag);
      Add_Component (S, E1, Motion'Tag);
      S.Motion.Data (S.Motion.Lookup (E1)).Linear_Velocity := (100.0, 0.0);

      -- Entity 2: moves up
      Add_Component (S, E2, Transform'Tag);
      Add_Component (S, E2, Motion'Tag);
      S.Motion.Data (S.Motion.Lookup (E2)).Linear_Velocity := (0.0, 200.0);

      -- Entity 3: moves diagonal
      Add_Component (S, E3, Transform'Tag);
      Add_Component (S, E3, Motion'Tag);
      S.Motion.Data (S.Motion.Lookup (E3)).Linear_Velocity := (50.0, 50.0);

      -- Update all at once
      Sys.Update (S, 0.1);

      -- Verify each moved correctly
      declare
         Pos1 : constant Vector2 := S.Transform.Data (S.Transform.Lookup (E1)).Position;
         Pos2 : constant Vector2 := S.Transform.Data (S.Transform.Lookup (E2)).Position;
         Pos3 : constant Vector2 := S.Transform.Data (S.Transform.Lookup (E3)).Position;
      
      begin
         Assert (Is_Equal_Approximate (Pos1, (10.0, 0.0)),
                 "Entity 1 moved right");
         Assert (Is_Equal_Approximate (Pos2, (0.0, 20.0)),
                 "Entity 2 moved up");
         Assert (Is_Equal_Approximate (Pos3, (5.0, 5.0)),
                 "Entity 3 moved diagonally");
      end;
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- SECTION 2: Paddle Control System
   --
   -- The Paddle Control System reads input flags (Move_Left, Move_Right)
   -- and sets the paddle's velocity accordingly. It also enforces
   -- screen boundaries to keep the paddle on screen.
   --
   -- Tests verify:
   --   - Move_Right sets positive velocity
   --   - Move_Left sets negative velocity
   --   - Both pressed = no movement (cancels out)
   --   - Neither pressed = no movement (stops)
   --   - Boundary enforcement (paddle can't leave screen)
   ------------------------------------------------------------
   Put_Line ("=== Section 2: Paddle Control System ===");
   Put_Line ("Purpose: Convert input flags into paddle velocity");
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 2.1: Move right
   -- Setup: paddle at center, Move_Right = True
   -- Expectation: velocity becomes positive
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 2.1: Move right input");
   Initialize (S);
   
   declare
      Sys : Paddle_Control_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Setup paddle at center
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Paddle'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (400.0, 50.0);

      -- Press right
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Right := True;

      -- Update system
      Sys.Update (S, 0.016);              -- 60fps

      -- Velocity should be positive (moving right)
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
      
      begin
         Assert (M.Linear_Velocity.X > 0.0,
                 "Paddle velocity positive when Move_Right true");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 2.2: Move left
   -- Setup: paddle at center, Move_Left = True
   -- Expectation: velocity becomes negative
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 2.2: Move left input");
   Initialize (S);
   
   declare
      Sys : Paddle_Control_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Paddle'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (400.0, 50.0);

      -- Press left
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Left := True;

      Sys.Update (S, 0.016);              -- 60fps

      -- Velocity should be negative (moving left)
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
      
      begin
         Assert (M.Linear_Velocity.X < 0.0,
                 "Paddle velocity negative when Move_Left true");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 2.3: Both keys pressed (should cancel)
   -- Setup: paddle at center, both Move_Left and Move_Right = True
   -- Expectation: velocity = 0 (inputs cancel out)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 2.3: Both keys pressed (cancel out)");
   Initialize (S);
   
   declare
      Sys : Paddle_Control_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Paddle'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (400.0, 50.0);

      -- Press both
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Left := True;
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Right := True;

      Sys.Update (S, 0.016);              -- 60fps

      -- Velocity should be zero (neither direction wins)
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
      
      begin
         Assert (M.Linear_Velocity.X = 0.0,
                 "Paddle stops when both keys pressed");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 2.4: Boundary enforcement - left edge
   -- Setup: paddle near left boundary, trying to move left
   -- Expectation: position clamped to Min_X, velocity zeroed
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 2.4: Left boundary enforcement");
   Initialize (S);
   
   declare
      Sys : Paddle_Control_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Paddle'Tag);

      -- Position paddle beyond left boundary
      S.Transform.Data (S.Transform.Lookup (E)).Position := (40.0, 50.0);
      S.Paddle.Data (S.Paddle.Lookup (E)).Min_X := 50.0;
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Left := True;

      Sys.Update (S, 0.016);              -- 60fps

      -- Position should be clamped to Min_X
      declare
         T : constant Transform := S.Transform.Data (S.Transform.Lookup (E));
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
      
      begin
         Assert (T.Position.X >= 50.0,
                 "Paddle position clamped to left boundary");
         Assert (M.Linear_Velocity.X = 0.0,
                 "Paddle velocity zeroed at boundary");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 2.5: Boundary enforcement - right edge
   -- Setup: paddle near right boundary, trying to move right
   -- Expectation: position clamped to Max_X, velocity zeroed
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 2.5: Right boundary enforcement");
   Initialize (S);
   
   declare
      Sys : Paddle_Control_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Paddle'Tag);

      -- Position paddle beyond right boundary
      S.Transform.Data (S.Transform.Lookup (E)).Position := (760.0, 50.0);
      S.Paddle.Data (S.Paddle.Lookup (E)).Max_X := 750.0;
      S.Paddle.Data (S.Paddle.Lookup (E)).Move_Right := True;

      Sys.Update (S, 0.016);              -- 60fps

      -- Position should be clamped to Max_X
      declare
         T : constant Transform := S.Transform.Data (S.Transform.Lookup (E));
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
      
      begin
         Assert (T.Position.X <= 750.0,
                 "Paddle position clamped to right boundary");
         Assert (M.Linear_Velocity.X = 0.0,
                 "Paddle velocity zeroed at boundary");
      end;
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- SECTION 3: Ball Physics System
   --
   -- The Ball Physics System enforces ball-specific constraints:
   --   - Attachment to paddle (ball follows paddle when Is_Attached)
   --   - Speed clamping (keeps ball between Min_Speed and Max_Speed)
   --
   -- Tests verify:
   --   - Attached ball follows paddle position
   --   - Attached ball has zero velocity
   --   - Detached ball clamps overspeed to Max_Speed
   --   - Detached ball clamps underspeed to Min_Speed
   --   - Speed clamping preserves direction
   ------------------------------------------------------------
   Put_Line ("=== Section 3: Ball Physics System ===");
   Put_Line ("Purpose: Enforce ball attachment and speed limits");
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 3.1: Attached ball follows paddle
   -- Setup: ball attached, paddle at specific position
   -- Expectation: ball positioned above paddle, velocity = 0
   -- TODO: Test currently fails (2/18/26):
   --    [FAIL] Attached ball X = paddle X + offset
   --    [FAIL] Attached ball Y above paddle
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 3.1: Attached ball follows paddle");
   Initialize (S);
   
   declare
      Sys      : Ball_Physics_System;
      Paddle_E : constant Entity_ID := Create_Entity (S);
      Ball_E   : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Setup paddle
      Add_Component (S, Paddle_E, Transform'Tag);

      S.Transform.Data (S.Transform.Lookup (Paddle_E)).Position := (300.0, 50.0);
      
      -- Setup ball (attached)
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Ball'Tag);

      S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached := True;
      S.Ball.Data (S.Ball.Lookup (Ball_E)).Attach_Offset_X := 10.0;

      -- Initial ball position shouldn't matter - system will override (to 310)
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (0.0, 0.0);

      Sys.Update (S, 0.016);     -- 60fps

      -- Ball should be positioned above paddle center + offset
      declare
         T : constant Transform := S.Transform.Data (S.Transform.Lookup (Ball_E));
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (Ball_E));
      
      begin
         Assert (Is_Equal_Approximate (T.Position.X, 310.0),
                 "Attached ball X = paddle X + offset");
         Assert (T.Position.Y > 50.0,
                 "Attached ball Y above paddle");
         Assert (M.Linear_Velocity = (0.0, 0.0),
                 "Attached ball has zero velocity");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 3.2: Overspeed clamping
   -- Setup: detached ball moving faster than Max_Speed
   -- Expectation: velocity magnitude clamped to Max_Speed, direction preserved
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 3.2: Overspeed clamping");
   Initialize (S);
   
   declare
      Sys : Ball_Physics_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Ball'Tag);

      -- Detach ball and set overspeed (diagonal, very fast)
      S.Ball.Data (S.Ball.Lookup (E)).Is_Attached := False;
      S.Ball.Data (S.Ball.Lookup (E)).Max_Speed := 800.0;
      S.Motion.Data (S.Motion.Lookup (E)).Linear_Velocity := (1000.0, 1000.0);

      Sys.Update (S, 0.016);              -- 60fps

      -- Speed should be clamped to max
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
         Speed : constant Float := Length (M.Linear_Velocity);
      
      begin
         Assert (Speed <= 800.01,
                 "Ball speed clamped to Max_Speed");
         -- Direction should be preserved (both X and Y still positive)
         Assert (M.Linear_Velocity.X > 0.0 and M.Linear_Velocity.Y > 0.0,
                 "Ball direction preserved during clamp");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 3.3: Underspeed clamping
   -- Setup: detached ball moving slower than Min_Speed
   -- Expectation: velocity magnitude increased to Min_Speed, direction preserved
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 3.3: Underspeed clamping");
   Initialize (S);
   
   declare
      Sys : Ball_Physics_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Transform'Tag);
      Add_Component (S, E, Motion'Tag);
      Add_Component (S, E, Ball'Tag);

      -- Detach ball and set underspeed
      S.Ball.Data (S.Ball.Lookup (E)).Is_Attached := False;
      S.Ball.Data (S.Ball.Lookup (E)).Min_Speed := 200.0;
      S.Motion.Data (S.Motion.Lookup (E)).Linear_Velocity := (30.0, 40.0);
      -- Speed = sqrt(30^2 + 40^2) = 50, which is < 200

      Sys.Update (S, 0.016);        -- 60fps

      -- Speed should be increased to min
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (E));
         Speed : constant Float := Length (M.Linear_Velocity);
      
      begin
         Assert (Speed >= 199.99,
                 "Ball speed increased to Min_Speed");
         -- Direction should be preserved
         Assert (M.Linear_Velocity.X > 0.0 and M.Linear_Velocity.Y > 0.0,
                 "Ball direction preserved during clamp");
      end;
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- SECTION 4: Collision System
   --
   -- The Collision System detects overlapping AABB boxes and:
   --   1. Separates entities along the axis of least penetration
   --   2. Reflects velocity on the separation axis (bounce)
   --   3. Applies damage to bricks when hit by balls
   --
   -- Tests verify:
   --   - Overlapping entities detected
   --   - Entities separated after collision
   --   - Velocity reflected (bounced)
   --   - Layer/mask filtering works correctly
   --   - Brick takes damage when hit by ball
   --   - Non-matching layers don't collide
   ------------------------------------------------------------
   Put_Line ("=== Section 4: Collision System ===");
   Put_Line ("Purpose: Detect overlaps, separate, bounce, damage bricks");
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 4.1: Horizontal collision and bounce
   -- Setup: ball moving right into wall
   -- Expectation: ball X velocity becomes negative (bounced)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 4.1: Horizontal collision and bounce");
   Initialize (S);
   
   declare
      Sys    : Collision_System;
      Ball_E : constant Entity_ID := Create_Entity (S);
      Wall_E : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Ball moving right
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (100.0, 100.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (300.0, 0.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Wall, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 8.0;
         C.Half_Height := 8.0;
      end;

      -- Wall (overlapping on right side)
      Add_Component (S, Wall_E, Transform'Tag);
      Add_Component (S, Wall_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Wall_E)).Position := (110.0, 100.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Wall_E));
      
      begin
         C.Layer := Layer_Wall;
         C.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 50.0;
      end;

      -- Run collision
      Sys.Update (S, 0.016);     -- 60fps

      -- Ball velocity X should be negative (bounced left)
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (Ball_E));
      
      begin
         Assert (M.Linear_Velocity.X < 0.0,
                 "Ball X velocity reversed after wall collision");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 4.2: Vertical collision and bounce
   -- Setup: ball moving up into ceiling
   -- Expectation: ball Y velocity becomes negative (bounced down)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 4.2: Vertical collision and bounce");
   Initialize (S);
   
   declare
      Sys      : Collision_System;
      Ball_E   : constant Entity_ID := Create_Entity (S);
      Ceiling_E : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Ball moving up
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (200.0, 200.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, 400.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Wall, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 8.0;
         C.Half_Height := 8.0;
      end;

      -- Ceiling (overlapping above ball)
      Add_Component (S, Ceiling_E, Transform'Tag);
      Add_Component (S, Ceiling_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Ceiling_E)).Position := (200.0, 210.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ceiling_E));
      
      begin
         C.Layer := Layer_Wall;
         C.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 50.0;
         C.Half_Height := 10.0;
      end;

      Sys.Update (S, 0.016);           -- 60fps

      -- Ball velocity Y should be negative (bounced down)
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (Ball_E));
      
      begin
         Assert (M.Linear_Velocity.Y < 0.0,
                 "Ball Y velocity reversed after ceiling collision");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 4.3: Entities separated after collision
   -- Setup: two overlapping entities
   -- Expectation: after collision resolution, boxes no longer overlap
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 4.3: Entities separated after collision");
   Initialize (S);
   
   declare
      Sys : Collision_System;
      E1  : constant Entity_ID := Create_Entity (S);
      E2  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Entity 1 (moving right)
      Add_Component (S, E1, Transform'Tag);
      Add_Component (S, E1, Motion'Tag);
      Add_Component (S, E1, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (E1)).Position := (100.0, 100.0);
      S.Motion.Data (S.Motion.Lookup (E1)).Linear_Velocity := (100.0, 0.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (E1));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Paddle, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 10.0;
      end;

      -- Entity 2 (static, overlapping)
      Add_Component (S, E2, Transform'Tag);
      Add_Component (S, E2, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (E2)).Position := (105.0, 100.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (E2));
      
      begin
         C.Layer := Layer_Paddle;
         C.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 10.0;
      end;

      -- Initial state: boxes overlap (distance = 5, sum of half-widths = 20)
      Sys.Update (S, 0.016);           -- 60fps

      -- After separation: distance should be >= sum of half-widths
      declare
         T1 : constant Transform := S.Transform.Data (S.Transform.Lookup (E1));
         T2 : constant Transform := S.Transform.Data (S.Transform.Lookup (E2));
         Separation : constant Float := Distance (T1.Position, T2.Position);
      
      begin
         Assert (Separation >= 19.99,
                 "Entities separated (no longer overlapping)");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 4.4: Layer/mask filtering - no collision
   -- Setup: two overlapping entities with incompatible layers
   -- Expectation: no bounce occurs (layers don't match masks)
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 4.4: Layer/mask filtering prevents collision");
   Initialize (S);
   
   declare
      Sys : Collision_System;
      E1  : constant Entity_ID := Create_Entity (S);
      E2  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Entity 1: Layer_Ball, only collides with Layer_Wall
      Add_Component (S, E1, Transform'Tag);
      Add_Component (S, E1, Motion'Tag);
      Add_Component (S, E1, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (E1)).Position := (100.0, 100.0);
      S.Motion.Data (S.Motion.Lookup (E1)).Linear_Velocity := (100.0, 0.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (E1));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Wall, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 10.0;
      end;

      -- Entity 2: Layer_Paddle, only collides with Layer_Brick
      -- Ball and Paddle layers/masks don't match, so no collision
      Add_Component (S, E2, Transform'Tag);
      Add_Component (S, E2, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (E2)).Position := (105.0, 100.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (E2));
      
      begin
         C.Layer := Layer_Paddle;
         C.Mask := (Layer_Brick, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 10.0;
      end;

      -- Store initial velocity
      declare
         Initial_Vel : constant Vector2 :=
            S.Motion.Data (S.Motion.Lookup (E1)).Linear_Velocity;
      
      begin
         Sys.Update (S, 0.016);           -- 60fps

         -- Velocity should be unchanged (no collision occurred)
         declare
            M : constant Motion := S.Motion.Data (S.Motion.Lookup (E1));
         
         begin
            Assert (M.Linear_Velocity = Initial_Vel,
                    "No velocity change when layers don't match");
         end;
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 4.5: Brick damage on ball collision
   -- Setup: ball collides with brick
   -- Expectation: brick health decreases, Is_Dying becomes True at 0 health
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 4.5: Brick takes damage from ball collision");
   Initialize (S);
   
   declare
      Sys     : Collision_System;
      Ball_E  : constant Entity_ID := Create_Entity (S);
      Brick_E : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Ball
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      Add_Component (S, Ball_E, Ball'Tag);
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (200.0, 200.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, 300.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Brick, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 8.0;
         C.Half_Height := 8.0;
      end;

      -- Brick (overlapping)
      Add_Component (S, Brick_E, Transform'Tag);
      Add_Component (S, Brick_E, Collision'Tag);
      Add_Component (S, Brick_E, Brick'Tag);
      S.Transform.Data (S.Transform.Lookup (Brick_E)).Position := (200.0, 205.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Brick_E));
      
      begin
         C.Layer := Layer_Brick;
         C.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 30.0;
         C.Half_Height := 12.0;
      end;

      -- Initial brick has 1 health
      Assert (S.Brick.Data (S.Brick.Lookup (Brick_E)).Health = 1,
              "Brick starts with 1 health");

      -- Run collision
      Sys.Update (S, 0.016);              -- 60fps

      -- Brick should be damaged
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      
      begin
         Assert (B.Health = 0,
                 "Brick health reduced to 0");
         Assert (B.Is_Dying,
                 "Brick marked as dying");
         Assert (B.Death_Timer > 0.0,
                 "Brick death timer started");
      end;
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- SECTION 5: Brick Destruction System
   --
   -- The Brick Destruction System ticks down death timers on dying
   -- bricks and removes them when the timer expires.
   --
   -- Tests verify:
   --   - Death timer decrements correctly
   --   - Brick remains until timer expires
   --   - Brick removed when timer <= 0
   --   - Non-dying bricks unaffected
   ------------------------------------------------------------
   Put_Line ("=== Section 5: Brick Destruction System ===");
   Put_Line ("Purpose: Tick death timers and remove expired bricks");
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 5.1: Death timer ticks down
   -- Setup: dying brick with timer = 0.5
   -- Expectation: after 0.2s, timer = 0.3
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 5.1: Death timer decrements");
   Initialize (S);
   
   declare
      Sys : Brick_Destruction_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Brick'Tag);

      -- Mark as dying with 0.5s timer
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (E));
      
      begin
         B.Is_Dying := True;
         B.Death_Timer := 0.5;
      end;

      -- Tick 0.2 seconds
      Sys.Update (S, 0.2);

      -- Timer should be 0.3 now
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (E));
      
      begin
         Assert (Is_Equal_Approximate (B.Death_Timer, 0.3),
                 "Death timer decremented by dt");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 5.2: Brick remains until timer expires
   -- Setup: dying brick with small positive timer
   -- Expectation: brick still exists after small update
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 5.2: Brick remains while timer > 0");
   Initialize (S);
   
   declare
      Sys : Brick_Destruction_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Brick'Tag);

      -- Dying with 0.1s remaining
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (E));
      
      begin
         B.Is_Dying := True;
         B.Death_Timer := 0.1;
      end;

      -- Tick only 0.05s (timer still positive)
      Sys.Update (S, 0.05);

      Assert (Has_Entity (S, E),
              "Brick still exists before timer expires");
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 5.3: Brick removed when timer expires
   -- Setup: dying brick with small timer
   -- Expectation: brick destroyed after timer reaches 0
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 5.3: Brick destroyed when timer <= 0");
   Initialize (S);
   
   declare
      Sys : Brick_Destruction_System;
      E   : constant Entity_ID := Create_Entity (S);
   
   begin
      Add_Component (S, E, Brick'Tag);

      -- Dying with 0.1s timer
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (E));
      
      begin
         B.Is_Dying := True;
         B.Death_Timer := 0.1;
      end;

      -- Tick enough to expire the timer
      Sys.Update (S, 0.5);

      Assert (not Has_Entity (S, E),
              "Brick destroyed after timer expires");
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 5.4: Non-dying bricks unaffected
   -- Setup: healthy brick (not dying) and dying brick
   -- Expectation: healthy brick remains, dying brick removed
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 5.4: Healthy bricks unaffected");
   Initialize (S);
   
   declare
      Sys      : Brick_Destruction_System;
      Healthy  : constant Entity_ID := Create_Entity (S);
      Dying    : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Healthy brick
      Add_Component (S, Healthy, Brick'Tag);
      -- (Is_Dying defaults to False)

      -- Dying brick
      Add_Component (S, Dying, Brick'Tag);
      
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Dying));
      
      begin
         B.Is_Dying := True;
         B.Death_Timer := 0.1;
      end;

      -- Update system
      Sys.Update (S, 0.5);

      Assert (Has_Entity (S, Healthy),
              "Healthy brick remains");
      Assert (not Has_Entity (S, Dying),
              "Dying brick destroyed");
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- SECTION 6: System Integration
   --
   -- These tests verify that multiple systems work together
   -- correctly in realistic scenarios.
   --
   -- Tests verify:
   --   - Movement --> Collision --> Bounce sequence
   --   - Paddle Control --> Movement chain
   --   - Ball Physics --> Movement --> Collision chain
   --   - Full gameplay: ball hits paddle, bounces, hits brick
   ------------------------------------------------------------
   Put_Line ("=== Section 6: System Integration ===");
   Put_Line ("Purpose: Verify systems work together correctly");
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 6.1: Movement with collision (ball hits wall)
   -- Setup: ball with velocity pointing at wall
   -- Expectation: movement moves ball, collision bounces it back
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 6.1: Movement --> Collision sequence");
   Initialize (S);
   
   declare
      Move_Sys : Movement_System;
      Col_Sys  : Collision_System;
      Ball_E   : constant Entity_ID := Create_Entity (S);
      Wall_E   : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Ball starting away from wall, moving toward it
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (50.0, 100.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (500.0, 0.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
      
      begin
         C.Layer := Layer_Ball;
         C.Mask := (Layer_Wall, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 8.0;
         C.Half_Height := 8.0;
      end;

      -- Wall ahead
      Add_Component (S, Wall_E, Transform'Tag);
      Add_Component (S, Wall_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Wall_E)).Position := (100.0, 100.0);
      
      declare
         C : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Wall_E));
      
      begin
         C.Layer := Layer_Wall;
         C.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         C.Half_Width := 10.0;
         C.Half_Height := 50.0;
      end;

      -- First frame: movement advances ball
      Move_Sys.Update (S, 0.1);  -- Ball moves 50 units right
      -- Now ball is at X=100, overlapping wall

      -- Second frame: collision detects overlap and bounces
      Col_Sys.Update (S, 0.016);             -- 60fps

      -- Ball should now be moving left
      declare
         M : constant Motion := S.Motion.Data (S.Motion.Lookup (Ball_E));
      
      begin
         Assert (M.Linear_Velocity.X < 0.0,
                 "Ball bounced after movement brought it into wall");
      end;
   end;
   
   New_Line;


   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   -- TEST 6.2: Full Arkanoid sequence
   -- Setup: paddle, ball, brick
   -- Expectation: ball hits paddle, bounces up, hits brick, brick dies
   -- TODO: Tests currently fail (2/18/26):
   --    [FAIL] Ball bounced up off paddle
   --    [FAIL] Brick marked as dying after ball hit
   --    [FAIL] Brick removed after death timer
   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Put_Line ("Test 6.2: Complete Arkanoid gameplay sequence");
   Initialize (S);
   
   declare
      Move_Sys : Movement_System;
      Col_Sys  : Collision_System;
      Dest_Sys : Brick_Destruction_System;
      
      Paddle_E : constant Entity_ID := Create_Entity (S);
      Ball_E   : constant Entity_ID := Create_Entity (S);
      Brick_E  : constant Entity_ID := Create_Entity (S);
   
   begin
      -- Paddle at bottom
      Add_Component (S, Paddle_E, Transform'Tag);
      Add_Component (S, Paddle_E, Collision'Tag);
      S.Transform.Data (S.Transform.Lookup (Paddle_E)).Position := (200.0, 50.0);
      
      declare
         CP : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Paddle_E));
      
      begin
         CP.Layer := Layer_Paddle;
         CP.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         CP.Half_Width := 50.0;
         CP.Half_Height := 10.0;
      end;

      -- Ball falling toward paddle
      Add_Component (S, Ball_E, Transform'Tag);
      Add_Component (S, Ball_E, Motion'Tag);
      Add_Component (S, Ball_E, Collision'Tag);
      Add_Component (S, Ball_E, Ball'Tag);
      S.Transform.Data (S.Transform.Lookup (Ball_E)).Position := (200.0, 100.0);
      S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity := (0.0, -300.0);
      
      declare
         CB : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Ball_E));
      
      begin
         CB.Layer := Layer_Ball;
         CB.Mask := (Layer_Paddle, Layer_Brick, Layer_None, Layer_None);
         CB.Half_Width := 8.0;
         CB.Half_Height := 8.0;
      end;

      -- Brick above
      Add_Component (S, Brick_E, Transform'Tag);
      Add_Component (S, Brick_E, Collision'Tag);
      Add_Component (S, Brick_E, Brick'Tag);
      S.Transform.Data (S.Transform.Lookup (Brick_E)).Position := (200.0, 150.0);
      
      declare
         K : ECS.Components_Collision.Collision renames
            S.Collision.Data (S.Collision.Lookup (Brick_E));
      
      begin
         K.Layer := Layer_Brick;
         K.Mask := (Layer_Ball, Layer_None, Layer_None, Layer_None);
         K.Half_Width := 30.0;
         K.Half_Height := 12.0;
      end;

      -- Frame 1: Ball falls and hits paddle
      Move_Sys.Update (S, 0.1);  -- Ball moves down
      Col_Sys.Update (S, 0.016);  -- Collision detects paddle hit; 60fps
      
      -- Ball should now be moving up
      declare
         M : Motion := S.Motion.Data (S.Motion.Lookup (Ball_E));
      
      begin
         Assert (M.Linear_Velocity.Y > 0.0,
                 "Ball bounced up off paddle");
         
         -- Force ball velocity up for next phase
         M.Linear_Velocity := (0.0, 300.0);
      end;

      -- Frame 2: Ball rises and hits brick
      Move_Sys.Update (S, 0.05);  -- Ball moves up
      Col_Sys.Update (S, 0.016);  -- Collision detects brick hit; 60fps

      -- Brick should be damaged
      declare
         B : ECS.Components_Brick.Brick renames
            S.Brick.Data (S.Brick.Lookup (Brick_E));
      
      begin
         Assert (B.Is_Dying,
                 "Brick marked as dying after ball hit");
      end;

      -- Frame 3: Destruction system removes brick
      Dest_Sys.Update (S, 1.0);
      
      Assert (not Has_Entity (S, Brick_E),
              "Brick removed after death timer");
   end;
   
   New_Line;


   ------------------------------------------------------------
   -- Final Test Summary
   ------------------------------------------------------------
   Put_Line ("========================================");
   Put_Line ("ECS Systems Test Summary:");
   Put_Line ("  Total Tests:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed:       " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed:       " & Natural'Image (Failed_Tests));
   
   if Failed_Tests = 0 then
      Put_Line ("========================================");
      Put_Line ("ALL SYSTEMS TESTS PASSED! :)");
      Put_Line ("Systems are functioning correctly.");
      Put_Line ("========================================");
   
   else
      Put_Line ("========================================");
      Put_Line ("SOME TESTS FAILED! :(");
      Put_Line ("Review output above for details.");
      Put_Line ("========================================");
   end if;

   New_Line;
   New_Line;

end ECS_Systems_Test;
