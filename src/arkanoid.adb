-- arkanoid.adb
--
-- MINIMAL ARKANOID IMPLEMENTATION
-- ================================
-- This creates a simple playable Arkanoid demo

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components_Transform;      use ECS.Components_Transform;
with ECS.Components_Motion;         use ECS.Components_Motion;
with ECS.Components_Collision;      use ECS.Components_Collision;
with ECS.Components_Paddle;         use ECS.Components_Paddle;
with ECS.Components_Ball;           use ECS.Components_Ball;
with ECS.Components_Brick;          use ECS.Components_Brick;
with ECS.Components_Renderable;     use ECS.Components_Renderable;
with Input;
with Math.Linear_Algebra;           use Math.Linear_Algebra;


package body Arkanoid is

   -- Game constants
   -- TODO: Play with these
   Screen_Width  : constant Float := 800.0;
   Screen_Height : constant Float := 600.0;


   ------------------------------------------------------------
   -- Initialize: Create initial scene (paddle, ball, bricks, walls)
   -- Called once at startup by the engine
   ------------------------------------------------------------
   procedure Initialize (G : in out Arkanoid_Game) is
   
   begin
      -- Initialize ECS Manager (clears store, sets up systems)
      G.Manager.Initialize;
      
      -- Load level 1
      Load_Level (G, 1);
   
   end Initialize;


   ------------------------------------------------------------
   -- Handle_Input: Read Input module, apply to paddle entity
   -- Called every frame before Update
   ------------------------------------------------------------
   procedure Handle_Input (
      G  : in out Arkanoid_Game;
      DT : Float) is
      
      S : access Store.Store := G.Manager.Get_Store;
   
   begin
      -- Skip input if not playing
      if G.State /= Playing then
         return;
      end if;
      
      -- Apply keyboard input to paddle entity
      -- This reads Input module and sets Paddle component flags
      Input.Apply_Input_To_Paddle (S.all);
      
      -- Handle ball launch with spacebar

      -- If detected launch ball input and there is a ball ...
      if Input.Is_Launch_Ball and then G.Ball_Entity /= 0 then
         if S.Has_Component (G.Ball_Entity, Ball'Tag) then
            
            declare
               Ball_Index : constant Natural := S.Ball.Lookup (G.Ball_Entity);
               B : Ball renames S.Ball.Data (Ball_Index);
            
            begin
               -- If the ball is attached to paddle ...
               if B.Is_Attached then
                  
                  -- Now it's not :)
                  B.Is_Attached := False;
                  
                  -- Give ball initial upward velocity
                  if S.Has_Component (G.Ball_Entity, Motion'Tag) then
                     
                     declare
                        Motion_Index : constant Natural := S.Motion.Lookup (G.Ball_Entity);
                        M : Motion renames S.Motion.Data (Motion_Index);
                     
                     begin
                        M.Linear_Velocity := (0.0, 300.0);  -- Launch upward
                     end;
                  
                  end if;
               end if;
            end;
         end if;
      end if;
   
   end Handle_Input;


   ------------------------------------------------------------
   -- Update: Run all systems, check game state + win/lose conditions
   -- Called at fixed 60fps by the engine -- DT = 0.016 (16.67ms)
   ------------------------------------------------------------
   procedure Update (
      G  : in out Arkanoid_Game;
      DT : Float) is
   
   begin
      -- Skip update if not playing
      if G.State /= Playing then
         return;
      end if;
      
      -- Run all ECS systems in order
      G.Manager.Update (DT);
      
      -- Check win/lose conditions
      if Is_Ball_Lost (G) then
         G.Lives := G.Lives - 1;
         
         -- Game over if 0 lives
         if G.Lives = 0 then
            G.State := Game_Over;
         
         else
            -- Reset ball to attached state
            Load_Level (G, G.Level);  -- Reload current level
         end if;
      
      -- Next level logic 😎
      elsif Is_Level_Complete (G) then
         G.Level := G.Level + 1;
         G.State := Victory;
         -- TODO: Could auto-load next level here (not needed for demo)
      end if;
   
   end Update;


   ------------------------------------------------------------
   -- Render: Draw all entities to screen
   -- Called at display refresh rate (may be faster than Update)
   -- DC = Windows device context to draw to
   -- TODO: Linux compatibility here!
   ------------------------------------------------------------
   procedure Render (
      G  : in out Arkanoid_Game;
      DC : HDC) is
      
      S : access Store.Store := G.Manager.Get_Store;
      
      -- Query entities with renderable components
      Entities : Entity_ID_Array_Access :=
         S.Get_Entities_With ((Transform'Tag, Renderable'Tag));
      
      -- GDI objects for drawing
      White_Brush : HBRUSH;
      Old_Brush   : HBRUSH;
   
   begin
      -- Clear screen to black
      declare
         Rect : aliased RECT;
      
      begin
         Rect.left := 0;
         Rect.top := 0;
         Rect.right := Int (Screen_Width);
         Rect.bottom := Int (Screen_Height);
         FillRect (DC, Rect'Access, GetStockObject (BLACK_BRUSH));
      end;
      
      -- Create white brush for drawing
      White_Brush := GetStockObject (WHITE_BRUSH);
      Old_Brush := SelectObject (DC, White_Brush);
      
      -- Draw all entities
      if Entities /= null then
         
         for I in Entities'Range loop
         
            declare
               -- Ol' reliable code block
               E : constant Entity_ID := Entities (I);
               
               T_Index : constant Natural := S.Transform.Lookup (E);
               T : Transform renames S.Transform.Data (T_Index);
               
               R_Index : constant Natural := S.Renderable.Lookup (E);
               R : Renderable renames S.Renderable.Data (R_Index);
               
               -- Calculate rectangle for this entity
               Left   : constant Int := Int (T.Position.X - R.Half_Width);
               Top    : constant Int := Int (T.Position.Y - R.Half_Height);
               Right  : constant Int := Int (T.Position.X + R.Half_Width);
               Bottom : constant Int := Int (T.Position.Y + R.Half_Height);
            
            begin
               -- Draw as rectangle (simple for now)
               Rectangle (DC, Left, Top, Right, Bottom);
            end;
         end loop;
      end if;
      
      -- Restore old brush
      SelectObject (DC, Old_Brush);       -- TODO: Idk why this is here and maybe doesnt have to be
   
   end Render;


   --===========================================================
   -- PRIVATE HELPERS
   --===========================================================

   ------------------------------------------------------------
   -- Load_Level
   -- Create the initial game scene
   -- Called by Initialize to set up paddle, ball, bricks, walls
   ------------------------------------------------------------
   procedure Load_Level (
      G     : in out Arkanoid_Game;
      Level : Positive := 1) is
      
      S : access Store.Store := G.Manager.Get_Store;
      E : Entity_ID;
   
   begin
      -- Clear existing entities
      S.Initialize;
      
      --~~~~~~~~~~~~~~~~~~~
      -- CREATE PADDLE
      --~~~~~~~~~~~~~~~~~~~
      E := Create_Entity (S.all);      -- New Paddle entity
      G.Paddle_Entity := E;
      
      Add_Component (S.all, E, Transform'Tag);
      S.Transform.Data (S.Transform.Lookup (E)) := (
         Position => (400.0, 550.0),      -- Bottom center
         Rotation => 0.0,
         Scale    => (1.0, 1.0)
      );
      
      Add_Component (S.all, E, Motion'Tag);     -- Motion defaults are fine (zero velocity)
      
      Add_Component (S.all, E, Collision'Tag);
      S.Collision.Data (S.Collision.Lookup (E)) := (     -- Paddle collides with ball, bounded by component details
         Layer       => Layer_Paddle,
         Mask        => (Layer_Ball, Layer_None, Layer_None, Layer_None),
         Half_Width  => 50.0,
         Half_Height => 10.0,
         Is_Trigger  => False
      );
      
      Add_Component (S.all, E, Paddle'Tag);
      S.Paddle.Data (S.Paddle.Lookup (E)) := (
         Move_Left  => False,
         Move_Right => False,
         Speed      => 400.0,
         Min_X      => 50.0,
         Max_X      => 750.0
      );
      
      Add_Component (S.all, E, Renderable'Tag);
      S.Renderable.Data (S.Renderable.Lookup (E)) := (
         Half_Width  => 50.0,
         Half_Height => 10.0,
         Color       => 16#FFFFFF#     -- Paddle is white (16#FFFFFF#)
      );
      
      --~~~~~~~~~~~~~~~~~~~
      -- CREATE BALL
      --~~~~~~~~~~~~~~~~~~~
      E := Create_Entity (S.all);
      G.Ball_Entity := E;
      
      Add_Component (S.all, E, Transform'Tag);        -- New ball entity

      -- Place the ball above the paddle
      S.Transform.Data (S.Transform.Lookup (E)) := (
         Position => (400.0, 520.0),
         Rotation => 0.0,
         Scale    => (1.0, 1.0)
      );
      
      Add_Component (S.all, E, Motion'Tag);     -- Starts with zero velocity (attached)
      
      Add_Component (S.all, E, Collision'Tag);
      S.Collision.Data (S.Collision.Lookup (E)) := (     -- Ball collides with a lot of stuff:
         Layer       => Layer_Ball,
         Mask        => (Layer_Paddle, Layer_Brick, Layer_Wall, Layer_None),
         Half_Width  => 8.0,
         Half_Height => 8.0,
         Is_Trigger  => False
      );
      
      Add_Component (S.all, E, Ball'Tag);          -- Balls be ballin, you know?
      S.Ball.Data (S.Ball.Lookup (E)) := (
         Is_Attached     => True,
         Attach_Offset_X => 0.0,
         Min_Speed       => 200.0,
         Max_Speed       => 600.0
      );
      
      Add_Component (S.all, E, Renderable'Tag);    -- I'm pretty good at drawin balls.
      S.Renderable.Data (S.Renderable.Lookup (E)) := (
         Half_Width  => 8.0,
         Half_Height => 8.0,
         Color       => 16#FFFFFF#  -- Ball is white
      );
      
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- CREATE BRICKS (8 columns × 3 rows)
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      declare
         Brick_Width  : constant Float := 70.0;
         Brick_Height : constant Float := 20.0;
         Start_X      : constant Float := 100.0;
         Start_Y      : constant Float := 100.0;
         Gap          : constant Float := 10.0;
      
      begin
         -- Create bricks in 3x8 pattern        -- TODO: Remake this with variables not magic numbers (for level creation)
         for Row in 0 .. 2 loop
            for Col in 0 .. 7 loop
               
               E := Create_Entity (S.all);      -- New brick entity
               
               -- Give transform
               Add_Component (S.all, E, Transform'Tag);
               S.Transform.Data (S.Transform.Lookup (E)) := (
                  Position => (
                     Start_X + Float (Col) * (Brick_Width + Gap),
                     Start_Y + Float (Row) * (Brick_Height + Gap)
                  ),
                  Rotation => 0.0,
                  Scale    => (1.0, 1.0)
               );
               
               -- Give Collision
               Add_Component (S.all, E, Collision'Tag);
               S.Collision.Data (S.Collision.Lookup (E)) := (
                  Layer       => Layer_Brick,
                  Mask        => (Layer_Ball, Layer_None, Layer_None, Layer_None),
                  Half_Width  => Brick_Width / 2.0,
                  Half_Height => Brick_Height / 2.0,
                  Is_Trigger  => False
               );
               
               -- Give Brick
               Add_Component (S.all, E, Brick'Tag);
               S.Brick.Data (S.Brick.Lookup (E)) := (
                  Health      => 1,
                  Is_Dying    => False,
                  Death_Timer => 0.0
               );
               
               -- Give Renderable
               Add_Component (S.all, E, Renderable'Tag);
               S.Renderable.Data (S.Renderable.Lookup (E)) := (
                  Half_Width  => Brick_Width / 2.0,
                  Half_Height => Brick_Height / 2.0,
                  Color       => 16#008000#        -- Bricks are Green because that's my favorite color
               );
            end loop;
         end loop;
      end;
      
      
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- CREATE WALLS (top, left, right)
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      -- Top wall
      E := Create_Entity (S.all);            -- New Wall Entity

      -- Give Transform
      Add_Component (S.all, E, Transform'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (400.0, 10.0);
      
      -- Give Collision
      Add_Component (S.all, E, Collision'Tag);
      S.Collision.Data (S.Collision.Lookup (E)) := (
         Layer       => Layer_Wall,             -- Only collides with Ball
         Mask        => (Layer_Ball, Layer_None, Layer_None, Layer_None),
         Half_Width  => 400.0,
         Half_Height => 10.0,
         Is_Trigger  => False
      );

      -- Give Renderable
      Add_Component (S.all, E, Renderable'Tag);
      S.Renderable.Data (S.Renderable.Lookup (E)) := (
         Half_Width  => 400.0,
         Half_Height => 10.0,
         Color       => 16#808080#     -- Walls are grey
      );
      
      -- Left wall (Repeat as above)
      E := Create_Entity (S.all);
      Add_Component (S.all, E, Transform'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (10.0, 300.0);
      Add_Component (S.all, E, Collision'Tag);
      S.Collision.Data (S.Collision.Lookup (E)) := (
         Layer       => Layer_Wall,
         Mask        => (Layer_Ball, Layer_Paddle, Layer_None, Layer_None),
         Half_Width  => 10.0,
         Half_Height => 300.0,
         Is_Trigger  => False
      );
      Add_Component (S.all, E, Renderable'Tag);
      S.Renderable.Data (S.Renderable.Lookup (E)) := (
         Half_Width  => 10.0,
         Half_Height => 300.0,
         Color       => 16#FFFFFF#     -- Walls are grey
      );
      
      -- Right wall (Repeat as above)
      E := Create_Entity (S.all);
      Add_Component (S.all, E, Transform'Tag);
      S.Transform.Data (S.Transform.Lookup (E)).Position := (790.0, 300.0);
      Add_Component (S.all, E, Collision'Tag);
      S.Collision.Data (S.Collision.Lookup (E)) := (
         Layer       => Layer_Wall,
         Mask        => (Layer_Ball, Layer_Paddle, Layer_None, Layer_None),
         Half_Width  => 10.0,
         Half_Height => 300.0,
         Is_Trigger  => False
      );
      Add_Component (S.all, E, Renderable'Tag);
      S.Renderable.Data (S.Renderable.Lookup (E)) := (
         Half_Width  => 10.0,
         Half_Height => 300.0,
         Color       => 16#FFFFFF#     -- Walls are grey
      );
      
      -- Bottom boundary: Just for detecting ball loss (heh)
      -- We don't actually create this wall - we check Y position in Is_Ball_Lost
   
   end Load_Level;


   ------------------------------------------------------------
   -- Is_Level_Complete: Check if all bricks destroyed
   ------------------------------------------------------------
   function Is_Level_Complete (
      G : Arkanoid_Game) return Boolean is
      
      S : access Store.Store := G.Manager.Get_Store;
      
      Bricks : Entity_ID_Array_Access :=
         S.Get_Entities_With ((Brick'Tag));
   
   begin
      -- Level complete if no bricks remain
      return Bricks = null or else Bricks'Length = 0;
   end Is_Level_Complete;


   ------------------------------------------------------------
   -- Is_Ball_Lost: Check if ball fell off bottom
   ------------------------------------------------------------
   function Is_Ball_Lost (
      G : Arkanoid_Game
   ) return Boolean is
      S : access Store.Store := G.Manager.Get_Store;
   
   begin
      if G.Ball_Entity = 0 then
         return False;
      end if;
      
      if not S.Has_Entity (G.Ball_Entity) then
         return False;
      end if;
      
      if S.Has_Component (G.Ball_Entity, Transform'Tag) then
         declare
            T_Index : constant Natural := S.Transform.Lookup (G.Ball_Entity);
            T : Transform renames S.Transform.Data (T_Index);
         
         begin
            -- Ball lost if it goes below screen
            return T.Position.Y > Screen_Height + 20.0;
         end;
      end if;
      
      return False;
   end Is_Ball_Lost;

end Arkanoid;
