-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Numerics;            use Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Interfaces.C;            use Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS;                     use ECS;
with ECS.Store;               use ECS.Store;
with ECS.Components;          use ECS.Components;
with ECS.Entities;            use ECS.Entities;
with ECS.Manager;             use ECS.Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.Systems;             use ECS.Systems;
with ECS.Systems_Movement;    use ECS.Systems_Movement;
with ECS.Systems_Ball_Physics; use ECS.Systems_Ball_Physics;
with ECS.Systems_Collision;   use ECS.Systems_Collision;
with ECS.Systems_Brick_Destruction; use ECS.Systems_Brick_Destruction;
with ECS.Systems_Paddle_Control; use ECS.Systems_Paddle_Control;
-- Component types (needed for direct field access through Store)
with ECS.Components.Transform; use ECS.Components.Transform;
with ECS.Components.Motion;    use ECS.Components.Motion;
with ECS.Components.Collider;  use ECS.Components.Collider;
with ECS.Components.Render;    use ECS.Components.Render;
with ECS.Components.Paddle;    use ECS.Components.Paddle;
with ECS.Components.Ball;      use ECS.Components.Ball;
with ECS.Components.Brick;     use ECS.Components.Brick;

with Audio;                   use Audio;
with Math.Linear_Algebra;     use Math.Linear_Algebra;
with Math.Physics.AABBs;      use Math.Physics.AABBs;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Rendering;      use Graphics.Rendering;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;

-- Window interface (platform-agnostic)
with Window;                  use Window;
with Win32;                   use Win32;
-- Game-side input module
with Input;

procedure Arkanoid is

   Width        : constant Integer := 224;
   Height       : constant Integer := 240;
   Title        : Unbounded_String := To_Unbounded_String ("Arkanoid Clone");
   GameWindow   : Window_Access;
   Buffer       : Graphics.Rendering.Byte_Array_Access :=
                     new Graphics.Rendering.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;
   
   -- Background Image (QOI)
   Bkgrnd : constant String := "Data/bkgrd.qoi";

   -- Target frame rate and delta time
   Target_FPS : constant Float := 60.0;
   DT         : constant Float := 1.0/ Target_FPS;

   -- ECS Manager with registered systems
   Manager : aliased ECS.Manager.ECS_Manager;

   -- Random number generator for ball-launch angle
   Gen : Generator;

   -- -----------------------------------------------------------------------
   -- Paddle layout constants
   -- -----------------------------------------------------------------------
   Paddle_W       : constant Integer := 40;
   Paddle_H       : constant Integer := 2;
   Paddle_Start_X : constant Float   := 112.0;   -- horizontal centre
   Paddle_Start_Y : constant Float   := 228.0;   -- near bottom

   -- -----------------------------------------------------------------------
   -- Ball layout constants
   -- Ball AABB is a square with half-extents equal to the radius.
   -- Ball starts attached to the paddle; press Space to launch upward.
   -- -----------------------------------------------------------------------
   Ball_Radius    : constant Integer := 3;         -- adjust for ball size
   Ball_Start_X   : constant Float   := 112.0;
   Ball_Start_Y   : constant Float   := 210.0;     -- just above paddle, attached (this is a little magic-numberish)

   -- -----------------------------------------------------------------------
   -- Wall layout constants
   -- 4 px thick; left/right span full height, top spans full width.
   -- Positions are centres of each wall rectangle.
   -- -----------------------------------------------------------------------
   Wall_Thickness : constant Integer := 4;
   Wall_Half      : constant Float   := Float (Wall_Thickness) / 2.0;

   Wall_Left_X    : constant Float   := Wall_Half;
   Wall_Right_X   : constant Float   := Float (Width)  - Wall_Half;
   Wall_Top_Y     : constant Float   := Wall_Half;

   -- -----------------------------------------------------------------------
   -- Brick grid layout constants
   -- -----------------------------------------------------------------------
   Brick_Cols     : constant Integer := 8;
   Brick_Rows     : constant Integer := 6;
   Brick_W        : constant Integer := 24;  -- widen bricks for better visual balance
   Brick_H        : constant Integer := 8;
   Brick_Gap      : constant Integer := 2;
   Brick_Origin_X : constant Integer := 9;   -- centered horizontally with Brick_W=24
   Brick_Origin_Y : constant Integer := 32;   -- top margin  (pixels)


begin

   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (Interfaces.C.int (Width), Interfaces.C.int (Height), Title);

   -- Welcome message on startup
   New_Line;
   New_Line;
   Put_Line ("#######################################");
   Put_Line ("Welcome to Arkanoid!");
   Put_Line ("Controls:");
   Put_Line ("   A/D = Move paddle");
   Put_Line ("   Space = Launch ball");
   Put_Line ("   Escape = Reset game");
   Put_Line ("#######################################");
   New_Line;

   ECS.Manager.Initialize (Manager);

   -- Initialize random generator
   Reset (Gen);

   declare
      S : ECS.Store.Store renames Manager.World;

      -- Entity IDs are updated by Reset_World and referenced by the game loop
      Paddle_E  : Entity_ID;
      Ball_E    : Entity_ID;
      Brick_E   : Entity_ID;
      Wall_E    : Entity_ID;

      Message   : MSG_Access := new MSG;
      Lp_Result : LRESULT;
      Running   : Boolean := True;
      Background_Image : QOI_Image_Data;

      -- =====================================================================
      -- Reset_World
      -- Destroys all current entities and rebuilds the full initial scene:
      -- walls, paddle, ball (attached), and the brick grid.
      -- Called once at startup and again whenever Escape is pressed.
      -- =====================================================================
      procedure Reset_World is
      begin
         -- Clear the store so all previous entities are gone
         ECS.Store.Initialize (S);
         Reset_Score;
         Input.Reset;

         -- ==================================================================
         -- Create Wall entities (left, right, top)
         -- Components: Transform, Collider, Render
         -- Grey filled rectangles; collidable with the ball.
         -- ==================================================================
         declare
            Grey : constant ECS.Components.Render.Color :=
               (R => 0.55, G => 0.55, B => 0.55, A => 1.0);

            -- Helper to configure a single wall given its centre/half-extents
            procedure Make_Wall
               (E  : Entity_ID;
                CX : Float; CY : Float;
                HW : Float; HH : Float) is
            begin
               S.Transform.Data (S.Transform.Lookup (E)).Position :=
                  (X => CX, Y => CY);

               S.Collider.Data (S.Collider.Lookup (E)).Bounding_Box :=
                  (Center    => (X => CX, Y => CY),
                   Half_Size => (X => HW, Y => HH));
               S.Collider.Data (S.Collider.Lookup (E)).Layer :=
                  Layer_Wall;
               S.Collider.Data (S.Collider.Lookup (E)).Mask :=
                  (Layer_Ball, Layer_None, Layer_None, Layer_None);
               S.Collider.Data (S.Collider.Lookup (E)).Collider_Form :=
                  Solid;

               S.Render.Data (S.Render.Lookup (E)).Shape   := Rectangle;
               S.Render.Data (S.Render.Lookup (E)).Tint    := Grey;
               S.Render.Data (S.Render.Lookup (E)).Layer   := 0;
               S.Render.Data (S.Render.Lookup (E)).Visible := True;
            end Make_Wall;

         begin
            -- Left wall
            Wall_E := Create_Entity (S);
            Add_Component (S, Wall_E, Transform_Component'Tag);
            Add_Component (S, Wall_E, Collider_Component'Tag);
            Add_Component (S, Wall_E, Render_Component'Tag);
            Make_Wall (Wall_E,
               CX => Wall_Left_X,
               CY => Float (Height) / 2.0,
               HW => Wall_Half,
               HH => Float (Height) / 2.0);

            -- Right wall
            Wall_E := Create_Entity (S);
            Add_Component (S, Wall_E, Transform_Component'Tag);
            Add_Component (S, Wall_E, Collider_Component'Tag);
            Add_Component (S, Wall_E, Render_Component'Tag);
            Make_Wall (Wall_E,
               CX => Wall_Right_X,
               CY => Float (Height) / 2.0,
               HW => Wall_Half,
               HH => Float (Height) / 2.0);

            -- Top wall
            Wall_E := Create_Entity (S);
            Add_Component (S, Wall_E, Transform_Component'Tag);
            Add_Component (S, Wall_E, Collider_Component'Tag);
            Add_Component (S, Wall_E, Render_Component'Tag);
            Make_Wall (Wall_E,
               CX => Float (Width) / 2.0,
               CY => Wall_Top_Y,
               HW => Float (Width) / 2.0,
               HH => Wall_Half);
         end;

         -- ==================================================================
         -- Create Paddle entity
         -- Components: Transform, Motion, Collider, Render, Paddle
         -- ==================================================================
         Paddle_E := Create_Entity (S);

         Add_Component (S, Paddle_E, Transform_Component'Tag);
         Add_Component (S, Paddle_E, Motion_Component'Tag);
         Add_Component (S, Paddle_E, Collider_Component'Tag);
         Add_Component (S, Paddle_E, Render_Component'Tag);
         Add_Component (S, Paddle_E, Paddle_Component'Tag);

         S.Transform.Data (S.Transform.Lookup (Paddle_E)).Position :=
            (X => Paddle_Start_X, Y => Paddle_Start_Y);

         S.Collider.Data (S.Collider.Lookup (Paddle_E)).Bounding_Box :=
            (Center    => (X => Paddle_Start_X, Y => Paddle_Start_Y),
             Half_Size => (X => Float (Paddle_W / 2),
                           Y => Float (Paddle_H / 2)));
         S.Collider.Data (S.Collider.Lookup (Paddle_E)).Layer :=
            Layer_Paddle;
         S.Collider.Data (S.Collider.Lookup (Paddle_E)).Mask :=
            (Layer_Ball, Layer_Wall, Layer_None, Layer_None);
         S.Collider.Data (S.Collider.Lookup (Paddle_E)).Collider_Form :=
            Solid;

         S.Render.Data (S.Render.Lookup (Paddle_E)).Shape   := Rectangle;
         S.Render.Data (S.Render.Lookup (Paddle_E)).Tint    :=
            (R => 1.0, G => 1.0, B => 1.0, A => 1.0);   -- White
         S.Render.Data (S.Render.Lookup (Paddle_E)).Layer   := 1;
         S.Render.Data (S.Render.Lookup (Paddle_E)).Visible := True;

         S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Min_X :=
            Float (Paddle_W / 2);
         S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Max_X :=
            Float (Width - Paddle_W / 2);

         -- Home_Y is the fixed row the paddle lives on.  Paddle_Control_System
         -- clamps T.Position.Y to this value every frame to prevent the paddle
         -- from drifting downward due to collision resolution.
         S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Home_Y := Paddle_Start_Y;

         -- ==================================================================
         -- Create Ball entity
         -- Components: Transform, Motion, Collider, Render, Ball
         -- Ball starts attached to the paddle; press Space to launch upward.
         -- ==================================================================
         Ball_E := Create_Entity (S);

         Add_Component (S, Ball_E, Transform_Component'Tag);
         Add_Component (S, Ball_E, Motion_Component'Tag);
         Add_Component (S, Ball_E, Collider_Component'Tag);
         Add_Component (S, Ball_E, Render_Component'Tag);
         Add_Component (S, Ball_E, Ball_Component'Tag);

         S.Transform.Data (S.Transform.Lookup (Ball_E)).Position :=
            (X => Ball_Start_X, Y => Ball_Start_Y);

         S.Collider.Data (S.Collider.Lookup (Ball_E)).Bounding_Box :=
            (Center    => (X => Ball_Start_X, Y => Ball_Start_Y),
             Half_Size => (X => Float (Ball_Radius),
                           Y => Float (Ball_Radius)));
         S.Collider.Data (S.Collider.Lookup (Ball_E)).Layer :=
            Layer_Ball;
         S.Collider.Data (S.Collider.Lookup (Ball_E)).Mask :=
            (Layer_Paddle, Layer_Brick, Layer_Wall, Layer_None);
         S.Collider.Data (S.Collider.Lookup (Ball_E)).Collider_Form :=
            Solid;

         S.Render.Data (S.Render.Lookup (Ball_E)).Shape   := Circle;
         S.Render.Data (S.Render.Lookup (Ball_E)).Tint    :=
            (R => 1.0, G => 1.0, B => 1.0, A => 1.0);   -- White
         S.Render.Data (S.Render.Lookup (Ball_E)).Layer   := 1;
         S.Render.Data (S.Render.Lookup (Ball_E)).Visible := True;

         -- Ball starts attached; velocity is zero until Space is pressed
         S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached     := True;
         S.Ball.Data (S.Ball.Lookup (Ball_E)).Attach_Offset_X := 0.0;
         S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity :=
            (X => 0.0, Y => 0.0);

         -- ==================================================================
         -- Create Brick entities
         -- 8 columns x 6 rows; each row has a distinct color and strength.
         --
         -- Row colors and health (top to bottom):
         --   Row 0 - Red,    Strong (2 hp)
         --   Row 1 - Orange, Normal (1 hp)
         --   Row 2 - Yellow, Normal (1 hp)
         --   Row 3 - Green,  Normal (1 hp)
         --   Row 4 - Cyan,   Normal (1 hp)
         --   Row 5 - Blue,   Normal (1 hp)
         -- ==================================================================
         for Row in 0 .. Brick_Rows - 1 loop
            for Col in 0 .. Brick_Cols - 1 loop

               Brick_E := Create_Entity (S);

               Add_Component (S, Brick_E, Transform_Component'Tag);
               Add_Component (S, Brick_E, Collider_Component'Tag);
               Add_Component (S, Brick_E, Render_Component'Tag);
               Add_Component (S, Brick_E, Brick_Component'Tag);

               declare
                  BX : constant Integer :=
                     Brick_Origin_X + Col * (Brick_W + Brick_Gap);
                  BY : constant Integer :=
                     Brick_Origin_Y + Row * (Brick_H + Brick_Gap);

                  CX : constant Float := Float (BX) + Float (Brick_W) / 2.0;
                  CY : constant Float := Float (BY) + Float (Brick_H) / 2.0;

                  Tint : ECS.Components.Render.Color :=
                     (case Row is
                        when 0 => (R => 1.0,  G => 0.16, B => 0.16, A => 1.0), -- Red
                        when 1 => (R => 1.0,  G => 0.55, B => 0.0,  A => 1.0), -- Orange
                        when 2 => (R => 1.0,  G => 0.94, B => 0.0,  A => 1.0), -- Yellow
                        when 3 => (R => 0.20, G => 0.86, B => 0.20, A => 1.0), -- Green
                        when 4 => (R => 0.0,  G => 0.86, B => 0.86, A => 1.0), -- Cyan
                        when 5 => (R => 0.24, G => 0.39, B => 1.0,  A => 1.0), -- Blue
                        when others => (R => 1.0, G => 1.0, B => 1.0, A => 1.0));

                  Kind   : constant Brick_Type :=
                     (if Row = 0 then Strong else Normal);
                  Health : constant Natural :=
                     (if Row = 0 then 2 else 1);

               begin
                  S.Transform.Data (S.Transform.Lookup (Brick_E)).Position :=
                     (X => CX, Y => CY);

                  S.Collider.Data (S.Collider.Lookup (Brick_E)).Bounding_Box :=
                     (Center    => (X => CX, Y => CY),
                      Half_Size => (X => Float (Brick_W) / 2.0,
                                    Y => Float (Brick_H) / 2.0));
                  S.Collider.Data (S.Collider.Lookup (Brick_E)).Layer :=
                     Layer_Brick;
                  S.Collider.Data (S.Collider.Lookup (Brick_E)).Mask :=
                     (Layer_Ball, Layer_None, Layer_None, Layer_None);
                  S.Collider.Data (S.Collider.Lookup (Brick_E)).Collider_Form :=
                     Solid;

                  S.Render.Data (S.Render.Lookup (Brick_E)).Shape   := Rectangle;
                  S.Render.Data (S.Render.Lookup (Brick_E)).Tint    := Tint;
                  S.Render.Data (S.Render.Lookup (Brick_E)).Layer   := 1;
                  S.Render.Data (S.Render.Lookup (Brick_E)).Visible := True;

                  S.Brick.Data (S.Brick.Lookup (Brick_E)).Brick_Kind := Kind;
                  S.Brick.Data (S.Brick.Lookup (Brick_E)).Health     := Health;
                  S.Brick.Data (S.Brick.Lookup (Brick_E)).Max_Health := Health;
                  S.Brick.Data (S.Brick.Lookup (Brick_E)).Points     :=
                     (if Row = 0 then 20 else 10);
               end;

            end loop;
         end loop;

      end Reset_World;

   begin
      Audio.Initialize;
      -- Register systems in execution order.
      -- Systems are allocated on the heap so they persist across resets.
      ECS.Manager.Add_System (Manager, new Movement_System);
      ECS.Manager.Add_System (Manager, new Ball_Physics_System);
      ECS.Manager.Add_System (Manager, new Collision_System);
      ECS.Manager.Add_System (Manager, new Brick_Destruction_System);
      ECS.Manager.Add_System (Manager, new Paddle_Control_System);

      -- Build the initial scene
      Reset_World;
      Background_Image := Load_QOI (Bkgrnd);
      Play_Audio("sfx/ost.wav");

      -- =====================================================================
      -- Game loop
      -- =====================================================================
      while Running loop
         Stop_Time    := Clock;
         Elapsed_Time := To_Duration (Stop_Time - Start_Time);
         Start_Time   := Stop_Time;
         Running   := Get_Message (Message, System.Null_Address, 0, 0);
         Lp_Result := Dispatch_Message (Message);

         -- Process platform events (Windows MSG or Wayland events)
         Window.Process_Events;

         -- -----------------------------------------------------------------
         -- Input pass
         -- Drain the engine event queue and update Input.State each frame.
         -- -----------------------------------------------------------------
         declare
            Ev      : ECS.Event.Event_T;
            Handler : ECS.Event_Manager.Platform_Event_Handler;
         begin
            loop
               Ev := ECS.Event_Manager.Get_Next_Event (Handler);
               exit when Ev.EventType = ECS.Event.NoEvent;
               Input.Handle_Event (Ev);
            end loop;
         end;

         -- -----------------------------------------------------------------
         -- Game logic: apply input state to ECS components
         -- -----------------------------------------------------------------

         -- Left / right arrow: drive paddle Move_Left / Move_Right flags
         S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Move_Left  :=
            Input.State.Left;
         S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Move_Right :=
            Input.State.Right;

         -- Space: launch the ball if it is still attached to the paddle
         if Input.State.Space then
            if S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached then
               S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached := False;
               -- Launch at a random diagonal angle (between -30 and +30 degrees)
               declare
                  Base_Speed : constant Float := S.Ball.Data (S.Ball.Lookup (Ball_E)).Base_Speed;
                  Angle : constant Float := Random (Gen) * (Pi / 3.0) - (Pi / 6.0);  -- -30 to +30 degrees
               begin
                  S.Motion.Data (S.Motion.Lookup (Ball_E)).Linear_Velocity :=
                     (X => Base_Speed * Sin (Angle),
                      Y => -Base_Speed * Cos (Angle));
               end;
            end if;
         end if;

         -- Escape: reset the entire scene to its initial state
         if Input.State.Escape then
            Reset_World;
         end if;

         -- Clear one-shot flags now that game logic has consumed them
         Input.Clear_Frame_Flags;

         -- -----------------------------------------------------------------
         -- Update ECS systems
         -- -----------------------------------------------------------------
         ECS.Manager.Update (Manager, DT);

         -- -----------------------------------------------------------------
         -- Render pass
         -- 1. Clear the framebuffer to black each frame.
         -- 2. Walk every entity that has both a Transform and a Render
         --    component and draw it according to its Shape.
         --    ECS Render.Color (0.0-1.0) is scaled to
         --    Graphics.Color.Color (0-255) at draw time.
         -- -----------------------------------------------------------------

         -- 1. Clear
         Graphics.Rendering.Clear_Buffer
            (C      => Black,
             Img    => Buffer.all,
             Width  => Width,
             Height => Height);

         -- Draw Background
         Draw (Buffer.all, Background_Image.Data, 0, 0, Integer(Width), Integer(Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));

         -- 2. Draw each visible entity
         declare
            Tags     : constant Component_Tag_Array :=
               (Transform_Component'Tag, Render_Component'Tag);
            Entities : Entity_ID_Array_Access :=
               ECS.Store.Get_Entities_With (S, Tags);
         begin
            if Entities /= null then
               for I in Entities'Range loop
                  declare
                     E : constant Entity_ID := Entities (I);

                     T : constant Transform_Component :=
                        S.Transform.Data (S.Transform.Lookup (E));
                     R : constant Render_Component :=
                        S.Render.Data (S.Render.Lookup (E));

                     -- Scale ECS Render.Color (0.0-1.0) to
                     -- Graphics.Color.Color (0-255) for the drawing calls
                     C : constant Graphics.Color.Color :=
                        (R => Color_Int (Integer (R.Tint.R * 255.0)),
                         G => Color_Int (Integer (R.Tint.G * 255.0)),
                         B => Color_Int (Integer (R.Tint.B * 255.0)),
                         A => Color_Int (Integer (R.Tint.A * 255.0)));

                     -- Centre position from Transform
                     PX : constant Integer := Integer (T.Position.X);
                     PY : constant Integer := Integer (T.Position.Y);

                  begin
                     if R.Visible then
                        declare
                           Box : constant AABB :=
                              S.Collider.Data
                                 (S.Collider.Lookup (E)).Bounding_Box;
                        begin
                           case R.Shape is

                              when Rectangle =>
                                 declare
                                    HW : constant Integer :=
                                       Integer (Box.Half_Size.X);
                                    HH : constant Integer :=
                                       Integer (Box.Half_Size.Y);
                                 begin
                                    Graphics.Rendering.Fill_Rect
                                       (X      => PX - HW,
                                        Y      => PY - HH,
                                        W      => HW * 2,
                                        H      => HH * 2,
                                        C      => C,
                                        Img    => Buffer.all,
                                        Width  => Width,
                                        Height => Height);
                                 end;

                              when Circle =>
                                 -- Use Half_Size.X as the radius
                                 Graphics.Rendering.Fill_Circle
                                    (CX     => PX,
                                     CY     => PY,
                                     Radius => Integer (Box.Half_Size.X),
                                     C      => C,
                                     Img    => Buffer.all,
                                     Width  => Width,
                                     Height => Height);

                              when Sprite =>
                                 -- TODO: sprite rendering not yet implemented
                                 null;

                           end case;
                        end;
                     end if;
                  end;
               end loop;
            end if;
         end;
         Audio.Update;
             Draw_String (Buffer.all, 10, 10, 0, 0, "SCORE: " & Trim (Integer'Image (Get_Score), Left),
                  (255, 255, 255, 255), Width, Height);
         Draw_Buffer (Buffer.all'Address);

         -- Frame rate limiting (~60 FPS)
         delay until Start_Time + Milliseconds (16);
      end loop;
      Audio.Finalize;
   end;
end Arkanoid;
