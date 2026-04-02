-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
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
-- Window interface (platform-agnostic)
with Window;                  use Window;
with Win32;                   use Win32;

procedure Arkanoid is

   Width        : constant Integer := 224;
   Height       : constant Integer := 240;
   Title        : Unbounded_String := To_Unbounded_String ("Arkanoid Clone");
   GameWindow   : Window_Access;
   Buffer       : Graphics.Rendering.Byte_Array_Access :=
                     new Graphics.Rendering.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;

   -- ECS store: holds all entities and their component data
   S : ECS.Store.Store;

   -- -----------------------------------------------------------------------
   -- Paddle layout constants
   -- -----------------------------------------------------------------------
   Paddle_W       : constant Integer := 40;
   Paddle_H       : constant Integer := 6;
   Paddle_Start_X : constant Float   := 112.0;   -- horizontal centre
   Paddle_Start_Y : constant Float   := 228.0;   -- near bottom

   -- -----------------------------------------------------------------------
   -- Ball layout constants
   -- Ball AABB is a square with half-extents equal to the radius
   -- -----------------------------------------------------------------------
   Ball_Radius    : constant Integer := 4;
   Ball_Start_X   : constant Float   := 112.0;
   Ball_Start_Y   : constant Float   := 222.0;   -- just above paddle

   -- -----------------------------------------------------------------------
   -- Brick grid layout constants
   -- -----------------------------------------------------------------------
   Brick_Cols     : constant Integer := 8;
   Brick_Rows     : constant Integer := 6;
   Brick_W        : constant Integer := 22;
   Brick_H        : constant Integer := 8;
   Brick_Gap      : constant Integer := 2;
   Brick_Origin_X : constant Integer := 10;   -- left margin (pixels)
   Brick_Origin_Y : constant Integer := 32;   -- top margin  (pixels)

begin

   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (Interfaces.C.int (Width), Interfaces.C.int (Height), Title);
   ECS.Store.Initialize (S);

   declare
      Message   : MSG_Access := new MSG;
      Lp_Result : LRESULT;
      Running   : Boolean := True;
      Paddle_E  : Entity_ID;                    -- these are pointers
      Ball_E    : Entity_ID;
      Brick_E   : Entity_ID;
   begin

      -- =====================================================================
      -- Create Paddle entity
      -- Components: Transform, Motion, Collider, Render, Paddle
      -- =====================================================================
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
         (R => 0.5, G => 0.5, B => 0.5, A => 1.0);   -- Grey
      S.Render.Data (S.Render.Lookup (Paddle_E)).Layer   := 1;
      S.Render.Data (S.Render.Lookup (Paddle_E)).Visible := True;

      S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Min_X :=
         Float (Paddle_W / 2);
      S.Paddle.Data (S.Paddle.Lookup (Paddle_E)).Max_X :=
         Float (Width - Paddle_W / 2);

      -- =====================================================================
      -- Create Ball entity
      -- Components: Transform, Motion, Collider, Render, Ball
      -- =====================================================================
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

      S.Ball.Data (S.Ball.Lookup (Ball_E)).Is_Attached     := False;
      S.Ball.Data (S.Ball.Lookup (Ball_E)).Attach_Offset_X := 0.0;

      -- =====================================================================
      -- Create Brick entities
      -- 8 columns x 6 rows; each row has a distinct color (red is stronger).
      --
      -- Row colors and health (top to bottom):
      --   Row 0 - Red,    Strong (2 hp)
      --   Row 1 - Orange, Normal (1 hp)
      --   Row 2 - Yellow, Normal (1 hp)
      --   Row 3 - Green,  Normal (1 hp)
      --   Row 4 - Cyan,   Normal (1 hp)
      --   Row 5 - Blue,   Normal (1 hp)
      -- =====================================================================
      for Row in 0 .. Brick_Rows - 1 loop
         for Col in 0 .. Brick_Cols - 1 loop

            Brick_E := Create_Entity (S);

            Add_Component (S, Brick_E, Transform_Component'Tag);
            Add_Component (S, Brick_E, Collider_Component'Tag);
            Add_Component (S, Brick_E, Render_Component'Tag);
            Add_Component (S, Brick_E, Brick_Component'Tag);

            declare
               -- Pixel position of this brick's top-left corner
               BX : constant Integer :=
                  Brick_Origin_X + Col * (Brick_W + Brick_Gap);
               BY : constant Integer :=
                  Brick_Origin_Y + Row * (Brick_H + Brick_Gap);

               -- Centre in world space (Transform and AABB both use centre)
               CX : constant Float := Float (BX) + Float (Brick_W) / 2.0;
               CY : constant Float := Float (BY) + Float (Brick_H) / 2.0;

               -- Per-row tint stored as ECS Render.Color (0.0 - 1.0 floats)     -- TODO: this is janky but ok for now
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
         -- Render pass
         -- 1. Clear the framebuffer to black each frame.
         -- 2. Walk every entity that has both a Transform and a Render
         --    component and draw it as a filled rectangle.
         --    ECS Render.Color (0.0-1.0) is scaled to
         --    Graphics.Color.Color (0-255) at draw time.
         -- -----------------------------------------------------------------

         -- 1. Clear
         Graphics.Rendering.Clear_Buffer
            (C      => Black,
             Img    => Buffer.all,
             Width  => Width,
             Height => Height);

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

                     -- Scale ECS Render.Color (0.0-1.0) to                   -- TODO: Can this conversion be avoided?
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
                                 -- Fill the rectangle
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
                                 -- Fill the circle
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

         Draw_Buffer (Buffer.all'Address);

         -- Frame rate limiting (~60 FPS)
         delay until Start_Time + Milliseconds (16);
      end loop;

   end;
end Arkanoid;
