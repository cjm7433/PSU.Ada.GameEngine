
package body Graphics.Rendering is
   type Point is record
      X : Integer;
      Y : Integer;
   end record;


   -----------------------------------------------------
   --  Procedure: Generic_Swap 
   -----------------------------------------------------
   procedure Generic_Swap (X, Y : in out T) is
      Tmp : constant T := x;
   begin
      X := Y;
      Y := Tmp;
   end Generic_Swap;


   -----------------------------------------------------
   --  Procedure: Set_Pixel_Color
   --  Inputs: Img - byte array of image data
   --    X - startin x position
   --    Y - starting y position
   --    C - RGB value for pixel color
   --    Width - width of the render window
   --    Height - height of the render window  
   -- Output: None
   -----------------------------------------------------
   procedure Set_Pixel_Color
      (Img : in out Byte_Array;
       X, Y :  Integer;
       C : Graphics.Color.Color;
       Width, Height : Natural) is
   begin
      -- Only set pixel if within bounds and alpha > 0
      if X >= 0 and then X < Width and then Y >= 0 and then Y < Height and then C.A > 0 then
         declare
            Index : constant Natural := (Y * Width + X) * 4;
         begin
            Img (Index) := Byte (C.B);
            Img (Index + 1) := Byte (C.G);
            Img (Index + 2) := Byte (C.R);
            Img (Index + 3) := Byte (C.A);
         end;
      end if;
   end Set_Pixel_Color; 

   
   -----------------------------------------------------
   --  Procedure: Get_Pixel_Color
   -----------------------------------------------------
   function Get_Pixel_Color
      (Img : in out Byte_Array;
       X, Y : Integer;
       Width, Height : Natural) return Graphics.Color.Color is

      Index : constant Natural := ((Y mod Height) * Width + (X mod Width)) * 4;
      C : Graphics.Color.Color;
   begin
      C.B := Color_Int (Img (Index));
      C.G := Color_Int (Img (Index + 1));
      C.R := Color_Int (Img (Index + 2));
      C.B := Color_Int (Img (Index + 3));
      return C;
   end Get_Pixel_Color; 

   -----------------------------------------------------
   --  Procedure: Line
   -----------------------------------------------------
   procedure Line
      (X1, Y1 : in out Integer;
       X2, Y2 : in out Integer;
       C : Graphics.Color.Color;
       Img : in out Byte_Array;
       Width, Height : Natural) is

      procedure Swap is new Generic_Swap (T => Integer);
      Steep : Boolean := False;
      Dx : Integer := X1 - X2;
      Dy : Integer := Y1 - Y2;
      Derror2, Error2, Y : Integer := 0;
   begin
      if abs Dx < abs Dy then
         Swap (X1, Y1);
         Swap (X2, Y2);
         Steep := True;
      end if;

      if X1 > X2 then
         Swap (X1, X2);
         Swap (Y1, Y2);
      end if;

      Dx := X2 - X1;
      Dy := Y2 - Y1;
      Derror2 := abs (Dy) * 2;   -- vs (abs Dy)
      Y := Y1;

      for X in X1 .. X2 loop
         if Steep then
            Set_Pixel_Color (Img, Y, X, C, Width, Height);
         else
            Set_Pixel_Color (Img, X, Y, C, Width, Height);
         end if;
         Error2 := Error2 + Derror2;

         if Error2 > Dx then
            Y := Y + (if Y2 > Y1 then 1 else - 1);
            Error2 := Error2 - Dx * 2;
         end if;
      end loop;

   end Line;


   -----------------------------------------------------
   --  Procedure: Fill_Rect
   --  Added 2025: required to draw solid filled rectangles for
   --    bricks and the paddle in the ECS render system.
   --  Inputs: X, Y  - top-left corner of the rectangle (pixels)
   --          W, H  - width and height of the rectangle (pixels)
   --          C     - fill colour
   --          Img   - byte array of image data (BGRA8888)
   --          Width - width of the render window
   --          Height - height of the render window
   --  Output: None
   --  Note: Pixels that fall outside the buffer bounds are silently
   --    clipped; the caller does not need to guard against overflow.
   -----------------------------------------------------
   procedure Fill_Rect
      (X, Y, W, H : Integer;
       C           : Graphics.Color.Color;
       Img         : in out Byte_Array;
       Width, Height : Natural) is
   begin
      for Row in Y .. Y + H - 1 loop
         for Col in X .. X + W - 1 loop
            Set_Pixel_Color (Img, Col, Row, C, Width, Height);
         end loop;
      end loop;
   end Fill_Rect;


   -----------------------------------------------------
   --  Procedure: Fill_Circle
   --  Added 2025: required to draw the ball as a filled circle
   --    in the ECS render system.
   --  Inputs: CX, CY - centre of the circle (pixels)
   --          Radius  - radius of the circle (pixels)
   --          C       - fill colour
   --          Img     - byte array of image data (BGRA8888)
   --          Width   - width of the render window
   --          Height  - height of the render window
   --  Output: None
   --  Note: Uses a scanline approach derived from the midpoint circle
   --    algorithm. Pixels outside the buffer bounds are silently
   --    clipped via Set_Pixel_Color.
   -----------------------------------------------------
   procedure Fill_Circle
      (CX, CY, Radius : Integer;
       C               : Graphics.Color.Color;
       Img             : in out Byte_Array;
       Width, Height   : Natural) is

      F     : Integer := 1 - Radius;
      Ddx   : Integer := 0;
      Ddy   : Integer := -2 * Radius;
      Px    : Integer := 0;
      Py    : Integer := Radius;

      -- Fills a horizontal span from (CX - Span_X, Row) to (CX + Span_X, Row)
      procedure Fill_Span (Row, Span_X : Integer) is
      begin
         for Col in CX - Span_X .. CX + Span_X loop
            Set_Pixel_Color (Img, Col, Row, C, Width, Height);
         end loop;
      end Fill_Span;

   begin
      -- Fill the top and bottom caps and the horizontal midline
      Fill_Span (CY,          Radius);
      Fill_Span (CY + Radius, 0);
      Fill_Span (CY - Radius, 0);

      while Px < Py loop
         if F >= 0 then
            Py  := Py  - 1;
            Ddy := Ddy + 2;
            F   := F   + Ddy;
         end if;

         Px  := Px  + 1;
         Ddx := Ddx + 2;
         F   := F   + Ddx + 1;

         Fill_Span (CY + Py,  Px);
         Fill_Span (CY - Py,  Px);
         Fill_Span (CY + Px,  Py);
         Fill_Span (CY - Px,  Py);
      end loop;
   end Fill_Circle;


   -----------------------------------------------------
   --  Procedure: Clear_Buffer
   --  Added 2025: required to erase the framebuffer at the start
   --    of each game loop iteration so that objects from the
   --    previous frame do not bleed into the current frame.
   --  Inputs: C      - colour to flood the buffer with
   --          Img    - byte array of image data (BGRA8888)
   --          Width  - width of the render window
   --          Height - height of the render window
   --  Output: None
   -----------------------------------------------------
   procedure Clear_Buffer
      (C             : Graphics.Color.Color;
       Img           : in out Byte_Array;
       Width, Height : Natural) is
   begin
      for Row in 0 .. Height - 1 loop
         for Col in 0 .. Width - 1 loop
            Set_Pixel_Color (Img, Col, Row, C, Width, Height);
         end loop;
      end loop;
   end Clear_Buffer;


   procedure Draw 
      (Buffer : in out Byte_Array;
       Img : in out Storage_Array_Access;
       X, Y : Integer;
       Width, Height : Integer;
       Screen_Width, Screen_Height : Natural) is

      function Blend_Color_Values (A, B, Alpha : Float) return Float is
      begin
         return A * Alpha + B * (1.0 - Alpha);
      end Blend_Color_Values;

   begin
      for I in 0 .. (Height - 1) loop
         begin
            for J in 0 .. (Width - 1) loop
               declare
                  Img_Index : Natural := (I * Width + J) * 4 + 1;
                  --  Need to offset the buffer index by the x and y values
                  Buffer_Index : Natural := ((Y mod Screen_Height + I) * Screen_Width + (X mod Screen_Width + J)) * 4;

                  New_Red_Value, New_Green_Value, New_Blue_Value, New_Alpha_Value : Float;
                  Orig_Red_Value, Orig_Green_Value, Orig_Blue_Value : Float;
                  Blend_Red, Blend_Green, Blend_Blue : Float;

               begin
                  if X + J < Screen_Width and then Y + I < Screen_Height then
                     Orig_Red_Value := Float (Buffer (Buffer_Index + 2));
                     Orig_Green_Value := Float (Buffer (Buffer_Index + 1));
                     Orig_Blue_Value := Float (Buffer (Buffer_Index));

                     New_Red_Value := Float (Img.all (Storage_Offset (Img_Index)));
                     New_Green_Value := Float (Img.all (Storage_Offset (Img_Index + 1)));
                     New_Blue_Value := Float (Img.all (Storage_Offset (Img_Index + 2)));
                     New_Alpha_Value := Float (Img.all (Storage_Offset (Img_Index + 3))) / 255.0;


                     Blend_Red := Blend_Color_Values (New_Red_Value, Orig_Red_Value, New_Alpha_Value);
                     Blend_Green := Blend_Color_Values (New_Green_Value, Orig_Green_Value, New_Alpha_Value);
                     Blend_Blue := Blend_Color_Values (New_Blue_Value, Orig_Blue_Value, New_Alpha_Value);

                     Buffer (Buffer_Index) := Byte (Blend_Blue);
                     Buffer (Buffer_Index + 1) := Byte (Blend_Green);
                     Buffer (Buffer_Index + 2) := Byte (Blend_Red);
                     Buffer (Buffer_Index + 3) := Byte (255);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Draw;

   procedure Draw_String (
      Img                           : in out Byte_Array;
      X, Y                           : Integer;
      Width, Height                 : Integer;
      S                             : String;
      Color                         : Graphics.Color.Color;
      Screen_Width, Screen_Height   : Natural
      )
   is
      StartX   : Integer := X;
      StartY   : Integer := Y;
      Char     : Character;
      Char_Spacing : constant Natural := 10;
   begin
      for I in 1 .. S'Length loop
         Char := S (I);
         Draw_Character (Img, StartX, StartY, Width, Height, Char, Color, Screen_Width, Screen_Height);
         StartX := StartX + Char_Spacing;
      end loop;
   end Draw_String;

   procedure Draw_Character (
      Img                           : in out Byte_Array;
      X, Y, Width, Height           : Integer;
      Char                          : Character;
      Color                         : Graphics.Color.Color;
      Screen_Width, Screen_Height   : Natural
      )
   is
      C : Text_Array := Get_Character (Char);
      StartX : Integer := X;
      StartY : Integer := Y;
   begin
      for I in 0 .. C'Length - 1 loop
         declare
            Bits : Graphics.Text.Text := C (I);
         begin
            for J in reverse 0 .. 7 loop
            --  Print the most significant bit
               declare
                  Bit : Integer := Integer ((Bits / (2**J)) and 1);
               begin
                  if Bit = 1 then
                     Set_Pixel_Color (Img, StartX, StartY, Color, Screen_Width, Screen_Height); -- Print the pixel and move to the next position
                     StartX := StartX + 1;
                  else
                     StartX := StartX + 1;
                  end if;
               end;
            end loop;
            StartY := StartY + 1; -- New line
            StartX := X;          -- Reset cursor
         end;
      end loop;
   end Draw_Character;

end Graphics.Rendering;
