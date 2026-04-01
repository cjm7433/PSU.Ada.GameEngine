
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
      Index : constant Natural := (Y * Width + X) * 4;
   begin
      if (X >= 0 and then X < Width) and then (Y >= 0 and then
         Y < Height) and then (C.A > 0) then
         Img (Index) := Byte (C.B);
         Img (Index + 1) := Byte (C.G);
         Img (Index + 2) := Byte (C.R);
         Img (Index + 3) := Byte (C.A);
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
      Dx := Y2 - Y1;
      Derror2 := (abs Dy) * 2;
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

   procedure Draw
      (Buffer : in out Byte_Array;
       Img : in out Storage_Array_Access;
       X, Y : Integer;
       Width, Height : Integer;
       Start_X, Start_Y : Integer;
       Screen_Width, Screen_Height : Natural;
       Img_Width, Img_Height : Natural) is

      function Blend_Color_Values (A, B, Alpha : Float) return Float is
      begin
         return A * Alpha + B * (1.0 - Alpha);
      end Blend_Color_Values;
      Img_Channel_Offset : constant Natural := 1;

   begin
      for I in 0 .. (Height - 1) loop
         begin
            for J in 0 .. (Width - 1) loop
               declare
                  Screen_X : constant Integer := X + J;
                  Screen_Y : constant Integer := Y + I;
                  Source_X : constant Integer := Start_X + J;
                  Source_Y : constant Integer := Start_Y + I;
                  New_Red_Value, new_Green_Value, New_Blue_Value, New_Alpha_Value : Float;
                  Orig_Red_Value, Orig_Green_Value, Orig_Blue_Value : Float;
                  Blend_Red, Blend_Green, Blend_Blue : Float;

               begin
                  if (Screen_X >= 0 and then Screen_X < Integer (Screen_Width))
                    and then (Screen_Y >= 0 and then Screen_Y < Integer (Screen_Height))
                    and then (Source_X >= 0 and then Source_X < Integer (Img_Width))
                    and then (Source_Y >= 0 and then Source_Y < Integer (Img_Height))
                  then
                     declare
                        Img_Index : constant Natural :=
                          Natural ((Source_Y * Integer (Img_Width) + Source_X) * 4 + Integer (Img_Channel_Offset));
                        -- Need to offset the buffer index by the x and y values
                        Buffer_Index : constant Natural :=
                          Natural ((Screen_Y * Integer (Screen_Width) + Screen_X) * 4);
                     begin
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
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Draw;
end Graphics.Rendering;