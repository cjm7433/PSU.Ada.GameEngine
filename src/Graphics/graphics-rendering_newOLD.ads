with Graphics.Color; use Graphics.Color;
with System.Storage_Elements; use System.Storage_Elements;

package Graphics.Rendering_newOLD is

   --  procedure Bind_Texture (T : QOI_Image_Data);

   -- Byte Array for pixel data (ARGB8888)
   type Byte is mod 2 ** 8 with Size => 8;
   type Byte_Array is array (Natural range <>) of Byte;
   type Byte_Array_Access is access Byte_Array;

   type Storage_Array_Access is access all Storage_Array;

   procedure Set_Pixel_Color
      (Img : in out Byte_Array; 
       X, Y :  Integer;
       C : Graphics.Color.Color; 
       Width, Height : Natural);

   function Get_Pixel_Color
      (Img : in out Byte_Array;
       X, Y : Integer;
       Width, Height : Natural) return Graphics.Color.Color;

   procedure Line
      (X1, Y1 : in out Integer;
       X2, Y2 : in out Integer;
       C : Graphics.Color.Color;
       Img : in out Byte_Array;
       Width, Height : Natural);

   procedure Draw
      (Buffer : in out Byte_Array;
       Img : in out Storage_Array_Access;
       X, Y : Integer;
       Width, Height : Integer;
       Screen_Width, Screen_Height : Natural);

   generic
      type T is private;
   procedure Generic_Swap (X, Y : in out T);

end Graphics.Rendering_newOLD;