with Interfaces;
with Interfaces.C;
with Win32; use Win32;
with Graphics.Color; use Graphics.Color;
with GameMath; use GameMath;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
package Graphics.Renderer is

  type Storage_Array_Access is access all Storage_Array;

   type bool is new boolean;
   for bool'size use 8;

   subtype Max_String_Length is Natural range 1 .. 255;
   type Char_Array is array (Max_String_Length) of Character;

   procedure Clear_Screen
      (Img : in out Byte_Array; C : Graphics.Color.Color; Screen_Width : Natural; Screen_Height : Natural);

   procedure set_pixel_color
     (Img : in out Byte_Array; X : Integer; Y : Integer; C : Graphics.Color.Color; Screen_Width : Natural; Screen_Height : Natural);

   function Get_Pixel_Color (Img : in out Byte_Array; X, Y : Integer; Screen_Width, Screen_Height : Natural) return Graphics.Color.Color;

   procedure line
     (X0  : in out Integer;
      Y0  : in out Integer;
      X1  : in out Integer;
      Y1  : in out Integer;
      C   : Graphics.Color.Color;
      Img : in out Byte_Array;
      Screen_Width : Natural;
      Screen_Height : Natural);

   procedure Draw_Regular_Polygon
     (Img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : Float;
      Center_Y : Float;
      C        : Graphics.Color.Color;
      Screen_Width    : Natural;
      Screen_Height   : Natural);

   procedure Draw_Filled_Triangle(Img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_Filled_Quad(Img : in out Byte_Array; X,Y,Width,Height : Float; C : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_Character(Img : in out Byte_Array; X,Y,Width,Height : Integer; Char : Character; color : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_String(Img : in out Byte_Array; X,Y : Integer; Width, Height : Integer; S : in String; Color : Graphics.Color.Color; Screen_Width, Screen_Height : Natural);
   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; Img: in out Storage_Array_Access; X,Y,Width,Height : Integer; Screen_Width, Screen_Height : Natural);
   procedure Draw_Image_To_Buffer(buffer : in out Byte_Array; Img: in out Storage_Array_Access; X,Y,Width,Height,StartX,StartY : Integer; Screen_Width, Screen_Height,Image_Width : Natural);


   generic
      type T is private;
   procedure generic_swap (X, Y : in out T);

end Graphics.Renderer;
