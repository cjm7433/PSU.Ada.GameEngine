-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces.C;            use Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS;                     use ECS;
with ECS.Store;               --use ECS.Store;
with ECS.Components;           use ECS.Components;
with ECS.Entities;              use ECS.Entities;
with ECS.Manager;             use ECS.Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.Systems;              use ECS.Systems;

with Audio;                   use Audio;
with Math.Linear_Algebra;     use Math.Linear_Algebra;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
--  with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Rendering;      use Graphics.Rendering;
--  with Graphics.Texture_Loader; use Graphics.Texture_Loader;
-- Window interface (platform-agnostic)
with Window;                  use Window;
with Win32;                   use Win32;
-- User defined modules
with Bricks;                  use Bricks;

procedure Arkanoid is

   Width                 : Integer                 := 224;
   Height                : Integer                 := 240;
   Title                 : Unbounded_String        := To_Unbounded_String ("Arkanoid Clone");
   GameWindow            : Window_Access;
   Buffer                : Graphics.Rendering.Byte_Array_Access := new Graphics.Rendering.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;

   -- Entity Manager and Entities
   S : ECS.Store.Store;

   -- Systems

   -- Textures
   bkgrd                : constant String       := "Data/bkgrd.qoi";

   




begin

   -- Used to calculate the frame time
   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (Interfaces.C.int (Width), Interfaces.C.int (Height), Title);
   ECS.Store.Initialize (S);

   declare
      Message : MSG_Access := new MSG;
      Lp_Result : LRESULT;
      Background_Image  : QOI_Image_Data;
      --Texture_Image     : QOI_Image_Data;
      Running           : Boolean := True;
   begin

      Background_Image        := Load_QOI (bkgrd);

      Add_Brick(S, (4.0, 4.0), 2, Red);

         -- Platform-agnostic game loop
      while Running loop
         Stop_Time    := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         Start_Time   := Stop_Time;
         Running := Get_Message (Message, System.Null_Address, 0, 0);
         Lp_Result := Dispatch_Message (Message);
         --  Process platform events (Windows MSG or Wayland events)
         Window.Process_Events;

         --  Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Width), Integer(Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width));
         Draw (Buffer.all, Background_Image.Data, 0, 0, Integer(Width), Integer(Height), 0,0, Width, Height,Natural(Background_Image.Desc.Width), Natural(Background_Image.Desc.Height));

         Draw_Buffer (Buffer.all'Address);
         
         --  Frame rate limiting (~60 FPS)
         delay until Start_Time + Milliseconds(16);
      end loop;
   end;
end Arkanoid;
