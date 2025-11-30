--  ============================================================
--  Wayland Window - Main Program (Using C Wrapper)
--  ============================================================
--  This program creates a window using the Wayland protocol
--  via a C wrapper that handles the low-level details.
--  ============================================================

with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Wayland_Wrapper;

procedure Wayland_Window is

   use Interfaces.C;
   use type System.Address;
   use Wayland_Wrapper;

   --  Window dimensions
   Width  : constant := 480;
   Height : constant := 360;

   --  Window context
   Window : Window_Context;

   --  Window title
   Title : Interfaces.C.Strings.chars_ptr;

begin
   Ada.Text_IO.Put_Line ("Wayland Window Example in Ada");
   Ada.Text_IO.Put_Line
      ("Creating " & Width'Img & "x" & Height'Img &
       " window...");
   Ada.Text_IO.New_Line;

   --  Create window title
   Title := Interfaces.C.Strings.New_String
      ("Ada Wayland Window");

   --  Create the Wayland window
   Window := Wayland_Create_Window (Width, Height, Title);

   if Window = Window_Context (System.Null_Address) then
      Ada.Text_IO.Put_Line
         ("Error: Failed to create Wayland window");
      Interfaces.C.Strings.Free (Title);
      return;
   end if;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
      ("Window created successfully!");

   --  Paint the window with a gradient
   Wayland_Paint_Gradient (Window);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
      ("Window displayed! Press Ctrl+C to exit.");
   Ada.Text_IO.New_Line;

   --  Event loop - keep the window open
   loop
      if Wayland_Dispatch_Event (Window) < 0 then
         Ada.Text_IO.Put_Line
            ("Error: Display dispatch failed");
         exit;
      end if;
   end loop;

   --  Cleanup
   Wayland_Destroy_Window (Window);
   Interfaces.C.Strings.Free (Title);
   Ada.Text_IO.Put_Line ("Program terminated");

end Wayland_Window;
