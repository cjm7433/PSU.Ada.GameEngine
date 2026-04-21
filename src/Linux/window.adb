with System; use System;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Event_Manager;
with ECS.Event;
with ECS;
with Wayland_Wrapper; use Wayland_Wrapper;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   W_Instance   : Window_Access;
   --  Declare the event manager
   Manager : aliased ECS.Event_Manager.Platform_Event_Handler;
   
   --  Wayland-specific context
   Wayland_Context : Wayland_Wrapper.Window_Context := 
      Wayland_Wrapper.Window_Context (System.Null_Address);

   --  =========================================================
   --  Input Event Handlers (called from C)
   --  =========================================================
   
   procedure Handle_Key_Event (Keycode : IC.unsigned; State : IC.unsigned)
      with Convention => C;
   
   procedure Handle_Key_Event (Keycode : IC.unsigned; State : IC.unsigned) is
      Event : ECS.Event.Event_T;
   begin
      Event.Source := 0;
      Event.Data.KeyCode := ECS.Event.Byte (Keycode);
      Event.Data.MouseX := 0;
      Event.Data.MouseY := 0;
      Event.Data.W_Width := 0;
      Event.Data.W_Height := 0;
      Event.Data.Additional := (others => 0);
      
      if State = 1 then
         Event.EventType := ECS.Event.KeyDown;
      else
         Event.EventType := ECS.Event.KeyUp;
      end if;
      
      ECS.Event_Manager.Emit_Event (Manager, Event);
   end Handle_Key_Event;
   
   procedure Handle_Mouse_Button (Button : IC.unsigned; X : IC.int; 
                                  Y : IC.int; State : IC.unsigned)
      with Convention => C;
   
   procedure Handle_Mouse_Button (Button : IC.unsigned; X : IC.int;
                                  Y : IC.int; State : IC.unsigned) is
      Event : ECS.Event.Event_T;
   begin
      Event.Source := 0;
      Event.Data.KeyCode := 0;
      Event.Data.MouseX := Integer (X);
      Event.Data.MouseY := Integer (Y);
      Event.Data.W_Width := 0;
      Event.Data.W_Height := 0;
      Event.Data.Additional := (others => 0);
      
      --  Map Wayland button codes to Windows-style event types
      --  BTN_LEFT = 0x110, BTN_RIGHT = 0x111
      case Button is
         when 16#110# =>  --  Left button
            if State = 1 then
               Event.EventType := ECS.Event.L_MouseDown;
            else
               Event.EventType := ECS.Event.L_MouseUp;
            end if;
         when 16#111# =>  --  Right button
            if State = 1 then
               Event.EventType := ECS.Event.R_MouseDown;
            else
               Event.EventType := ECS.Event.R_MouseUp;
            end if;
         when others =>
            return;  --  Ignore other buttons
      end case;
      
      ECS.Event_Manager.Emit_Event (Manager, Event);
   end Handle_Mouse_Button;
   
   procedure Handle_Mouse_Motion (X : IC.int; Y : IC.int)
      with Convention => C;
   
   procedure Handle_Mouse_Motion (X : IC.int; Y : IC.int) is
      Event : ECS.Event.Event_T;
   begin
      Event.Source := 0;
      Event.EventType := ECS.Event.MouseMove;
      Event.Data.KeyCode := 0;
      Event.Data.MouseX := Integer (X);
      Event.Data.MouseY := Integer (Y);
      Event.Data.W_Width := 0;
      Event.Data.W_Height := 0;
      Event.Data.Additional := (others => 0);
      
      ECS.Event_Manager.Emit_Event (Manager, Event);
   end Handle_Mouse_Motion;

   --  =========================================================
   --  Draw_Buffer
   --  =========================================================
   --  Draws the provided buffer to the window
   --  =========================================================
   procedure Draw_Buffer (
      Buffer     : System.Address;
      Src_Width  : Interfaces.C.int;
      Src_Height : Interfaces.C.int) is
   begin
      -- Pass source dimensions so the Wayland wrapper can safely copy
      -- the source into the shared buffer when the sizes differ.
      if Wayland_Context /= Wayland_Wrapper.Window_Context (System.Null_Address) then
         Wayland_Wrapper.Wayland_Update_Buffer (Wayland_Context, Buffer, Src_Width, Src_Height);
      end if;
   end Draw_Buffer;

   --  =========================================================
   --  New_Window
   --  =========================================================
   --  Creates a new Wayland window with specified dimensions
   --  =========================================================
   function New_Window (Width : Interfaces.C.int; Height : Interfaces.C.int;
      Title : Unbounded_String) return Window_Access
   is
      Win : Window_Access;
      C_Title : ICS.chars_ptr;
   begin
      Win := new Window_T;
      Win.Width := Width;
      Win.Height := Height;
      Win.Current_Width := Width;
      Win.Current_Height := Height;
      Win.Title := Title;
      
      --  Convert title to C string
      C_Title := ICS.New_String (To_String (Title));
      
      --  Create Wayland window
      Wayland_Context := Wayland_Wrapper.Wayland_Create_Window (
         Width  => Width,
         Height => Height,
         Title  => C_Title
      );
      
      ICS.Free (C_Title);
      
      if Wayland_Context = Wayland_Wrapper.Window_Context (System.Null_Address) then
         Put_Line ("Error: Failed to create Wayland window");
         Free_Window (Win);
         return null;
      end if;
      
      Win.Handle := HWND (Wayland_Context);
      W_Instance := Win;
      
      --  Set global window dimensions for ECS
      ECS.WindowWidth := Integer (Width);
      
      --  Register input event callbacks
      Wayland_Wrapper.Wayland_Set_Key_Callback (Wayland_Context,
         Handle_Key_Event'Access);
      Wayland_Wrapper.Wayland_Set_Mouse_Button_Callback (Wayland_Context,
         Handle_Mouse_Button'Access);
      Wayland_Wrapper.Wayland_Set_Mouse_Motion_Callback (Wayland_Context,
         Handle_Mouse_Motion'Access);
      
      Put_Line ("Wayland window created successfully");
      return Win;
   end New_Window;

   --  =========================================================
   --  Process_Events
   --  =========================================================
   --  Process Wayland events (called from main loop)
   --  Uses non-blocking dispatch for game loops
   --  =========================================================
   procedure Process_Events is
      Result : IC.int;
   begin
      if Wayland_Context /= Wayland_Wrapper.Window_Context (System.Null_Address) then
         Result := Wayland_Wrapper.Wayland_Dispatch_Pending (Wayland_Context);
         if Result < 0 then
            Put_Line ("Warning: Wayland event dispatch error");
         end if;
      end if;
   end Process_Events;

   function Peek_Message return Boolean is
   begin
      null;
   end Peek_Message;
begin
   --  Note: Event manager doesn't need initialization
   --  Window context will be initialized when New_Window is called
   null;

end Window;
