--  ============================================================
--  Wayland Wrapper - Ada Bindings
--  ============================================================
--  Simple Ada interface to the C Wayland wrapper
--  ============================================================

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Wayland_Wrapper is

   use Interfaces.C;

   --  Opaque handle to window context
   type Window_Context is new System.Address;

   --  Input event callback types
   type Key_Callback is access procedure (Keycode : unsigned; State : unsigned)
      with Convention => C;
   
   type Mouse_Button_Callback is access procedure 
      (Button : unsigned; X : int; Y : int; State : unsigned)
      with Convention => C;
   
   type Mouse_Motion_Callback is access procedure (X : int; Y : int)
      with Convention => C;

   --  =========================================================
   --  Wayland_Create_Window
   --  =========================================================
   --  Creates and displays a Wayland window with the specified
   --  dimensions and title.
   --  Returns: Window context handle, or null on failure
   --  =========================================================
   function Wayland_Create_Window
     (Width  : int;
      Height : int;
      Title  : Interfaces.C.Strings.chars_ptr)
      return Window_Context;
   pragma Import (C, Wayland_Create_Window,
                  "wayland_create_window");

   --  =========================================================
   --  Wayland_Paint_Gradient
   --  =========================================================
   --  Paints the window buffer with a gradient pattern.
   --  The gradient transitions from blue to red horizontally
   --  and from dark to bright vertically.
   --  =========================================================
   procedure Wayland_Paint_Gradient (Ctx : Window_Context);
   pragma Import (C, Wayland_Paint_Gradient,
                  "wayland_paint_gradient");

   --  =========================================================
   --  Wayland_Update_Buffer
   --  =========================================================
   --  Copies pixel data from source buffer to Wayland window
   --  and commits the surface to display changes.
   --  =========================================================
   procedure Wayland_Update_Buffer (Ctx : Window_Context; 
                                     Buffer : System.Address);
   pragma Import (C, Wayland_Update_Buffer,
                  "wayland_update_buffer");

   --  =========================================================
   --  Wayland_Set_Key_Callback
   --  =========================================================
   --  Sets the callback function for keyboard events
   --  =========================================================
   procedure Wayland_Set_Key_Callback (Ctx : Window_Context;
                                        Callback : Key_Callback);
   pragma Import (C, Wayland_Set_Key_Callback,
                  "wayland_set_key_callback");

   --  =========================================================
   --  Wayland_Set_Mouse_Button_Callback
   --  =========================================================
   --  Sets the callback function for mouse button events
   --  =========================================================
   procedure Wayland_Set_Mouse_Button_Callback 
      (Ctx : Window_Context; Callback : Mouse_Button_Callback);
   pragma Import (C, Wayland_Set_Mouse_Button_Callback,
                  "wayland_set_mouse_button_callback");

   --  =========================================================
   --  Wayland_Set_Mouse_Motion_Callback
   --  =========================================================
   --  Sets the callback function for mouse motion events
   --  =========================================================
   procedure Wayland_Set_Mouse_Motion_Callback
      (Ctx : Window_Context; Callback : Mouse_Motion_Callback);
   pragma Import (C, Wayland_Set_Mouse_Motion_Callback,
                  "wayland_set_mouse_motion_callback");

   --  =========================================================
   --  Wayland_Dispatch_Pending
   --  =========================================================
   --  Processes one event from the Wayland display.
   --  Returns: 0 on success, -1 on error
   --  =========================================================
   function Wayland_Dispatch_Event
      (Ctx : Window_Context) return int;
   pragma Import (C, Wayland_Dispatch_Event,
                  "wayland_dispatch_event");

   --  =========================================================
   --  Wayland_Dispatch_Pending
   --  =========================================================
   --  Processes pending events without blocking (non-blocking).
   --  Returns: Number of events dispatched, or -1 on error
   --  =========================================================
   function Wayland_Dispatch_Pending
      (Ctx : Window_Context) return int;
   pragma Import (C, Wayland_Dispatch_Pending,
                  "wayland_dispatch_pending");

   --  =========================================================
   --  Wayland_Destroy_Window
   --  =========================================================
   --  Destroys the window and frees all associated resources.
   --  =========================================================
   procedure Wayland_Destroy_Window (Ctx : Window_Context);
   pragma Import (C, Wayland_Destroy_Window,
                  "wayland_destroy_window");

end Wayland_Wrapper;
