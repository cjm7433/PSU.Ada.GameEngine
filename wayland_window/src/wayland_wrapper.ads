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
   --  Wayland_Dispatch_Event
   --  =========================================================
   --  Processes one event from the Wayland display.
   --  Returns: 0 on success, -1 on error
   --  =========================================================
   function Wayland_Dispatch_Event
      (Ctx : Window_Context) return int;
   pragma Import (C, Wayland_Dispatch_Event,
                  "wayland_dispatch_event");

   --  =========================================================
   --  Wayland_Destroy_Window
   --  =========================================================
   --  Destroys the window and frees all associated resources.
   --  =========================================================
   procedure Wayland_Destroy_Window (Ctx : Window_Context);
   pragma Import (C, Wayland_Destroy_Window,
                  "wayland_destroy_window");

end Wayland_Wrapper;
