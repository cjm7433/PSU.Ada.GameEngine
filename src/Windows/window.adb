with System; use System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Event_Manager; use ECS.Event_Manager;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with ECS.Event;
with ECS;
with Win32; use Win32;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   W_Instance   : Window_Access;
   --  Declare the event manager
   Manager : aliased ECS.Event_Manager.Platform_Event_Handler;
   Need_Background_Clear : Boolean := True;

   --  Return the lower 16 bits of LPARAM
   function LOWORD (Value : LPARAM) return WORD is
      use Interfaces;

   begin
      return WORD (Unsigned_32 (Value) and 16#FFFF#);
   end LOWORD;

   --  Return the upper 16 bits of LPARAM
   function HIWORD (value : LPARAM) return WORD is
   begin
      return WORD (value / 2 ** 16);
   end HIWORD;

   --  EDITED 11/26/24 to add WM_KEYDOWN and WM_LBUTTONDOWN
   function Wnd_Proc (H_Wnd   : HWND;
                     Msg     : IC.unsigned;
                     W_Param : WPARAM;
                     L_Param : LPARAM) return LRESULT is
   begin
      case Msg is
         when WM_DESTROY =>
            Post_Quit_Message (0);

         when WM_ERASEBKGND =>
            -- We draw the full frame ourselves; suppress default background wipe.
            return 1;

         when WM_PAINT =>
            -- Validate the invalid region so WM_PAINT does not continuously requeue.
            declare
               Paint_Struct : aliased PAINTSTRUCT;
               Paint_DC     : HDC;
               Paint_Result : Boolean;
            begin
               Paint_DC := Begin_Paint (H_Wnd, Paint_Struct'Access);
               Paint_Result := End_Paint (H_Wnd, Paint_Struct'Access);
               if Paint_DC = Null_Address or else not Paint_Result then
                  null;
               end if;
            end;

         when WM_SIZE =>
            W_Instance.Current_Width   := IC.int (LOWORD (L_Param));
            W_Instance.Current_Height  := IC.int (HIWORD (L_Param));
            ECS.WindowWidth := Integer (LOWORD (L_Param));
            Need_Background_Clear := True;

         when WM_KEYDOWN =>
         declare
            KeyCode : ECS.Event.Byte := ECS.Event.Byte (W_Param);
            Event : ECS.Event.Event_T :=
            (Source    => 0,
             EventType => ECS.Event.KeyDown,
             Data      => (KeyCode   => KeyCode,
                           MouseX      => 0,
                           MouseY      => 0,
                           W_Width     => 0,
                           W_Height    => 0,
                           Additional  => [others => 0]));
         begin
            Emit_Event (Manager, Event);
         end;

         when WM_KEYUP =>
         declare
            KeyCode : ECS.Event.Byte := ECS.Event.Byte (W_Param);
            Event : ECS.Event.Event_T :=
            (Source    => 0,
               EventType => ECS.Event.KeyUp,
               Data      => (KeyCode    => KeyCode,
                           MouseX     => 0,
                           MouseY     => 0,
                           W_Width     => 0,
                           W_Height    => 0,
                           Additional => [others => 0]));
         begin
            Emit_Event (Manager, Event);
         end;

         when WM_LBUTTONDOWN =>
         declare
            MouseX : Integer := Integer (IC.int (LOWORD (L_Param)));
            MouseY : Integer := Integer (IC.int (HIWORD (L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.L_MouseDown,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0,
                             W_Height    => 0,
                             Additional => [others => 0]));
         begin
            Emit_Event (Manager, MouseEvent);
         end;

         when WM_LBUTTONUP =>
         declare
            MouseX : Integer := Integer (IC.int (LOWORD (L_Param)));
            MouseY : Integer := Integer (IC.int (HIWORD (L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.L_MouseUp,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0,
                             W_Height    => 0,
                             Additional => [others => 0]));
         begin
            Emit_Event (Manager, MouseEvent);
         end;

         when WM_RBUTTONDOWN =>
         declare
            MouseX : Integer := Integer (IC.int (LOWORD (L_Param)));
            MouseY : Integer := Integer (IC.int (HIWORD (L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.R_MouseDown,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0,
                             W_Height    => 0,
                             Additional => [others => 0]));
         begin
            Emit_Event (Manager, MouseEvent);
         end;

         when WM_RBUTTONUP =>
         declare
            MouseX : Integer := Integer (IC.int (LOWORD (L_Param)));
            MouseY : Integer := Integer (IC.int (HIWORD (L_Param)));
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.R_MouseUp,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0,
                             W_Height    => 0,
                             Additional => [others => 0]));
         begin
            Emit_Event (Manager, MouseEvent);
         end;

         when WM_MOUSEMOVE =>
         declare
            MouseX : Integer := Integer (IC.int (LOWORD (L_Param)));
            MouseY : Integer := Integer (IC.int (HIWORD (L_Param)));
         begin
               declare
                  MouseEvent : ECS.Event.Event_T :=
                  (Source    => 0,
                   EventType => ECS.Event.MouseMove,
                   Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             W_Width     => 0,
                             W_Height    => 0,
                             Additional => [others => 0]));
               begin
                  Emit_Event (Manager, MouseEvent);
               end;
         end;
         when others =>
            return Def_Window_Proc (H_Wnd, Msg, W_Param, L_Param);
      end case;
      return 0;
   end Wnd_Proc;

   --  Creates a new window instance and returns it to the caller
   function New_Window (Width : Interfaces.C.int; Height : Interfaces.C.int;
         Title : Unbounded_String) return Window_Access is
      WC       : aliased WNDCLASS;
      Res_Atom : ATOM;

   begin
      W_Instance := new Window_T;
      --  Initialize the WNDCLASS struct
      WC.Lp_fn_Wnd_Proc    := Wnd_Proc'Access;
      WC.H_Instance        := Get_H_Instance;
      WC.Lpsz_Class_Name   := TO_LPCSTR (ICS.New_String ("SampleWindowClass"));
      WC.H_br_Background   := HBRUSH (Get_Stock_Object (COLOR_BACKGROUND));

      --  Register the window class with the win32 API
      Res_Atom := Register_Class (WC'Access);
      if Res_Atom = 0 then
         raise Program_Error with "Failed to register window class.";
      end if;

      --  Create the window
      W_Instance.Handle := Create_Window (
         Dw_Ex_Style    => 0,
         Lp_Class_Name  => WC.Lpsz_Class_Name,
         Lp_Window_Name => TO_LPCSTR (ICS.New_String (To_String (Title))),
         Dw_Style       => WS_OVERLAPPEDWINDOW,
         X              => 0,  --  defines the window start coordinate.
         Y              => 0,
         Width          => Width,
         Height         => Height,
         H_Wnd_Parent   => System.Null_Address,
         H_Menu         => System.Null_Address,
         H_Instance     => WC.H_Instance,
         Lp_Param       => System.Null_Address
      );

      --  Window properties
      W_Instance.Width := Width;
      W_Instance.Height := Height;
      W_Instance.Current_Width := Width;
      W_Instance.Current_Height := Height;
      W_Instance.Title := Title;

      --  Show the window
      --  Not used anywhere yet, but might be needed later
      declare
         SW_Result : Boolean;
         UW_Result : Boolean;
      begin
         SW_Result := Show_Window (W_Instance.Handle, SW_SHOW);
         UW_Result := Update_Window (W_Instance.Handle);
      end;
      return W_Instance;

   end New_Window;

   procedure Draw_Buffer (
      Buffer     : System.Address;
      Src_Width  : Interfaces.C.int;
      Src_Height : Interfaces.C.int) is
      Bmi_Reset : Byte_Array (0 .. BITMAPINFO'Size / 8 - 1) := (others => 0);
      Bmi : aliased BITMAPINFO with Address => Bmi_Reset'Address;
      Result : Interfaces.C.int;
      Handle_DC : HDC := GetDC (W_Instance.Handle);
      Source_Width  : Integer := Integer (Src_Width);
      Source_Height : Integer := Integer (Src_Height);
      Client_Width  : Integer := Integer (W_Instance.Current_Width);
      Client_Height : Integer := Integer (W_Instance.Current_Height);
      Scale_X       : Float;
      Scale_Y       : Float;
      Scale         : Float;
      Scaled_Width  : Integer;
      Scaled_Height : Integer;
      Dest_X        : Integer;
      Dest_Y        : Integer;
      Full_Client_Rect : aliased RECT;
      Fill_Result      : Interfaces.C.int;
      Background_Brush : HBRUSH := HBRUSH (Get_Stock_Object (BLACK_BRUSH));
   begin
      if Handle_DC = Null_Address then
         return;
      end if;

      if Source_Width <= 0 or else Source_Height <= 0 or else Client_Width <= 0 or else Client_Height <= 0 then
         declare
            Result_Release : Boolean := ReleaseDC (W_Instance.Handle, Handle_DC);
         begin
            if not Result_Release then
               Put_Line ("Failed to release HDC.");
            end if;
         end;
         return;
      end if;

      -- Preserve aspect ratio by using the smaller axis scale.
      Scale_X := Float (Client_Width) / Float (Source_Width);
      Scale_Y := Float (Client_Height) / Float (Source_Height);
      Scale := Float'Min (Scale_X, Scale_Y);

      Scaled_Width := Integer (Float (Source_Width) * Scale);
      Scaled_Height := Integer (Float (Source_Height) * Scale);

      if Scaled_Width <= 0 or else Scaled_Height <= 0 then
         declare
            Result_Release : Boolean := ReleaseDC (W_Instance.Handle, Handle_DC);
         begin
            if not Result_Release then
               Put_Line ("Failed to release HDC.");
            end if;
         end;
         return;
      end if;

      -- Center the scaled image to produce letterbox/pillarbox bars.
      Dest_X := (Client_Width - Scaled_Width) / 2;
      Dest_Y := (Client_Height - Scaled_Height) / 2;

      Bmi.bmiHeader.biSize            := BITMAPINFOHEADER'Size / 8;
      -- Use framebuffer dimensions, not window dimensions, for source bitmap metadata.
      Bmi.bmiHeader.biWidth           := Src_Width;
      Bmi.bmiHeader.biHeight          := -Src_Height;
      Bmi.bmiHeader.biPlanes          := 1;
      Bmi.bmiHeader.biBitCount        := 32;
      Bmi.bmiHeader.biCompression     := BI_RGB;
      Bmi.bmiHeader.biSizeImage       := 0;
      Bmi.bmiHeader.biXPelsPerMeter   := 0;
      Bmi.bmiHeader.biYPelsPerMeter   := 0;
      Bmi.bmiHeader.biClrUsed         := 0;
      Bmi.bmiHeader.biClrImportant    := 0;

      -- Clear letterbox/pillarbox bars only when needed (startup/resize).
      if Need_Background_Clear then
         Full_Client_Rect.Left := 0;
         Full_Client_Rect.Top := 0;
         Full_Client_Rect.Right := Interfaces.C.long (Client_Width);
         Full_Client_Rect.Bottom := Interfaces.C.long (Client_Height);
         Fill_Result := Fill_Rect (Handle_DC, Full_Client_Rect'Address, Background_Brush);
         if Fill_Result = 0 then
            null;
         end if;
         Need_Background_Clear := False;
      end if;

      Result := Stretch_DIBits (
         H_Dc           => Handle_DC,
         X_Dest         => Interfaces.C.int (Dest_X),
         Y_Dest         => Interfaces.C.int (Dest_Y),
         Dest_Width     => Interfaces.C.int (Scaled_Width),
         Dest_Height    => Interfaces.C.int (Scaled_Height),
         X_Src          => 0,
         Y_Src          => 0,
         Src_Width      => Src_Width,
         Src_Height     => Src_Height,
         Bits           => Buffer,
         Bitmap_Info    => Bmi'Unchecked_Access,
         Usage          => DIB_RGB_COLORS,
         Rop            => SRCCOPY
      );

      --  Release the HDC to avoid memory leak
      if Handle_DC /= Null_Address then
         declare
            Result_Release : Boolean :=
               ReleaseDC (W_Instance.Handle, Handle_DC);
         begin
            if not Result_Release then
               Put_Line ("Failed to release HDC.");
            end if;
         end;
      end if;

      -- Draw returns negative on true GDI error; zero can occur for no-op cases.
      if Result < 0 then
         Put_Line ("StretchDIBits failed.");
      end if;
   end Draw_Buffer;

   --  =========================================================
   --  Process_Events
   --  =========================================================
   --  On Windows, event processing is handled by the message loop
   --  This is a no-op stub for compatibility with Linux
   --  =========================================================
   procedure Process_Events is
   begin
      null;  --  Windows processes events through Get_Message/Dispatch_Message
   end Process_Events;

end Window;
