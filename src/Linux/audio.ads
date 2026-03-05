--  ============================================================
--  Audio Stub for Linux
--  ============================================================
--  Provides minimal audio interface stub for Linux
--  TODO: Implement with ALSA, PulseAudio, or other Linux audio
--  ============================================================

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Audio is
   package IC renames Interfaces.C; use IC;
   --  No linker options for Linux stub (would be -lasound for ALSA)

   --  Type aliases for compatibility (Linux doesn't use these)
   subtype LPCSTR is IC.Strings.chars_ptr;
   subtype LPSTR is IC.Strings.chars_ptr;
   subtype HANDLE is System.Address;

   --  Stub function for Windows MCI audio (not implemented on Linux)
   function mciSendStringA (
      lpszCommand : LPCSTR;
      lpszReturnString : LPSTR;
      uReturnLength : IC.int;
      hwndCallback : HANDLE
   ) return IC.int is (0);  -- Return success stub

   --  Main audio interface
   procedure Play_Audio (File_Path : String);
   procedure Cleanup_Sounds;

private
   use Ada.Strings.Unbounded;

   package Unbounded_Strings renames Ada.Strings.Unbounded;
   package Alias_Vectors is new Ada.Containers.Vectors (Index_Type =>
      Natural, Element_Type => Unbounded_Strings.Unbounded_String);

   Tracked_Aliases   : Alias_Vectors.Vector;
   Audio_ID          : Natural := 0;

end Audio;
