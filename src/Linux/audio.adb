--  ============================================================
--  Audio Stub Implementation for Linux
--  ============================================================

with Ada.Text_IO;

package body Audio is

   procedure Play_Audio (File_Path : String) is
   begin
      Ada.Text_IO.Put_Line ("[Audio Stub] Would play audio: " & File_Path);
      --  TODO: Implement with ALSA, PulseAudio, or SDL2 audio
   end Play_Audio;

   procedure Cleanup_Sounds is
   begin
      Ada.Text_IO.Put_Line ("[Audio Stub] Cleanup sounds");
      Tracked_Aliases.Clear;
      Audio_ID := 0;
   end Cleanup_Sounds;

end Audio;
