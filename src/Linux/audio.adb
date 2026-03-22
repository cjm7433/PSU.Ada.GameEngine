--  ============================================================
--  Audio Stub Implementation for Linux
--  ============================================================

with Ada.Text_IO;
with System;

package body Audio is

   procedure Initialize is
   begin
      null;
   end Initialize;

   procedure Play_Audio (Filename : String) is
   begin
      Ada.Text_IO.Put_Line ("[Audio Stub] Would play audio: " & Filename);
      --  TODO: Implement with ALSA, PulseAudio, or SDL2 audio
   end Play_Audio;

   procedure Update is
   begin
      null;
   end Update;

   procedure Finalize is
   begin
      null;
   end Finalize;


end Audio;
