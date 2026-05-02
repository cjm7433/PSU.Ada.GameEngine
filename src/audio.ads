-- audio.ads
--
-- This package defines the interface for audio playback in the game engine.
-- It provides procedures for initializing the audio system, playing audio files,
-- updating the audio state, and finalizing the audio system on shutdown.

with System;

package Audio is
   procedure Initialize;
   procedure Play_Audio (Filename : String; Looping : Boolean; Volume   : Float := 1.0);
   procedure Update;
   procedure Finalize;
end Audio;