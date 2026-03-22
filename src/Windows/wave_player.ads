with System;

package Wave_Player is

   procedure Initialize;
   procedure Play_Wav (Filename : String);
   procedure Update;
   procedure Finalize;

private
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-lxaudio2_9");

end Wave_Player;