with System;

package Audio is
   procedure Initialize;
   procedure Play_Audio (Filename : String; Looping : Boolean; Volume   : Float := 1.0);
   procedure Update;
   procedure Finalize;
end Audio;