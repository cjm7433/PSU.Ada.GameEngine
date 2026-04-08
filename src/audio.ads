with System;

package Audio is
   procedure Initialize;
   procedure Play_Audio (Filename : String; Looping : Boolean);
   procedure Update;
   procedure Finalize;
end Audio;