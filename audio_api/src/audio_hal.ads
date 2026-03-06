pragma Ada_2022;
pragma SPARK_Mode (Off);

with System;
with System.Storage_Elements;

package Audio_HAL is

   type Sample_Rate is new Positive;
   type Channel_Count is new Positive;

   type Frame_Count is new Natural;

   type Audio_Config is record
      Rate          : Sample_Rate;
      Channels      : Channel_Count;
      Frames_Buffer : Frame_Count;
   end record;

   type Status is
     (Ok,
      Not_Initialized,
      Device_Error,
      Unsupported_Config);

   subtype Sample is Float;

   type Sample_Buffer is array (Natural range <>) of Sample;

end Audio_HAL;

