pragma Ada_2022;
pragma SPARK_Mode (On);

generic
   type Device is limited private;

   with procedure Low_Level_Open
     (Dev : in out Device;
      Rate : Positive;
      Channels : Positive);

   with procedure Low_Level_Close
     (Dev : in out Device);

package Audio_Control is

   procedure Open
     (Dev : in out Device;
      Rate : Positive;
      Channels : Positive);

   procedure Close
     (Dev : in out Device);

end Audio_Control;