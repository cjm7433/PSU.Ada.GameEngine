package body Audio_Control is

   procedure Open
     (Dev : in out Device;
      Rate : Positive;
      Channels : Positive) is
   begin
      Low_Level_Open (Dev, Rate, Channels);
   end;

   procedure Close
     (Dev : in out Device)  is
   begin
      Low_Level_Close (Dev);
   end;

end Audio_Control;