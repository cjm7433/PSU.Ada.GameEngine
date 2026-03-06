package body Audio_Data is

   procedure Submit
     (Dev     : in out Device;
      Samples : System.Address;
      Frames  : Natural) is
   begin
      Low_Level_Write (Dev, Samples, Frames);
   end;

end Audio_Data;