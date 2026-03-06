package body Audio_Pipewire is

   function Make_Device return Device is
      ((Id => 1000));

   procedure Low_Level_Open
     (Dev : in out Device;
      Rate : Positive;
      Channels : Positive) is
   begin
      -- OPEN SPECIFIC WINDOWS
      null;
   end;

   procedure Low_Level_Write
     (Dev     : in out Device;
      Samples : System.Address;
      Frames  : Natural) is
   begin 
      -- WRITE SPECIFIC WINDOWS
      null;
   end;

   procedure Low_Level_Close
     (Dev : in out Device) is
   begin
      -- CLOSE SPECIFIC WINDOWS
      null;
   end;

   function Get_ID (Dev : Device) return String is
      (Dev.Id'Image);

end Audio_Pipewire;