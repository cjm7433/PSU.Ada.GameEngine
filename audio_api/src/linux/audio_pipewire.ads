with System;

package Audio_Pipewire is

   type Device is private;

   function Make_Device return Device;

   procedure Low_Level_Open
     (Dev : in out Device;
      Rate : Positive;
      Channels : Positive);

   procedure Low_Level_Write
     (Dev     : in out Device;
      Samples : System.Address;
      Frames  : Natural);

   procedure Low_Level_Close
     (Dev : in out Device);

   function Get_ID (Dev : Device) return String;

private
   type Device is record
      Id : Natural := 0;
   end record;

end Audio_Pipewire;