with System;

generic
   type Device is limited private;

   with procedure Low_Level_Write
     (Dev     : in out Device;
      Samples : System.Address;
      Frames  : Natural);

package Audio_Data is

   procedure Submit
     (Dev     : in out Device;
      Samples : System.Address;
      Frames  : Natural);

end Audio_Data;