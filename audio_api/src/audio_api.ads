with Audio_Control;
with Audio_Data;

with Audio_Hal; use Audio_Hal;

generic
   type Device is private;
   with package Control is new Audio_Control (Device, others => <>);
   with package Data is new Audio_Data (Device, others => <>);
   with function Make_Device return Device;
   with function Read_ID (Dev : Device) return String;

package Audio_API is
      --  Re-export the generic formal type through a visible name so
      --  clients of instantiated packages can declare variables.
      subtype Device_Handle is Device;

      procedure Open (Dev : in out Device; Cfg : Audio_Config; Res : out Status); 
      procedure Close (Dev : in out Device; Res : out Status);  
      procedure Submit (Dev : in out Device; Buf : Sample_Buffer; Frames : Frame_Count; Res : out Status);
      function Make_Dev return Device;
      function Get_ID (Dev : Device) return String;
end Audio_API;