
package body Audio_API is
   procedure Open (Dev : in out Device; Cfg : Audio_Config; Res : out Status) is
   begin
      Control.Open (Dev, Integer (Cfg.Rate), Integer (Cfg.Channels));
   end;

   procedure Close (Dev : in out Device; Res : out Status) is
   begin
      Control.Close (Dev);
   end;

   procedure Submit (Dev : in out Device; Buf : Sample_Buffer; Frames : Frame_Count; Res : out Status) is
   begin
      Data.Submit (Dev, Buf'Address, Integer (Frames));
   end;

   function Make_Dev return Device is
      (Make_Device);

   function Get_ID (Dev : Device) return String is
      (Read_ID (Dev));
end Audio_API;