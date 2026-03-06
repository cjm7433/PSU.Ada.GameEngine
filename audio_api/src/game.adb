with Audio;

with Ada.Text_IO; use Ada.Text_IO;

procedure Game is
   Device : Audio.Device_Handle := Audio.Make_Dev;
begin
   Put_Line (Audio.Get_ID (Device));
end Game;
