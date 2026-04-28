with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;


package ECS.Components.Audio is
   type Audio_Component is new Component with record
      File_Path   : Unbounded_String;
      Volume      : Float;
   end record;
end;
