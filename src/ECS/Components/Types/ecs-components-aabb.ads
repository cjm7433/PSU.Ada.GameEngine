package ECS.Components.AABB is
   type AABB_Component is new Component with record
      Left : Float;
      Bottom : Float;
      Right : Float;
      Top : Float;
   end record;
end;
