package ECS.Components.Transform is
   type Transform is record
      Position : Float;    --TODO: Replace with Vector
      Rotation : Float;    -- Radians
      Scale    : Float;    --TODO: Replace with Vector
   end record;
end;