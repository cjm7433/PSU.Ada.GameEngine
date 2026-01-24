with Math.Linear_Algebra; use Math.Linear_Algebra;


package ECS.Components.Transform is
   type Transform is record
      Position : Vector2;
      Rotation : Float;    -- Radians
      Scale    : Vector2;
   end record;
end;