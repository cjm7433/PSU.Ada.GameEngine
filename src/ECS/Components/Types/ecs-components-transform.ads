with Math.Linear_Algebra;     use Math.Linear_Algebra;


package ECS.Components.Transform is
   type Transform_Component is new Component with record
      Position : Vector2;
      Rotation : Float;
      Scale    : Vector2;
   end record;
end;
