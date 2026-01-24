with Math.Linear_Algebra; use Math.Linear_Algebra;


package ECS.Components.Motion is
   type Motion is record
      Linear_Velocity:        Vector2;
      Angular_Velocity:       Vector2;
      Linear_Acceleration:    Vector2;
      Angular_Acceleration:   Vector2;
   end record;
end;