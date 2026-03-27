with Math.Linear_Algebra;     use Math.Linear_Algebra;


package ECS.Components.Motion is
   type Motion_Component is new Component with record
      Linear_Velocity       : Vector2;
      Angular_Velocity      : Float;
      Linear_Acceleration   : Vector2;
      Angular_Acceleration  : Float;
   end record;
end;
