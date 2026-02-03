with ECS.Components;          use ECS.Components;
with Math.Linear_Algebra;     use Math.Linear_Algebra;

package ECS.Components_Motion is

   type Motion is new Component with record
      Linear_Velocity       : Vector2;
      Angular_Velocity      : Vector2;
      Linear_Acceleration   : Vector2;
      Angular_Acceleration  : Vector2;
   end record;

end ECS.Components_Motion;
