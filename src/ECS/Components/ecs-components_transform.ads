with ECS.Components;          use ECS.Components;
with Math.Linear_Algebra;     use Math.Linear_Algebra;

package ECS.Components_Transform is

   type Transform is new Component with record
      Position : Vector2;
      Rotation : Float;
      Scale    : Vector2;
   end record;

end ECS.Components_Transform;
