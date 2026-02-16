with ECS.Components;          use ECS.Components;
with Math.Linear_Algebra;     use Math.Linear_Algebra;

package ECS.Components_Collider is

   type Shape_Kind is (AABB);

   type Collider is new Component with record
      Shape          : Shape_Kind := AABB;
      Half_Extents   : Vector2;
      Is_Trigger     : Boolean := False;
   end record;

end ECS.Components_Collider;
