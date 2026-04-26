with Math.Linear_Algebra;  use Math.Linear_Algebra;
with Math.Physics.AABBs;   use Math.Physics.AABBs;


package body Math.Physics.Shapes is
   -- Computes a new AABB given a shape
    function Compute_AABB (
        Geometry  : Shape;
        Position  : Vector2
    ) return AABB is
      Box  : AABB;
    begin
      case Geometry.Model is
         when Circle =>
            Box := (
               Center      => Position,
               Half_Size   => (Geometry.Radius, Geometry.Radius)
            );

         when Polygon =>
            Box := From_Vertices (Position, Geometry.Vertices);

         when others =>
            raise Constraint_Error with "Unknown Shape Model";
      end case;

      return Box;
    end;
end;