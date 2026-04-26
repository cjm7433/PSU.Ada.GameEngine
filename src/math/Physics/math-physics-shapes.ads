with Math.Linear_Algebra;  use Math.Linear_Algebra;
with Math.Physics.AABBs;   use Math.Physics.AABBs;


package Math.Physics.Shapes is
   -- Geometrical classification of a shape
   type Shape_Model is (Circle, Polygon);


   -- Shape type
   type Shape (Model : Shape_Model) is private;


   -- Computes a new AABB given a shape
    function Compute_AABB (
        Geometry  : Shape;
        Position  : Vector2
    ) return AABB;


private
   -- Shape type
   type Shape (Model : Shape_Model) is record
      case Model is
         when Circle =>
            Radius : Float;

         when Polygon =>
            Vertices : Vertex_Set_Access;
      end case;
   end record;
end;