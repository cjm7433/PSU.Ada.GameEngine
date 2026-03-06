with Math.Linear_Algebra; use Math.Linear_Algebra;


package Math.Physics is
   type AABB is tagged private;   -- Axis-Aligned Bounding Box


   type Vertex_Set is array (Natural range <>) of Vector2;


   -- Computes a new AABB given a set of vertices
    function Compute_AABB (
        Vertices : Vertex_Set
    ) return AABB;


   -- Reports whether or not two AABBs are overlapped
   function Is_Overlapping (
        Box1 : AABB;
        Box2 : AABB
   ) return Boolean;

   -- Get the center of an AABB
   function Get_Center (Box : AABB) return Vector2;

   -- Get the half-size of an AABB
   function Get_Half_Size (Box : AABB) return Vector2;


private
   -- Axis-Aligned Bounding Box; Half-extent implementation
   type AABB is tagged record
        Center    : Vector2;
        Half_Size : Vector2;
   end record;
end;