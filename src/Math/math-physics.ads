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


private
   -- Axis-Aligned Bounding Box type
   type AABB is tagged record
        Min : Vector2;       -- Top-left corner
        Max : Vector2;       -- Bottom-right corner
   end record;
end;