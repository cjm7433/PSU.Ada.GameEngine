with Math.Linear_Algebra; use Math.Linear_Algebra;


package Math.Physics.AABBs is
   -- Axis-Aligned Bounding Box; Half-extent implementation
   type AABB is record
        Center    : Vector2;
        Half_Size : Vector2;
   end record;


   type Vertex_Set is array (Natural range <>) of Vector2;
   type Vertex_Set_Access is access Vertex_Set;


   -- Returns an AABB computed from a set of vertices
   function From_Vertices (
         Position          : Vector2;
         Vertices_Access   : Vertex_Set_Access
   ) return AABB;


   -- Returns the minimum vector in an AABB
   function Get_Min (Box : AABB) return Vector2;


   -- Returns the maximum vector in an AABB
   function Get_Max (Box : AABB) return Vector2;


   -- Returns the axis-aligned normal vector between two AABBs
   function Get_Aligned_Normal (
         Box_From : AABB;
         Box_To   : AABB
   ) return Vector2;


   -- Reports whether or not two AABBs are overlapped
   function Is_Overlapping (
        Box1 : AABB;
        Box2 : AABB
   ) return Boolean;


   -- Reports at what fractional part along a motion vector collision between two AABBs occurred.
   function Collision_Sweep (
         Box1     : AABB;
         Box2     : AABB;
         Motion   : Vector2
   ) return Float;

   -- Get the half-size of an AABB
   function Get_Half_Size (Box : AABB) return Vector2;

end;
