with Linear_Algebra; use Linear_Algebra;


package Physics is
   type PAABB is tagged private;   -- Padded Axis-Aligned Bounding Box


   type Vertex_Set is array (Natural range <>) of Vector2;


   -- Computes new PAABB given a set of vertices; Assumes rotation around origin
    function Compute_PAABB (
        Vertices : Vertex_Set
    ) return PAABB;


   -- Reports whether or not two PAABBs are overlapped
   function Is_Overlapping (
        Box1 : PAABB;
        Box2 : PAABB
   ) return Boolean;
private
   -- Padded Axis-Aligned Bounding Box type
   type PAABB is tagged record
        Min : Vector2;       -- Top-left corner
        Max : Vector2;       -- Bottom-right corner
   end record;
end;