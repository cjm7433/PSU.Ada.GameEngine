with Linear_Algebra; use Linear_Algebra;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;


package body Physics is
   -- Computes new PAABB given a set of vertices; Assumes rotation around origin
   function Compute_PAABB (
      Vertices : Vertex_Set
   ) return PAABB with
      Refined_Post => (
               Compute_PAABB'Result.Min.X <= Compute_PAABB'Result.Max.X
         and   Compute_PAABB'Result.Min.Y <= Compute_PAABB'Result.Max.Y
      )
   is
      Radius_Squared : Float := 0.0;
   begin
      -- Determine the vertex furthest from the rotation point
      for V of Vertices loop
         Radius_Squared := Float'Max(
               Radius_Squared,
               V.Distance_Squared(Vector2'(0.0, 0.0))
         );
      end loop;

      Radius : Float := Sqrt(Radius_Squared);

      return (
            Min => (-Radius, -Radius),
            Max => ( Radius,  Radius)
      );
   end;


   -- Reports whether or not two AABBs are overlapped
   function Is_Overlapping (
      Box1 : PAABB;
      Box2 : PAABB
   ) return Boolean is
   begin
      return (
               Box1.Min.X <= Box2.Max.X   -- 1's left face on or before 2's right face
         and   Box1.Max.X >= Box2.Min.X   -- 1's right face on or after 2's left face
         and   Box1.Min.Y <= Box2.Max.Y   -- 1's top face on or before 2's bottom face
         and   Box1.Max.Y >= Box2.Min.Y   -- 1's bottom face on or after 2's top face
      );
   end;
end;