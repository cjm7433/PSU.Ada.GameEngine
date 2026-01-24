with Math.Linear_Algebra; use Math.Linear_Algebra;


package body Math.Physics is
   -- Computes a new AABB given a set of vertices
   function Compute_AABB (
      Vertices : Vertex_Set
   ) return AABB with
      Refined_Post => (
               Compute_AABB'Result.Min.X <= Compute_AABB'Result.Max.X
         and   Compute_AABB'Result.Min.Y <= Compute_AABB'Result.Max.Y
      )
   is
      Min : Vector2 := (X => Float'Last, Y => Float'Last);
      Max : Vector2 := (X => Float'First, Y => Float'First);
   begin
      for V of Vertices loop
         -- Update left and top edges
         Min.X := Float'Min(Min.X, V.X);
         Min.Y := Float'Min(Min.Y, V.Y);

         -- Update right and bottom edges
         Max.X := Float'Max(Max.X, V.X);
         Max.Y := Float'Max(Max.Y, V.Y);
      end loop;

      return (Min, Max);
   end;


   -- Reports whether or not two AABBs are overlapped
   function Is_Overlapping (
      Box1 : AABB;
      Box2 : AABB
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