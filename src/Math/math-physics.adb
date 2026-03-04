with Math.Linear_Algebra; use Math.Linear_Algebra;


package body Math.Physics is
   -- Computes a new AABB given a set of vertices
   function Compute_AABB (
      Vertices : Vertex_Set
   ) return AABB
   is
      Min         : Vector2 := (X => Float'Last, Y => Float'Last);
      Max         : Vector2 := (X => Float'First, Y => Float'First);
      Center      : Vector2;
      Half_Size   : Vector2;
   begin
      for V of Vertices loop
         -- Compute left and top edges
         Min.X := Float'Min(Min.X, V.X);
         Min.Y := Float'Min(Min.Y, V.Y);

         -- Compute right and bottom edges
         Max.X := Float'Max(Max.X, V.X);
         Max.Y := Float'Max(Max.Y, V.Y);
      end loop;

      Center      := (Min + Max) * 0.5;
      Half_Size   := (Max - Min) * 0.5;

      return (Center, Half_Size);
   end;


   -- Reports whether or not two AABBs are overlapped
   function Is_Overlapping (
      Box1 : AABB;
      Box2 : AABB
   ) return Boolean is
   begin
      return (
             abs(Box1.Center.X - Box2.Center.X) <= (Box1.Half_Size.X + Box2.Half_Size.X)
         and abs(Box1.Center.Y - Box2.Center.Y) <= (Box1.Half_Size.Y + Box2.Half_Size.Y)
      );
   end;

   -- Get the center of an AABB
   function Get_Center (Box : AABB) return Vector2 is
   begin
      return Box.Center;
   end;

   -- Get the half-size of an AABB
   function Get_Half_Size (Box : AABB) return Vector2 is
   begin
      return Box.Half_Size;
   end;