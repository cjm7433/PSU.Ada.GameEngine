with Math.Linear_Algebra; use Math.Linear_Algebra;


package body Math.Physics.AABBs is
   -- Returns an AABB computed from a set of vertices
   function From_Vertices (
      Position          : Vector2;
      Vertices_Access   : Vertex_Set_Access
   ) return AABB
   is
      Min         : Vector2 := (X => Float'Last, Y => Float'Last);
      Max         : Vector2 := (X => Float'First, Y => Float'First);
      Center      : Vector2;
      Half_Size   : Vector2;
      Vertices    : Vertex_Set := Vertices_Access.all;
      Global_V    : Vector2;
   begin
      for V of Vertices loop
         Global_V := V + Position;

         -- Compute left and top edges
         Min.X := Float'Min(Min.X, Global_V.X);
         Min.Y := Float'Min(Min.Y, Global_V.Y);

         -- Compute right and bottom edges
         Max.X := Float'Max(Max.X, Global_V.X);
         Max.Y := Float'Max(Max.Y, Global_V.Y);
      end loop;

      Center      := (Min + Max) * 0.5;
      Half_Size   := (Max - Min) * 0.5;

      return (Center, Half_Size);
   end;


   -- Returns the minimum vector in the AABB
   function Get_Min (Box : AABB) return Vector2 is
   begin
      return Box.Center - Box.Half_Size;
   end;


   -- Returns the maximum vector in the AABB
   function Get_Max (Box : AABB) return Vector2 is
   begin
      return Box.Center + Box.Half_Size;
   end;


   -- Returns the axis-aligned normal vector between two AABBs
   function Get_Aligned_Normal (
         Box_From : AABB;
         Box_To   : AABB
   ) return Vector2 is
      Delta_Vector      : Vector2 := Box_To.Center - Box_From.Center;
      Separation_Vector : Vector2 := (
            abs(Delta_Vector.X) - (Box_From.Half_Size.X + Box_To.Half_Size.X),
            abs(Delta_Vector.Y) - (Box_From.Half_Size.Y + Box_To.Half_Size.Y)
      );
      Normal : Vector2;
   begin
      if Separation_Vector.X > Separation_Vector.Y then
         -- Dominant X axis
         if Delta_Vector.X > 0.0 then
            Normal := (1.0, 0.0);
         else
            Normal := (-1.0, 0.0);
         end if;
      else
         -- Dominant Y axis
         if Delta_Vector.Y > 0.0 then
            Normal := (0.0, 1.0);
         else
            Normal := (0.0, -1.0);
         end if;
      end if;

      return Normal;
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


   -- Reports at what fractional part along a motion vector collision between two AABBs occurred.
   function Collision_Sweep (
         Box1     : AABB;
         Box2     : AABB;
         Motion   : Vector2
   ) return Float is
      Min1        : Vector2 := Box1.Get_Min;
      Max1        : Vector2 := Box1.Get_Max;
      Min2        : Vector2 := Box2.Get_Min;
      Max2        : Vector2 := Box2.Get_Max;

      Entry_Pos       : Vector2;
      Exit_Pos        : Vector2;

      Fraction_Entry_X : Float := Float'First;
      Fraction_Entry_Y : Float := Float'First;
      Fraction_Exit_X  : Float := Float'Last;
      Fraction_Exit_Y  : Float := Float'Last;

      Entry_Time  : Float;
      Exit_Time   : Float;
   begin
      -- Compute entry/exit distances
      if Motion.X > 0.0 then
         Entry_Pos.X := Min2.X - Max1.X;
         Exit_Pos.X  := Max2.X - Min1.X;
      else
         Entry_Pos.X := Max2.X - Min1.X;
         Exit_Pos.X  := Min2.X - Max1.X;
      end if;
      if Motion.Y > 0.0 then
         Entry_Pos.Y := Min2.Y - Max1.Y;
         Exit_Pos.Y  := Max2.Y - Min1.Y;
      else
         Entry_Pos.Y := Max2.Y - Min1.Y;
         Exit_Pos.Y  := Min2.Y - Max1.Y;
      end if;

      -- Compute entry/exit times
      if not Is_Equal_Approximate(Motion.X, 0.0) then
         Fraction_Entry_X := Entry_Pos.X / Motion.X;
         Fraction_Exit_X  := Exit_Pos.X  / Motion.X;
      end if;
      if not Is_Equal_Approximate(Motion.Y, 0.0) then
         Fraction_Entry_Y := Entry_Pos.Y / Motion.Y;
         Fraction_Exit_Y  := Exit_Pos.Y  / Motion.Y;
      end if;

      Entry_Time := Float'Max(Fraction_Entry_X, Fraction_Entry_Y);
      Exit_Time  := Float'Min(Fraction_Exit_X,  Fraction_Exit_Y);

      -- No collision
      if Entry_Time > Exit_Time
         or else Entry_Time < 0.0
         or else Entry_Time > 1.0
      then
         return Float'Last;
      end if;

      return Entry_Time;
   end;
end;
