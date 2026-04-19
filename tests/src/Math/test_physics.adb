with Math.Linear_Algebra;  use Math.Linear_Algebra;
with Math.Physics.AABBs;   use Math.Physics.AABBs;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Physics is
   G : Generator;


   procedure Initialize is
   begin
      Reset(G);
   end;


   procedure Test_Put is
      C :         constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0);
      S :         constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0);
      B :         constant AABB     := (Center => C, Half_Size => S);   begin
      Put_Line("        Test_Put    " & B'Image);
   end;


   procedure Test_Bounding is
      O :         constant Vector2  := (10.0, 10.0);
      V0 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0, Random(G) * 10.0);
      V1 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0, Random(G) * 10.0);
      V2 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0, Random(G) * 10.0);
      V3 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0, Random(G) * 10.0);
      VS :        constant Vertex_Set_Access := new Vertex_Set'
                     (
                        0 => V0,
                        1 => V1,
                        2 => V2,
                        3 => V3
                     );
      B :         constant AABB     := From_Vertices(Position => (0.0, 0.0), Vertices_Access => VS);
      Bounded :   constant Boolean  := (B.Half_Size <= O);
      Status :    constant String   := (if Bounded then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Bounding    " & B.Half_Size'Image & " <= " & O'Image);
   end;


   procedure Test_MinMax is
      S :         constant Vector2  := Vector2_From_Polar(Random(G) * Pi * 2.0, Random(G) * 10.0);
      B :         constant AABB     := (Center => (0.0, 0.0), Half_Size => S);
      Corner :    constant Boolean  := (Get_Min(B) = -S and Get_Max(B) = S);
      Status :    constant String   := (if Corner then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_MinMax    " & Get_Min(B)'Image & ", " & Get_Max(B)'Image & "  |  " & S'Image);
   end;


   procedure Test_AANormal is
      P1 :        constant Vector2   := Vector2_From_Polar(Random(G) * Pi * 2.0, 10.0);
      B1 :        constant AABB     := (Center => P1, Half_Size => (1.0, 1.0));
      B2 :        constant AABB     := (Center => (0.0, 0.0), Half_Size => (1.0, 1.0));
      X_Greater : constant Boolean  := (abs(P1.X) > abs(P1.Y));
      Normal :    constant Vector2  := Get_Aligned_Normal(B2, B1);
      Aligned :   constant Boolean  := (
            (     X_Greater and Normal = (Sign(P1.X), 0.0))
         or (not  X_Greater and Normal = (0.0, Sign(P1.Y)))
      );
      Status :    constant String   := (if Aligned then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_AANormal    " & P1'Image & " => " & Normal'Image);
   end;


   procedure Test_Overlap is
      D  :        constant Float    := 12.0;
      S1 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi / 2.0, 10.0);
      B1 :        constant AABB     := (Center => (0.0, 0.0), Half_Size => S1);
      S2 :        constant Vector2  := Vector2_From_Polar(Random(G) * Pi / 2.0, 10.0);
      B2 :        constant AABB     := (Center => (D, 0.0), Half_Size => S2);
      Overlap :   constant Boolean  := Is_Overlapping(B1, B2);
      Expected :  constant Boolean  := S1.X + S2.X > D;
      Status :    constant String   := (if Overlap = Expected then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Overlap    " & S1'Image & " & " & S2'Image & " = " & Overlap'Image & " (" & D'Image & ")");
   end;
end Test_Physics;