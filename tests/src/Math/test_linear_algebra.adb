with Linear_Algebra; use Linear_Algebra;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Linear_Algebra is
   G : Generator;


   procedure Initialize is
   begin
      Reset(G);
   end;


   procedure Test_Put is
      X : constant Float   := Random(G);
      Y : constant Float   := Random(G);
      V : constant Vector2 := (X, Y);
   begin
      Put_Line("        Test_Put    " & V'Image);
   end;


   procedure Test_Angle is
      Angle :     constant Float    := Random(G) * Pi * 2.0;
      V :         constant Vector2  := Vector2_From_Polar(Angle);
      Is_Equal :  constant Boolean  := Is_Equal_Approximate(Angle, V.Angle);
      Status :    constant String   := (if Is_Equal then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Angle    " & Angle'Image & " ~ " & V.Angle'Image);
   end;


   procedure Test_Angle_Zero_Vector is
      V :         constant Vector2  := (0.0, 0.0);
      Is_Zero :  constant Boolean   := V.Angle = 0.0;
      Status :    constant String   := (if Is_Zero then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Angle_Zero_Vector    " & V.Angle'Image);
   end;


   procedure Test_Comparison is
      Angle :        constant Float    := Random(G) * Pi * 2.0;
      V :            constant Vector2  := Vector2_From_Polar(Angle);
      V_Double :     constant Vector2  := V * 2.0;
      V_Half :       constant Vector2  := V / 2.0;
      Inequality :   constant Boolean  := V_Half < V and V < V_Double;
      Status :       constant String   := (if Inequality then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Comparison    " & V_Half.Length'Image & " < " & V.Length'Image & " < " & V_Double.Length'Image);
   end;


   procedure Test_Distance is
      Angle :        constant Float    := Random(G) * Pi * 2.0;
      V :            constant Vector2  := Vector2_From_Polar(Angle) * (Random(G) + 1.0) * 10.0;
      V_Inverse :    constant Vector2  := -V;
      Distant :      constant Boolean  := Is_Equal_Approximate(V.Distance(V_Inverse), V.Length * 2.0);
      Status :       constant String   := (if Distant then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Distance    " & V'Image & " & " & V_Inverse'Image & " = " & V.Distance(V_Inverse)'Image);
   end;


   procedure Test_Rotation is
      Angle :     constant Float    := Random(G) * Pi * 2.0;
      Offset :    constant Float    := Random(G) * Pi * 2.0;
      V :         constant Vector2  := Vector2_From_Polar(Angle);
      V_Rotated : constant Vector2  := V.Rotate(Offset);
      Is_Equal :  constant Boolean  := Is_Equal_Approximate(V_Rotated.Angle, Rotate(Angle, Offset));
      Status :    constant String   := (if Is_Equal then "PASS" else "FAIL");
   begin
      Put_Line(Status & "    Test_Rotation    " & V.Angle'Image & " + " & Offset'Image & " = " & V_Rotated.Angle'Image);
   end;
end Test_Linear_Algebra;