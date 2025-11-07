with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;


package body Linear_Algebra is
   -- Pushes the Vector2 data onto the root buffer as a custom formatted String
   -- Formats as (X, Y)
   procedure Put_Image_Vector2 (
         Buffer  : in out Root_Buffer_Type'Class;
         Arg     :        Vector2
   ) is
      Output : constant String := "(" & Arg.X'Image & ", " & Arg.Y'Image & ")";
   begin
      Buffer.Put(Output);
   end Put_Image_Vector2;


   -- Cartesian constructor
   function Vector2 (
         X, Y : Float := 0.0
   ) return Vector2 is
   begin
      return (X => X, Y => Y);
   end Vector2;


   -- Polar constructor (radians)
   function Vector2_Polar (
         Angle       : Float := 0.0;
         Magnitude   : Float := 1.0
   ) return Vector2 is
   begin
      return (X => Magnitude * Cos(Angle), Y => Magnitude * Sin(Angle));
   end Vector2;


   -- Exact equality
   function "=" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1 = V2;
   end "=";


   -- Exact inequality
   function "/=" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1 /= V2;
   end "/=";


   -- Approximate equality
   function "==" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return Abs(V1.X - V2.X) <= Epsilon and Abs(V1.Y - V2.Y) <= Epsilon;
   end "==";


   -- Approximate inequality
   function "=/=" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return not (V1 == V2);
   end "=/=";


   -- Magntitude greater than
   function ">" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1.Length_Squared > V2.Length_Squared;
   end ">";


   -- Magnitude greater than or exactly equal
   function ">=" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1.Length_Squared >= V2.Length_Squared;
   end ">=";


   -- Magnitude less than
   function "<" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1.Length_Squared < V2.Length_Squared;
   end "<";


   -- Magnitude less than or exactly equal
   function "<=" (
         V1, V2 : Vector2
   ) return Boolean is
   begin
      return V1.Length_Squared <= V2.Length_Squared;
   end "<=";


   -- Component-wise addition
   function "+" (
         V1, V2 : Vector2
   ) return Vector2 is
   begin
      return (X => V1.X + V2.X, Y => V1.Y + V2.Y);
   end "+";


   -- Component-wise subtraction
   function "-" (
         V1, V2 : Vector2
   ) return Vector2 is
   begin
      return (X => V1.X - V2.X, Y => V1.Y - V2.Y);
   end "-";


   -- Negation
   function "-" (
         V : Vector2
   ) return Vector2 is
   begin
      return (X => -V.X, Y => -V.Y);
   end "-";


   -- Component-wise multiplication
   function "*" (
         V1, V2 : Vector2
   ) return Vector2 is
   begin
      return (X => V1.X * V2.X, Y => V1.Y * V2.Y);
   end "*";


   -- Magnitude Integer multiplication
   function "*" (
         V : Vector2;
         S : Integer
   ) return Vector2 is
   begin
      return (X => V.X * S, Y => V.Y * S);
   end "*";


   -- Magnitude Float multiplication
   function "*" (
         V : Vector2;
         S : Float
   ) return Vector2 is
   begin
      return (X => V.X * S, Y => V.Y * S);
   end "*";


   -- Component-wise division
   function "/" (
         V1, V2 : Vector2
   ) return Vector2 is
   begin
      return (X => V1.X / V2.X, Y => V1.Y / V2.Y);
   end "/";


   -- Magnitude Integer division
   function "/" (
         V : Vector2;
         S : Integer
   ) return Vector2 is
   begin
      return (X => V.X / S, Y => V.Y / S);
   end "/";


   -- Magnitude Float division
   function "/" (
         V : Vector2;
         S : Float
   ) return Vector2 is
   begin
      return (X => V.X / S, Y => V.Y / S);
   end "/";


   -- Normalization
   function Normalize (
         V : Vector2
   ) return Vector2 is
   begin
      if V.Length = 0.0 then
         -- Return arbitrary unit vector if original vector has no magnitude
         return Vector2(1.0, 0.0);
      end if;

      return V / V.Length;
   end Normalize;


   -- Magnitude
   function Length (
         V : Vector2
   ) return Float is
   begin
      return Sqrt(V.X ** 2 + V.Y ** 2);
   end Length;


   -- Magnitude squared (faster calculation; useful for comparisons)
   function Length_Squared (
         V : Vector2
   ) return Float is
   begin
      return V.X ** 2 + V.Y ** 2;
   end Length_Squared;


   -- Rotation (radians)
   function Rotate (
         V     : Vector2;
         Angle : Float
   ) return Vector2 is
      X_Rotated : Float := V.X * Cos(Angle) - V.Y * Sin(Angle);
      Y_Rotated : Float := V.X * Sin(Angle) + V.Y * Cos(Angle);
   begin
      return (X => X_Rotated, Y => Y_Rotated);
   end Rotate;


   -- Distance
   function Distance (
         V1 : Vector2;
         V2 : Vector2
   ) return Float is
      Relative : Vector2 := V1 - V2;
   begin
      return Relative.Length;
   end Distance;


   -- Distance squared (faster calculation; useful for comparisons)
   function Distance_Squared (
         V1 : Vector2;
         V2 : Vector2
   ) return Float is
      Relative : Vector2 := V1 - V2;
   begin
      return Relative.Length_Squared;
   end Distance_Squared;


   -- Angle
   function Angle (
         V : Vector2
   ) return Float is
   begin
      return Atan2(V.Y, V.X);
   end Angle;
end Linear_Algebra;