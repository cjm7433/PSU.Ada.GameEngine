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
    end;


    -- Polar constructor (radians)
    function Vector2_From_Polar (
         Angle       : Float := 0.0;
         Magnitude   : Float := 1.0
    ) return Vector2 is
         Normal : Vector2 := (X => Cos(Angle), Y => Sin(Angle));
    begin
         return Normal * Magnitude;
    end;


    -- Exact equality
    function "=" (
         V1, V2 : Vector2
    ) return Boolean is
    begin
         return V1.X = V2.X and V1.Y = V2.Y;
    end;


    -- Approximate equality
    function Is_Equal_Approximate (
         V1, V2 : Vector2;
         Epsilon : Float := 1.0E-6
    ) return Boolean is
    begin
         return Abs(V1.X - V2.X) <= Epsilon and Abs(V1.Y - V2.Y) <= Epsilon;
    end;


    -- Magntitude greater than
    function ">" (
         V1, V2 : Vector2
    ) return Boolean is
    begin
         return V1.Length_Squared > V2.Length_Squared;
    end;


    -- Magnitude greater than or exactly equal
    function ">=" (
         V1, V2 : Vector2
    ) return Boolean is
    begin
         return V1.Length_Squared >= V2.Length_Squared;
    end;


    -- Magnitude less than
    function "<" (
         V1, V2 : Vector2
    ) return Boolean is
    begin
         return V1.Length_Squared < V2.Length_Squared;
    end;


    -- Magnitude less than or exactly equal
    function "<=" (
         V1, V2 : Vector2
    ) return Boolean is
    begin
         return V1.Length_Squared <= V2.Length_Squared;
    end ;


    -- Component-wise addition
    function "+" (
         V1, V2 : Vector2
    ) return Vector2 is
    begin
         return (X => V1.X + V2.X, Y => V1.Y + V2.Y);
    end;


    -- Component-wise subtraction
    function "-" (
         V1, V2 : Vector2
    ) return Vector2 is
    begin
         return (X => V1.X - V2.X, Y => V1.Y - V2.Y);
    end;


    -- Negation
    function "-" (
         V : Vector2
    ) return Vector2 is
    begin
         return (X => -V.X, Y => -V.Y);
    end;


    -- Component-wise multiplication
    function "*" (
         V1, V2 : Vector2
    ) return Vector2 is
    begin
         return (X => V1.X * V2.X, Y => V1.Y * V2.Y);
    end;


    -- Magnitude Integer multiplication
    function "*" (
         V : Vector2;
         S : Integer
    ) return Vector2 is
    begin
         return (X => V.X * Float(S), Y => V.Y * Float(S));
    end;


    -- Magnitude Float multiplication
    function "*" (
         V : Vector2;
         S : Float
    ) return Vector2 is
    begin
         return (X => V.X * S, Y => V.Y * S);
    end;


    -- Component-wise division
    function "/" (
         V1, V2 : Vector2
    ) return Vector2 is
    begin
         return (X => V1.X / V2.X, Y => V1.Y / V2.Y);
    end;


    -- Magnitude Integer division
    function "/" (
         V : Vector2;
         S : Integer
    ) return Vector2 is
    begin
         return (X => V.X / Float(S), Y => V.Y / Float(S));
    end;


    -- Magnitude Float division
    function "/" (
         V : Vector2;
         S : Float
    ) return Vector2 is
    begin
         return (X => V.X / S, Y => V.Y / S);
    end;


    -- Normalization
    function Normalize (
         V : Vector2
    ) return Vector2 is
    begin
         if V.Length_Squared = 0.0 then
            -- Return zero-length vector if original vector has no magnitude
            return (0.0, 0.0);
         end if;

         return V / V.Length;
    end;


    -- Magnitude
    function Length (
         V : Vector2
    ) return Float is
    begin
         return Sqrt(V.Length_Squared);
    end;


    -- Magnitude squared (faster calculation; useful for comparisons)
    function Length_Squared (
         V : Vector2
    ) return Float is
    begin
         return V.X ** 2 + V.Y ** 2;
    end;


    -- Rotation (radians; counter-clockwise)
    function Rotate (
         V     : Vector2;
         Angle : Float
    ) return Vector2 is
         X_Rotated : Float := V.X * Cos(Angle) - V.Y * Sin(Angle);
         Y_Rotated : Float := V.X * Sin(Angle) + V.Y * Cos(Angle);
    begin
         return (X_Rotated, Y_Rotated);
    end;


    -- Distance
    function Distance (
         V1 : Vector2;
         V2 : Vector2
    ) return Float is
         Relative : Vector2 := V1 - V2;
    begin
         return Relative.Length;
    end;


    -- Distance squared (faster calculation; useful for comparisons)
    function Distance_Squared (
         V1 : Vector2;
         V2 : Vector2
    ) return Float is
         Relative : Vector2 := V1 - V2;
    begin
         return Relative.Length_Squared;
    end;


    -- Angle
    function Angle (
         V : Vector2
    ) return Float is
    begin
         return Arctan(V.Y, V.X);
    end;
end;