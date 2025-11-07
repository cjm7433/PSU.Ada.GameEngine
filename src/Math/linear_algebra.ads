with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;


package Linear_Algebra is
   -- Two-dimensional vector type
   type Vector2 is tagged record
      X : Float;
      Y : Float;
   end record with Put_Image => Put_Image_Vector2;


   Epsilon: Float := 1.0E-6;  -- Tolerance fidelity for vector approximation


   -- Pushes the Vector2 data onto the root buffer as a custom formatted String
   procedure Put_Image_Vector2 (
         Buffer  : in out Root_Buffer_Type'Class;
         Arg     :        Vector2
   );



   -- CONSTRUCTORS
      -- Cartesian constructor
      function Vector2 (
            X, Y : Float := 0.0
      ) return Vector2;


      -- Polar constructor (radians)
      function Vector2_Polar (
            Angle       : Float := 0.0;
            Magnitude   : Float := 1.0
      ) return Vector2;



   -- COMPARISON
      -- Exact equality
      function "=" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Exact inequality
      function "/=" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Approximate equality
      function "==" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Approximate inequality
      function "=/=" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Magntitude greater than
      function ">" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Magnitude greater than or exactly equal
      function ">=" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Magnitude less than
      function "<" (
            V1, V2 : Vector2
      ) return Boolean;


      -- Magnitude less than or exactly equal
      function "<=" (
            V1, V2 : Vector2
      ) return Boolean;



   -- ARITHMETIC
      -- Component-wise addition
      function "+" (
         V1, V2 : Vector2
      ) return Vector2;


      -- Component-wise subtraction
      function "-" (
         V1, V2 : Vector2
      ) return Vector2;


      -- Negation
      function "-" (
         V : Vector2
      ) return Vector2;


      -- Component-wise multiplication
      function "*" (
         V1, V2 : Vector2
      ) return Vector2;


      -- Magnitude Integer multiplication
      function "*" (
         V : Vector2;
         S : Integer
      ) return Vector2;


      -- Magnitude Float multiplication
      function "*" (
         V : Vector2;
         S : Float
      ) return Vector2;


      -- Component-wise division
      function "/" (
         V1, V2 : Vector2
      ) return Vector2;


      -- Magnitude Integer division
      function "/" (
         V : Vector2;
         S : Integer
      ) return Vector2;


      -- Magnitude Float division
      function "/" (
         V : Vector2;
         S : Float
      ) return Vector2;



   -- ALGEBRA
      -- Normalization
      function Normalize (
            V : Vector2
      ) return Vector2;


      -- Magnitude
      function Length (
            V : Vector2
      ) return Float;


      -- Magnitude squared (faster calculation; useful for comparisons)
      function Length_Squared (
            V : Vector2
      ) return Float;


      -- Rotation (radians)
      function Rotate (
            V     : Vector2;
            Angle : Float
      ) return Vector2;


      -- Distance
      function Distance (
            V1 : Vector2;
            V2 : Vector2
      ) return Float;


      -- Distance squared (faster calculation; useful for comparisons)
      function Distance_Squared (
            V1 : Vector2;
            V2 : Vector2
      ) return Float;


      -- Angle
      function Angle (
            V : Vector2
      ) return Float;
end Linear_Algebra;