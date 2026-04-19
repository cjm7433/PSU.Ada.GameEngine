with Test_Linear_Algebra;
with Test_Physics;

procedure Tests is
begin
   Test_Linear_Algebra.Initialize;
   Test_Linear_Algebra.Test_Put;
   Test_Linear_Algebra.Test_Angle;
   Test_Linear_Algebra.Test_Angle_Zero_Vector;
   Test_Linear_Algebra.Test_Comparison;
   Test_Linear_Algebra.Test_Distance;
   Test_Linear_Algebra.Test_Rotation;
   Test_Linear_Algebra.Test_Arithmetic;

   Test_Physics.Initialize;
   Test_Physics.Test_Put;
   Test_Physics.Test_Bounding;
   Test_Physics.Test_MinMax;
   Test_Physics.Test_AANormal;
   Test_Physics.Test_Overlap;
end Tests;
