with Math.Linear_Algebra; use Math.Linear_Algebra;

package ECS is
  type MousePosition is record
    PreviousPos : Vector2;
    CurrentPos : Vector2;
  end record;

  -- Package level static variables
  MousePos : MousePosition := ((0.0, 0.0), (0.0, 0.0));
  WindowWidth : Integer := 0;
end ECS;
