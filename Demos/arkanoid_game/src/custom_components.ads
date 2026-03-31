with ECS.Components; use ECS.Components;
--  with GameMath;
with Math.Linear_Algebra;
package Custom_Components is
   type Ball_State_T is new Component_T with record
      Ball_Launched : Boolean;
      Previous_Pos : Math.Linear_Algebra.Vector2;
   end record;

   type Brick_Attributes is new Component_T with record
      Hits : Integer;
      Score : Integer;
      Indestructable : Boolean;
   end record;

end Custom_Components;
