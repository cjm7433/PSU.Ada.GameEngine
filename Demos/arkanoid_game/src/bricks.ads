with ECS.Components; use ECS.Components;
with ECS.Store; use ECS.Store;
with ECS.Entities; use ECS.Entities;
with ECS.Components.Transform; use ECS.Components.Transform;
with Graphics.Color; use Graphics.Color;
with Math.Linear_Algebra; use Math.Linear_Algebra;

package Bricks is
------------------procedure Add_Brick(
--   Manager : access ECS.Manager.ECS_Manager'Class; 
--   Position : Vector2; 
--   Brick_Type : Integer;  
--   Texture : Texture_Access
--   );

procedure Add_Brick(
   S : in out Store;
   P : Vector2; 
   Brick_Type : Integer;  
   C : Color
   );

end Bricks;
