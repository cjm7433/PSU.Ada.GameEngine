-- ecs-components_transform.ads
--
-- Transform component for the ECS system.
-- Defines an entity's spatial properties: position, rotation, and scale.
--
-- Usage:
--   - Nearly every visible entity has a Transform
--   - Position is in world coordinates (2D vector)
--   - Rotation is in radians (0 to 2π)
--   - Scale is a 2D multiplier (1.0 = normal size)
--
-- Related Systems:
--   - Movement System: integrates velocity into position
--   - Render System: reads position/rotation/scale for drawing
--   - Collision System: uses position to compute overlap

with ECS.Components;          use ECS.Components;
with Math.Linear_Algebra;     use Math.Linear_Algebra;

package ECS.Components_Transform is

   type Transform is new Component with record
      Position : Vector2;
      Rotation : Float;
      Scale    : Vector2;
   end record;

end ECS.Components_Transform;
