-- ecs-components_motion.ads
--
-- Motion component for the ECS system.
-- Defines an entity's linear and angular velocities and accelerations.
--
-- Usage:
--   - Entities with Motion are affected by the Movement System
--   - Linear velocity moves the entity's position each frame
--   - Angular velocity rotates the entity each frame
--   - Accelerations are integrated into velocities by Movement System
--
-- Physics note:
--   The Movement System performs explicit Euler integration:
--     velocity += acceleration * dt
--     position += velocity * dt
--   This is simple and sufficient for Arkanoid-style gameplay.
--
-- Related Systems:
--   - Movement System: integrates acceleration → velocity → position
--   - Paddle Control System: sets paddle velocity based on input
--   - Ball Physics System: clamps ball velocity to min/max speed
--   - Collision System: reflects velocity when entities bounce

with ECS.Components;          use ECS.Components;
with Math.Linear_Algebra;     use Math.Linear_Algebra;

package ECS.Components_Motion is

   type Motion is new Component with record
      Linear_Velocity       : Vector2;
      Angular_Velocity      : Vector2;
      Linear_Acceleration   : Vector2;
      Angular_Acceleration  : Vector2;
   end record;

end ECS.Components_Motion;
