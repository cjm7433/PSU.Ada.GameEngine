-- ecs-components_brick.ads
--
-- Brick component for the ECS system.
-- Marks an entity as a destructible brick with health and point value.
--
-- Usage in Arkanoid:
--   - Each brick entity has this component
--   - Brick Destruction System handles damage and scoring
--   - Different brick types have different health/points

with ECS.Components;          use ECS.Components;

package ECS.Components_Brick is

   -- Brick types for different behaviors/visuals
   type Brick_Type is (
      Normal,        -- Standard brick (1 hit)
      Strong,        -- Requires multiple hits
      Unbreakable,   -- Cannot be destroyed
      Special        -- May drop power-ups?
   );

   -- Brick component: Destructible block with health and scoring
   type Brick is new Component with record
      
      -- Brick properties
      Brick_Kind  : Brick_Type := Normal;
      
      -- Health (hits required to destroy)
      Health      : Natural := 1;
      Max_Health  : Natural := 1;
      
      -- Points awarded when destroyed
      Points      : Natural := 10;
      
      -- Is this brick currently being destroyed? (for animation)
      Is_Dying    : Boolean   := False;
      Death_Timer : Float     := 0.0;         -- Time remaining in death animation
      
   end record;

end ECS.Components_Brick;
