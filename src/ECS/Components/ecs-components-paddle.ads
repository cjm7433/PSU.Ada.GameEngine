-- ecs-components-paddle.ads
--
-- Paddle component for the ECS system.
-- Marks an entity as the player-controlled paddle and defines its behavior.
--
-- Usage in Arkanoid:
--   - One entity with this component represents the player's paddle
--   - Paddle Control System reads input and updates this entity's Motion
--
-- TODO: Migrate to Arkanoid. This is a game-specific component and should not be provided by the engine.

package ECS.Components.Paddle is

   -- Paddle component: Player-controlled horizontal platform
   type Paddle_Component is new Component with record

      -- Movement speed in units per second
      Move_Speed : Float := 500.0;

      -- Horizontal bounds (keep paddle on screen)
      -- These are world-space X coordinates
      -- TODO: Maybe set them elsewhere or make a global somewhere else idk
      Min_X : Float := 50.0;   -- Left boundary
      Max_X : Float := 750.0;  -- Right boundary

      -- TODO: Delete in favor of an input component
      Move_Left  : Boolean := False;
      Move_Right : Boolean := False;

   end record;

end ECS.Components.Paddle;
