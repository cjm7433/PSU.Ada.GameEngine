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
      Move_Speed : Float := 100.0;

      -- Fixed vertical position of the paddle in world space.
      -- Set once at entity creation time and used by Paddle_Control_System
      -- to clamp T.Position.Y each frame, preventing the paddle from being
      -- pushed off its row by collision resolution.
      Home_Y : Float := 228.0;

      -- TODO: Delete in favor of an input component
      Move_Left  : Boolean := False;
      Move_Right : Boolean := False;

   end record;

end ECS.Components.Paddle;
