-- ecs-components_ball.ads
--
-- Ball component for the ECS system.
-- Marks an entity as a ball and defines its physics behavior.
--
-- Usage in Arkanoid:
--   - One or more entities with this component represent balls
--   - Ball Physics System handles bouncing and speed constraints
--   - Collision System detects when ball hits paddle/bricks/walls

with ECS.Components;          use ECS.Components;

package ECS.Components_Ball is

   -- Ball component: Bouncing projectile with physics constraints
   type Ball is new Component with record
      
      -- Speed limits                        ** TODO: Should these be globals? **
      Min_Speed   : Float  := 200.0;         -- Minimum speed (prevent ball from stopping)
      Max_Speed   : Float  := 800.0;         -- Maximum speed (prevent ball from going too fast)
      Base_Speed  : Float  := 400.0;         -- Standard speed after launch
      
      -- Bounce behavior
      Bounce_Damping : Float  := 1.0;        -- Speed multiplier after bounce (1.0 = no damping)
                                             -- Not sure if Arkanoid will need this, but might be nice to have.
      
      -- Ball state
      Is_Attached : Boolean   := True;       -- Is ball stuck to paddle before launch?
                                                -- Newly spawned balled will need to turn this off.

      Attach_Offset_X : Float := 0.0;        -- Offset from paddle center when attached

      -- TODO: Mess with these values (especially speed) to get the feeling right ;)
      
   end record;

end ECS.Components_Ball;
