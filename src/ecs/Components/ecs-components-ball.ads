-- TODO: Migrate to Arkanoid. This is a game-specific component and should not be provided by the engine.

package ECS.Components.Ball is
   type Ball_Component is new Component with record
      -- Speed limits
      Min_Speed   : Float  := 100.0;          -- Minimum speed (prevent ball from stopping)
      Max_Speed   : Float  := 500.0;          -- Maximum speed (prevent ball from going too fast)
      Base_Speed  : Float  := 125.0;          -- Standard speed after launch

      -- Bounce behavior
      Bounce_Damping : Float  := 1.0;        -- Speed multiplier after bounce (1.0 = no damping)

      -- Ball state
      Is_Attached : Boolean   := True;       -- Is ball stuck to paddle before launch?
                                                -- Newly spawned balled will need to turn this off.

      Attach_Offset_X : Float := 0.0;        -- Offset from paddle center when attached
      Attach_Offset_Y : Float := -10.0;      -- Offset above paddle when attached (negative = upward)

      -- Paddle hit flag:
      -- Set to True by the Collision System when the ball resolves a collision
      -- with the paddle. Read and cleared each frame by the Ball Physics System
      -- to apply Arkanoid zone-based paddle deflection.
      Hit_Paddle : Boolean := False;
   end record;
end;
