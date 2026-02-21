-- TODO: Migrate to Arkanoid. This is a game-specific component and should not be provided by the engine.

package ECS.Components.Ball is
   type Ball_Component is new Component with record
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
   end record;
end;
