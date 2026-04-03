-- TODO: Migrate to Arkanoid. This is a game-specific component and should not be provided by the engine.

package ECS.Components.Brick is
   -- Brick types for different behaviors/visuals
   type Brick_Type is (
      Normal,        -- Standard brick (1 hit)
      Strong,        -- Requires multiple hits
      Unbreakable,   -- Cannot be destroyed
      Special        -- May drop power-ups?
   );


   -- Brick component: Destructible block with health and scoring
   type Brick_Component is new Component with record
      -- Brick properties
      Brick_Kind  : Brick_Type := Normal;

      -- Health (hits required to destroy)
      Health      : Natural := 1;
      Max_Health  : Natural := 1;

      -- Points awarded when destroyed
      Points      : Natural := 10;

      -- Is this brick currently being destroyed? (for animation)
      Is_Dying      : Boolean   := False;
      Death_Timer   : Float     := 0.5;         -- Time remaining in death animation (seconds)
   end record;
end;
