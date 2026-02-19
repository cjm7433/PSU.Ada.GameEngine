-- ecs-components_renderable.ads
--
-- Renderable component for the ECS system.
-- Defines how an entity should be drawn on screen.
--
-- Usage in Arkanoid:
--   - All visible entities (paddle, ball, bricks, walls) have this
--   - Render System uses this to draw entities
--   - Can be extended later to support sprites/textures

with ECS.Components;          use ECS.Components;

package ECS.Components_Renderable is

   -- Render shape types
   type Shape_Type is (
      Rectangle,              -- Filled rectangle (most common for Arkanoid)
      Circle,                 -- Filled circle (for ball)
      Sprite                  -- Texture/sprite (TODO: future enhancement)
   );

   -- Color representation (RGBA)
   type Color is record
      R : Float := 1.0;       -- Red   (0.0 to 1.0)
      G : Float := 1.0;       -- Green (0.0 to 1.0)
      B : Float := 1.0;       -- Blue  (0.0 to 1.0)
      A : Float := 1.0;       -- Alpha (0.0 = transparent, 1.0 = opaque)
   end record;

   -- Renderable component: Visual representation
   type Renderable is new Component with record
      
      -- What shape to draw
      Shape : Shape_Type := Rectangle;
      
      -- Color
      Tint : Color := (1.0, 1.0, 1.0, 1.0);  -- Default: white, opaque
      
      -- Render layer (for draw order)
      -- Lower numbers drawn first (background), higher numbers on top
      -- Example layers:
      --   0 = Background
      --   1 = Walls
      --   2 = Bricks
      --   3 = Paddle
      --   4 = Ball
      --   5 = Particles/Effects
      Layer : Natural := 0;
      
      -- Is this entity visible?
      Visible : Boolean := True;
      
      -- TODO: Future: Sprite/texture reference
      -- Sprite_ID : Natural := 0;
      
   end record;

end ECS.Components_Renderable;
