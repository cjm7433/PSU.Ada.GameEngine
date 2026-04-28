package ECS.Components.Render is
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


   type Render_Component is new Component with record
      Shape : Shape_Type := Rectangle;
      Tint : Color := (1.0, 1.0, 1.0, 1.0);

      -- Render layer (for draw order)
      -- Lower numbers drawn first (background), higher numbers on top (foreground)
      Layer : Natural := 0;

      Visible : Boolean := True;

      -- TODO: Future: Sprite/texture reference

   end record;
end;
