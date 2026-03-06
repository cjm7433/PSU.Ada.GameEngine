-- arkanoid.ads
--
-- ARKANOID GAME DEMO IMPLEMENTATION
-- =============================
-- This implements the Game_Interface from game_engine.ads
-- with all Arkanoid-specific logic.
--
--
-- GAME STATES:
-- ------------
-- Playing:    Normal gameplay
-- Paused:     Systems don't update, input frozen
-- Game_Over:  Ball fell off bottom, waiting for restart
-- Victory:    All bricks destroyed, waiting for next level (or restart)
--
-- USAGE:
-- ------
-- This is instantiated in main.adb and passed to the engine:
--
--    procedure Main is
--       Engine : Game_Engine.Engine;
--       Game   : Arkanoid.Arkanoid_Game;
--    begin
--       Engine.Initialize (800, 600, "Arkanoid");
--       Engine.Run (Game);
--    end Main;

with Game_Engine;
with ECS.Manager;
with Win32.Windef;

package Arkanoid is

   ------------------------------------------------------------
   -- GAME STATE ENUM
   ------------------------------------------------------------
   type Game_State is (
      Playing,     -- Normal gameplay
      Paused,      -- Frozen (ESC pressed)
      Game_Over,   -- Ball lost, waiting for restart
      Victory      -- Level complete
   );


   ------------------------------------------------------------
   -- Arkanoid_Game
   -- Implements Game_Interface to work with the engine.
   ------------------------------------------------------------
   type Arkanoid_Game is new Game_Engine.Game_Interface with private;
   
   
   ------------------------------------------------------------
   -- Initialize: Create initial scene (paddle, ball, bricks, walls)
   -- Called once at startup by the engine
   ------------------------------------------------------------
   overriding
   procedure Initialize (
      G : in out Arkanoid_Game);
   

   ------------------------------------------------------------
   -- Handle_Input: Read Input module, apply to paddle entity
   -- Called every frame before Update
   ------------------------------------------------------------
   overriding
   procedure Handle_Input (
      G  : in out Arkanoid_Game;
      DT : Float);
   

   ------------------------------------------------------------
   -- Update: Run all systems, check win/lose conditions
   -- Called at fixed 60fps by the engine
   -- DT = 0.016 (16.67ms)
   ------------------------------------------------------------
   overriding
   procedure Update (
      G  : in out Arkanoid_Game;
      DT : Float);
   

   ------------------------------------------------------------
   -- Render: Draw all entities to screen
   -- Called at display refresh rate (may be faster than Update)
   -- DC = Windows device context to draw to
   ------------------------------------------------------------
   overriding
   procedure Render (
      G  : in out Arkanoid_Game;
      DC : Win32.Windef.HDC);


private

   type Arkanoid_Game is new Game_Engine.Game_Interface with record
      
      -- ECS Manager: Owns all entities, components, systems
      Manager : ECS.Manager.ECS_Manager;
      
      -- Game state tracking
      State   : Game_State := Playing;
      Score   : Natural := 0;          -- TODO: Add score
      Lives   : Natural := 3;          --       Lives
      Level   : Positive := 1;         --       Levels?
      
      -- Entity IDs for quick access (avoid repeated queries)
      -- We cache these after creating them in Initialize
      Paddle_Entity : ECS.Entities.Entity_ID := 0;
      Ball_Entity   : ECS.Entities.Entity_ID := 0;
      
   end record;
   
   
   --===========================================================
   -- PRIVATE HELPERS
   --===========================================================
   
   ------------------------------------------------------------
   -- Load_Level
   -- Create the initial game scene
   -- Called by Initialize to set up paddle, ball, bricks, walls
   ------------------------------------------------------------
   procedure Load_Level (
      G     : in out Arkanoid_Game;
      Level : Positive := 1);
   

   ------------------------------------------------------------
   -- Is_Level_Complete
   -- Check if level is complete (all bricks destroyed)
   ------------------------------------------------------------
   function Is_Level_Complete (
      G : Arkanoid_Game
   ) return Boolean;
   

   ------------------------------------------------------------
   -- Is_Ball_Lost
   -- Check if ball fell off bottom (game-over condition)
   ------------------------------------------------------------
   function Is_Ball_Lost (
      G : Arkanoid_Game
   ) return Boolean;

end Arkanoid;
