-- input.ads
--
-- Game-side input module for Arkanoid.
-- Translates raw engine key events into named boolean flags that the
-- game loop can read each frame.
--
-- Design notes:
--   - This module is intentionally game-specific and lives alongside
--     arkanoid.adb rather than inside the engine library.  The engine
--     emits platform-agnostic KeyDown / KeyUp events; this module
--     decides what those key codes mean for Arkanoid.
--   - On Windows the key codes are Win32 Virtual Key (VK) values,
--     which is what window.adb places in Event_T.Data.KeyCode via
--     WM_KEYDOWN / WM_KEYUP.
--   - On Linux (future): the same interface is used; only the VK
--     constants below need a companion set of X11 / evdev values
--     selected at compile time via a platform discriminant.
--     TODO: add Linux key code constants when Linux support is added.

with ECS.Event; use ECS.Event;

package Input is

   -- -----------------------------------------------------------------------
   -- Win32 Virtual Key constants for the keys Arkanoid cares about.
   -- Source: https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
   -- -----------------------------------------------------------------------
   VK_Left   : constant ECS.Event.Byte := 16#41#;   -- A
   VK_Right  : constant ECS.Event.Byte := 16#44#;   -- D
   VK_Q      : constant ECS.Event.Byte := 16#51#;   -- Q
   VK_Space  : constant ECS.Event.Byte := 16#20#;   -- Spacebar
   VK_Escape : constant ECS.Event.Byte := 16#1B#;   -- Escape
   VK_P      : constant ECS.Event.Byte := 16#50#;   -- P key
   VK_F      : constant ECS.Event.Byte := 16#46#;   -- F key


   -- Linux Bindings
   LK_Left   : constant ECS.Event.Byte := 16#61#;   -- A
   LK_Right  : constant ECS.Event.Byte := 16#64#;   -- D
   LK_Q      : constant ECS.Event.Byte := 16#71#;   -- Q
   LK_Space  : constant ECS.Event.Byte := 16#1B#;   -- Spacebar
   LK_Escape : constant ECS.Event.Byte := 16#0D#;   -- Escape
   LK_P      : constant ECS.Event.Byte := 16#70#;   -- P key
   LK_F      : constant ECS.Event.Byte := 16#66#;   -- F key

   -- -----------------------------------------------------------------------
   -- Input_State
   -- Holds the current pressed / released status of every key Arkanoid
   -- cares about.  The game loop reads these flags each frame.
   -- -----------------------------------------------------------------------
   type Input_State is record
      Left   : Boolean := False;   -- Left arrow held
      Right  : Boolean := False;   -- Right arrow held
      Space  : Boolean := False;   -- Spacebar pressed this frame (one-shot)
      Escape : Boolean := False;   -- Escape pressed this frame (one-shot)
      P      : Boolean := False;   -- P pressed this frame (one-shot)
      F      : Boolean := False;   -- F pressed this frame (one-shot)
      Q      : Boolean := False;   -- Q pressed this frame (one-shot)
   end record;

   -- Current input state; read directly by the game loop.
   State : Input_State;

   -- -----------------------------------------------------------------------
   -- Handle_Event
   -- Call once per event drained from the engine event queue each frame.
   -- Updates State based on KeyDown / KeyUp event types and key codes.
   -- -----------------------------------------------------------------------
   procedure Handle_Event (Event : ECS.Event.Event_T);

   -- -----------------------------------------------------------------------
   -- Clear_Frame_Flags
   -- Clears one-shot flags (Space, Escape) that should only be acted on
   -- for a single frame.  Call at the END of each game loop iteration
   -- after the game logic has already consumed them.
   -- -----------------------------------------------------------------------
   procedure Clear_Frame_Flags;

   -- -----------------------------------------------------------------------
   -- Reset
   -- Clears all input flags to unpressed.  Call when resetting the game.
   -- -----------------------------------------------------------------------
   procedure Reset;

end Input;