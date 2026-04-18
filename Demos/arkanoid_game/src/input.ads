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
   VK_Left   : constant ECS.Event.Byte := 16#41#;   -- A (Win VK)
   VK_Right  : constant ECS.Event.Byte := 16#44#;   -- D (Win VK)
   VK_Q      : constant ECS.Event.Byte := 16#51#;   -- Q (Win VK)
   VK_Space  : constant ECS.Event.Byte := 16#20#;   -- Spacebar (Win VK)
   VK_Escape : constant ECS.Event.Byte := 16#1B#;   -- Escape (Win VK)
   VK_P      : constant ECS.Event.Byte := 16#50#;   -- P key (Win VK)
   VK_F      : constant ECS.Event.Byte := 16#46#;   -- F key (Win VK)

   -- Linux/X11 keycodes (the Wayland wrapper currently sends X11-style
   -- keycodes produced by taking the evdev code and adding 8). We accept
   -- both Win VK and these X11 values so controls behave on both platforms.
   LK_A      : constant ECS.Event.Byte := 38;  -- 'A' key (evdev 30 + 8)
   LK_D      : constant ECS.Event.Byte := 40;  -- 'D' key (evdev 32 + 8)
   LK_Q      : constant ECS.Event.Byte := 24;  -- 'Q' key (evdev 16 + 8)
   LK_SPACE  : constant ECS.Event.Byte := 65;  -- Space (evdev 57 + 8)
   LK_ESC    : constant ECS.Event.Byte := 9;   -- Escape (evdev 1 + 8)
   LK_P      : constant ECS.Event.Byte := 33;  -- 'P' key (evdev 25 + 8)
   LK_F      : constant ECS.Event.Byte := 41;  -- 'F' key (evdev 33 + 8)

   -- -----------------------------------------------------------------------
   -- Action and dynamic bindings
   -- Define logical actions (game-side) and allow dynamic key bindings
   -- so controls can be changed at runtime or loaded from a config.
   -- -----------------------------------------------------------------------

   type Action is (
      Act_Move_Left,
      Act_Move_Right,
      Act_Launch,
      Act_Reset,
      Act_Toggle_Perf,
      Act_Full_Perf,
      Act_Quit
   );

   Max_Bindings_Per_Action : constant Positive := 8;

   subtype Key_Code is ECS.Event.Byte;

   type Key_Array is array (1 .. Max_Bindings_Per_Action) of Key_Code;

   -- Holds the dynamic binding list and a count for each action
   type Binding_T is record
      Keys  : Key_Array := (others => 0);
      Count : Natural := 0;
   end record;

   -- Add/remove bindings at runtime
   procedure Bind_Key (Act : Action; Key : Key_Code);
   procedure Unbind_Key (Act : Action; Key : Key_Code);
   procedure Clear_Bindings (Act : Action);
   procedure Set_Default_Bindings;

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

   -- Return the binding list for an action (read-only)
   function Get_Bindings (Act : Action) return Binding_T;

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
