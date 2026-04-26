-- input.adb
--
-- Implementation of the Arkanoid input module.
-- See input.ads for design notes.

package body Input is

   -----------------------------------------------------
   --  Procedure: Handle_Event
   --  Inputs: Event - a single event from the engine queue
   --  Output: None (updates Input.State in place)
   -----------------------------------------------------
   procedure Handle_Event (Event : ECS.Event.Event_T) is
      Code : constant ECS.Event.Byte := Event.Data.KeyCode;

      -- Linux keycodes emitted by the Wayland wrapper
      VK_Left   : constant ECS.Event.Byte := 38;   -- A (endev 30 + 8)
      VK_Right  : constant ECS.Event.Byte := 40;   -- D (endev 32 + 8)
      VK_Q      : constant ECS.Event.Byte := 24;   -- Q (endev 16 + 8)
      VK_Space  : constant ECS.Event.Byte := 65;   -- Spacebar (endev 57 + 8)
      VK_Escape : constant ECS.Event.Byte := 9;   -- Escape (endev 1 + 8)
      VK_P      : constant ECS.Event.Byte := 33;   -- P key (endev 25 + 8)
      VK_F      : constant ECS.Event.Byte := 41;   -- F key (endev 33 + 8)
   begin
      case Event.EventType is

         when ECS.Event.KeyDown =>
            -- Held keys: set flag on; cleared by KeyUp
            if Code = VK_Left then
               State.Left := True;
            elsif Code = VK_Right then
               State.Right := True;
            -- One-shot keys: set flag on; cleared by Clear_Frame_Flags
            elsif Code = VK_Space then
               State.Space := True;
            elsif Code = VK_Escape then
               State.Escape := True;
            elsif Code = VK_P then
               State.P := True;
            elsif Code = VK_F then
               State.F := True;
            elsif Code = VK_Q then
               State.Q := True;
            end if;

         when ECS.Event.KeyUp =>
            -- Only held keys need clearing on release
            if Code = VK_Left then
               State.Left := False;
            elsif Code = VK_Right then
               State.Right := False;
            end if;

         when others =>
            null;   -- Mouse events and NoEvent are ignored here

      end case;
   end Handle_Event;


   -----------------------------------------------------
   --  Procedure: Clear_Frame_Flags
   --  Resets one-shot flags after the game loop has consumed them.
   -----------------------------------------------------
   procedure Clear_Frame_Flags is
   begin
      State.Space  := False;
      State.Escape := False;
      State.P      := False;
   end Clear_Frame_Flags;


   -----------------------------------------------------
   --  Procedure: Reset
   --  Clears all input flags to unpressed.
   -----------------------------------------------------
   procedure Reset is
   begin
      State := (others => False);
   end Reset;

end Input;
