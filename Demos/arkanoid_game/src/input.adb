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
