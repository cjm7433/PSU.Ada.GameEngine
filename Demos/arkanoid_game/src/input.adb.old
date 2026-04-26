
-- input.adb
--
-- Clean, self-contained implementation of the Arkanoid input module.
-- Matches the spec in input.ads and provides dynamic bindings.

with ECS.Event; use ECS.Event;

package body Input is

   -- Storage for dynamic bindings
   Bindings : array (Action) of Binding_T := (others => (Keys => (others => 0), Count => 0));

   -----------------------------------------------------
   -- Bind_Key
   -----------------------------------------------------
   procedure Bind_Key (Act : Action; Key : Key_Code) is
         B : Binding_T renames Bindings (Act);
   begin
      -- avoid duplicates
      for I in 1 .. B.Count loop
         if B.Keys (I) = Key then
            return;
         end if;
      end loop;
      if B.Count < Max_Bindings_Per_Action then
         B.Count := B.Count + 1;
         B.Keys (B.Count) := Key;
      end if;
   end Bind_Key;

   -----------------------------------------------------
   -- Unbind_Key
   -----------------------------------------------------
   procedure Unbind_Key (Act : Action; Key : Key_Code) is
         B : Binding_T renames Bindings (Act);
      Found : Natural := 0;
   begin
      for I in 1 .. B.Count loop
         if B.Keys (I) = Key then
            Found := I;
            exit;
         end if;
      end loop;
      if Found /= 0 then
         for J in Found .. B.Count - 1 loop
            B.Keys (J) := B.Keys (J+1);
         end loop;
         B.Keys (B.Count) := 0;
         B.Count := B.Count - 1;
      end if;
   end Unbind_Key;

   -----------------------------------------------------
   -- Clear_Bindings
   -----------------------------------------------------
   procedure Clear_Bindings (Act : Action) is
   begin
      Bindings (Act).Keys  := (others => 0);
      Bindings (Act).Count := 0;
   end Clear_Bindings;

   -----------------------------------------------------
   -- Get_Bindings
   -----------------------------------------------------
   function Get_Bindings (Act : Action) return Binding_T is
   begin
      return Bindings (Act);
   end Get_Bindings;

   -----------------------------------------------------
   -- Set_Default_Bindings
   -----------------------------------------------------
   procedure Set_Default_Bindings is
   begin
      for A in Action loop
         Clear_Bindings (A);
      end loop;

      -- Move left
      Bind_Key (Act_Move_Left, VK_Left);
      Bind_Key (Act_Move_Left, LK_A);

      -- Move right
      Bind_Key (Act_Move_Right, VK_Right);
      Bind_Key (Act_Move_Right, LK_D);

      -- Launch
      Bind_Key (Act_Launch, VK_Space);
      Bind_Key (Act_Launch, LK_SPACE);

      -- Reset
      Bind_Key (Act_Reset, VK_Escape);
      Bind_Key (Act_Reset, LK_ESC);

      -- Toggle perf
      Bind_Key (Act_Toggle_Perf, VK_P);
      Bind_Key (Act_Toggle_Perf, LK_P);

      -- Full perf
      Bind_Key (Act_Full_Perf, VK_F);
      Bind_Key (Act_Full_Perf, LK_F);

      -- Quit
      Bind_Key (Act_Quit, VK_Q);
      Bind_Key (Act_Quit, LK_Q);
   end Set_Default_Bindings;

   -----------------------------------------------------
   -- Handle_Event
   -----------------------------------------------------
   procedure Handle_Event (Event : ECS.Event.Event_T) is
      Code : constant ECS.Event.Byte := Event.Data.KeyCode;
   begin
      case Event.EventType is
         when ECS.Event.KeyDown =>
            -- Move Left
            for I in 1 .. Bindings (Act_Move_Left).Count loop
               if Bindings (Act_Move_Left).Keys (I) = Code then
                  State.Left := True;
                  exit;
               end if;
            end loop;

            -- Move Right
            for I in 1 .. Bindings (Act_Move_Right).Count loop
               if Bindings (Act_Move_Right).Keys (I) = Code then
                  State.Right := True;
                  exit;
               end if;
            end loop;

            -- Launch
            for I in 1 .. Bindings (Act_Launch).Count loop
               if Bindings (Act_Launch).Keys (I) = Code then
                  State.Space := True;
                  exit;
               end if;
            end loop;

            -- Reset
            for I in 1 .. Bindings (Act_Reset).Count loop
               if Bindings (Act_Reset).Keys (I) = Code then
                  State.Escape := True;
                  exit;
               end if;
            end loop;

            -- Toggle Perf
            for I in 1 .. Bindings (Act_Toggle_Perf).Count loop
               if Bindings (Act_Toggle_Perf).Keys (I) = Code then
                  State.P := True;
                  exit;
               end if;
            end loop;

            -- Full Perf
            for I in 1 .. Bindings (Act_Full_Perf).Count loop
               if Bindings (Act_Full_Perf).Keys (I) = Code then
                  State.F := True;
                  exit;
               end if;
            end loop;

            -- Quit
            for I in 1 .. Bindings (Act_Quit).Count loop
               if Bindings (Act_Quit).Keys (I) = Code then
                  State.Q := True;
                  exit;
               end if;
            end loop;

         when ECS.Event.KeyUp =>
            for I in 1 .. Bindings (Act_Move_Left).Count loop
               if Bindings (Act_Move_Left).Keys (I) = Code then
                  State.Left := False;
                  exit;
               end if;
            end loop;
            for I in 1 .. Bindings (Act_Move_Right).Count loop
               if Bindings (Act_Move_Right).Keys (I) = Code then
                  State.Right := False;
                  exit;
               end if;
            end loop;

         when others =>
            null;
      end case;
   end Handle_Event;

   -----------------------------------------------------
   -- Clear_Frame_Flags
   -----------------------------------------------------
   procedure Clear_Frame_Flags is
   begin
      State.Space  := False;
      State.Escape := False;
      State.P      := False;
   end Clear_Frame_Flags;

   -----------------------------------------------------
   -- Reset
   -----------------------------------------------------
   procedure Reset is
   begin
      State := (others => False);
   end Reset;

begin
   -- Initialize default bindings on package elaboration
   Set_Default_Bindings;

   end Input;

