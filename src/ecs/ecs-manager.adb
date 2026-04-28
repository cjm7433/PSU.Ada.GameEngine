-- ecs-manager.adb
--
-- ECS Manager Implementation

with Ada.Text_IO;
with ECS.Store; use ECS.Store;
with ECS.Systems;
with ECS.Components;

package body ECS.Manager is

   ------------------------------------------------------------
   -- Initialize
   -- Reset store and clear all systems and performance data
   ------------------------------------------------------------
   procedure Initialize (M : in out ECS_Manager) is
   begin
      -- Clear all entities and components
      ECS.Store.Initialize (M.World);

      -- Clear system list (no default systems)
      M.Systems.Clear;

      -- Clear performance data
      M.System_Timers.Clear;
      M.Performance_Data.Clear;
      M.Frame_Count := 0;
   end Initialize;


   ------------------------------------------------------------
   -- Add_System
   -- Register a system for execution and create performance timer
   ------------------------------------------------------------
   procedure Add_System (
      M   : in out ECS_Manager;
      Sys : ECS.Systems.System_Access
   ) is
      New_Timer : Performance.Timer;
      New_Perf_Data : System_Performance_Data := (Total_Time => 0.0, Min_Time => Float'Last, Max_Time => 0.0, Call_Count => 0);
   begin
      M.Systems.Append (Sys);
      M.System_Timers.Append (New_Timer);
      M.Performance_Data.Append (New_Perf_Data);
   end Add_System;


   ------------------------------------------------------------
   -- Clear_Systems
   -- Remove all registered systems
   ------------------------------------------------------------
   procedure Clear_Systems (M : in out ECS_Manager) is
   begin
      M.Systems.Clear;
   end Clear_Systems;


   ------------------------------------------------------------
   -- Update
   -- Execute all systems using dynamic dispatch with performance tracking
   ------------------------------------------------------------
   procedure Update (M : in out ECS_Manager; DT : Float) is
      System_Index : Positive := 1;
   begin
      M.Frame_Count := M.Frame_Count + 1;

      -- Iterate through all registered systems
      for Sys of M.Systems loop
         -- Start timing this system
         M.System_Timers(System_Index).Start;

         -- Dynamic dispatch to correct System.Update
         Sys.Update (M.World, DT);

         -- Stop timing and collect performance data
         declare
            Time_Taken : constant Float := M.System_Timers(System_Index).Stop;
            Perf_Data : System_Performance_Data renames M.Performance_Data(System_Index);
         begin
            -- Update performance statistics
            Perf_Data.Total_Time := Perf_Data.Total_Time + Time_Taken;
            Perf_Data.Call_Count := Perf_Data.Call_Count + 1;

            if Time_Taken < Perf_Data.Min_Time then
               Perf_Data.Min_Time := Time_Taken;
            end if;

            if Time_Taken > Perf_Data.Max_Time then
               Perf_Data.Max_Time := Time_Taken;
            end if;

            -- Log if system is slow (> 5ms)
            if Time_Taken > 0.005 then
               Ada.Text_IO.Put_Line ("WARNING: Slow system (> 5ms) '" & Sys.Name &
                                   "' took" & Time_Taken'Image & " seconds");
            end if;
         end;

         System_Index := System_Index + 1;
      end loop;
   end Update;


   ------------------------------------------------------------
   -- Get_Store
   -- Provide access to the store
   --
   -- Accessibility Fixes (2/26):
   -- --------------------
   -- - M is 'aliased in out' (parameter must be aliased)
   -- - M.World is 'aliased' (record component)
   -- - Return type is anonymous 'access Store'
   -- - Compiler ensures access can't outlive M
   --
   -- At call site, declare Manager as:
   --    Manager : aliased ECS_Manager;
   --    Store_Ptr : access ECS.Store.Store := Manager.Get_Store;
   ------------------------------------------------------------
   function Get_Store (M : aliased in out ECS_Manager)
      return access ECS.Store.Store
   is
   begin
      return M.World'Access;
   end Get_Store;


   ------------------------------------------------------------
   -- Validate_Systems
   -- Check if systems can find entities to process
   --
   -- This is useful for debugging - it queries the store
   -- for each system's required components and reports if
   -- any system won't find matching entities.
   ------------------------------------------------------------
   function Validate_Systems (M : ECS_Manager) return Boolean is
      use ECS.Components;
      Entities : ECS.Store.Entity_ID_Array_Access;
   begin
      -- Check each registered system
      for Sys of M.Systems loop
         -- Get the components this system needs
         declare
            Needed : constant ECS.Components.Component_Tag_Array := Sys.Components_Needed;
         begin
            -- Query store for entities with these components
            Entities := M.World.Get_Entities_With (Needed);
            
            -- If no entities found, this system won't do anything
            -- (This isn't necessarily an error - systems can have
            --  zero work to do, but it's worth knowing)
            if Entities = null or else Entities'Length = 0 then
               -- System has no work (might be intentional)
               null;
            end if;
         end;
      end loop;
      
      return True;
   end Validate_Systems;


   ------------------------------------------------------------
   -- Get_System_Count
   -- Return number of registered systems
   ------------------------------------------------------------
   function Get_System_Count (M : ECS_Manager) return Natural is
   begin
      return Natural (M.Systems.Length);
   end Get_System_Count;


   ------------------------------------------------------------
   -- Get_Performance_Summary
   -- Display a summary of System performance (in a table)
   ------------------------------------------------------------
   procedure Get_Performance_Summary (M : in ECS_Manager; FPS_Tracker : in Performance.FPS_Counter) is
   System_Index : Positive := 1;

   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("=== ECS System Performance Summary ===");
      Ada.Text_IO.Put_Line ("Frames processed:" & M.Frame_Count'Image);
      
      -- FPS Information
      Ada.Text_IO.Put_Line ("Average FPS: " & Integer (Performance.Get_Average_FPS (FPS_Tracker))'Image);
      Ada.Text_IO.Put_Line ("Min FPS:     " & Integer (Performance.Get_Min_FPS (FPS_Tracker))'Image);
      Ada.Text_IO.Put_Line ("Max FPS:     " & Integer (Performance.Get_Max_FPS (FPS_Tracker))'Image);
      
      Ada.Text_IO.New_Line;

      -- Header
      Ada.Text_IO.Set_Col (3);
      Ada.Text_IO.Put ("System");

      Ada.Text_IO.Set_Col (28);
      Ada.Text_IO.Put ("Avg (ms)");

      Ada.Text_IO.Set_Col (42);
      Ada.Text_IO.Put ("Min (ms)");

      Ada.Text_IO.Set_Col (56);
      Ada.Text_IO.Put ("Max (ms)");

      Ada.Text_IO.New_Line;

      -- Separator
      Ada.Text_IO.Set_Col (3);
      Ada.Text_IO.Put ("------------------------");

      Ada.Text_IO.Set_Col (28);
      Ada.Text_IO.Put ("----------");

      Ada.Text_IO.Set_Col (42);
      Ada.Text_IO.Put ("----------");

      Ada.Text_IO.Set_Col (56);
      Ada.Text_IO.Put ("----------");

      Ada.Text_IO.New_Line;

      -- Data rows
      for Sys of M.Systems loop
         declare
            Perf_Data : System_Performance_Data renames
            M.Performance_Data (System_Index);

            Avg_Time : Float := 0.0;
         begin
            if Perf_Data.Call_Count > 0 then
               Avg_Time := Perf_Data.Total_Time / Float (Perf_Data.Call_Count);
            end if;

            -- Name
            Ada.Text_IO.Set_Col (3);
            Ada.Text_IO.Put (Sys.Name);

            -- Avg
            Ada.Text_IO.Set_Col (28);
            Ada.Text_IO.Put (Integer (Avg_Time * 1000.0)'Image);

            -- Min
            Ada.Text_IO.Set_Col (42);
            Ada.Text_IO.Put (Integer (Perf_Data.Min_Time * 1000.0)'Image);

            -- Max
            Ada.Text_IO.Set_Col (56);
            Ada.Text_IO.Put (Integer (Perf_Data.Max_Time * 1000.0)'Image);

            Ada.Text_IO.New_Line;
         end;

         System_Index := System_Index + 1;
      end loop;
   end Get_Performance_Summary;

   ------------------------------------------------------------
   -- Get_Performance_Details
   -- Display detailed performance information
   ------------------------------------------------------------
   procedure Get_Performance_Details (M : in ECS_Manager) is
      System_Index : Positive := 1;
   begin
      Ada.Text_IO.Put_Line ("=== ECS System Performance Details ===");
      Ada.Text_IO.Put_Line ("Frames processed:" & M.Frame_Count'Image);
      Ada.Text_IO.New_Line;

      for Sys of M.Systems loop
         declare
            Perf_Data : System_Performance_Data renames M.Performance_Data(System_Index);
            Avg_Time : Float := 0.0;
         begin
            if Perf_Data.Call_Count > 0 then
               Avg_Time := Perf_Data.Total_Time / Float(Perf_Data.Call_Count);
            end if;

            Ada.Text_IO.Put_Line ("System: " & Sys.Name);
            Ada.Text_IO.Put_Line ("  Calls:" & Perf_Data.Call_Count'Image);
            Ada.Text_IO.Put_Line ("  Total Time:" & Perf_Data.Total_Time'Image & " seconds");
            Ada.Text_IO.Put_Line ("  Average Time:" & Avg_Time'Image & " seconds (" &
                                Integer(Avg_Time * 1000.0)'Image & "ms)");
            Ada.Text_IO.Put_Line ("  Min Time:" & Perf_Data.Min_Time'Image & " seconds (" &
                                Integer(Perf_Data.Min_Time * 1000.0)'Image & "ms)");
            Ada.Text_IO.Put_Line ("  Max Time:" & Perf_Data.Max_Time'Image & " seconds (" &
                                Integer(Perf_Data.Max_Time * 1000.0)'Image & "ms)");
            Ada.Text_IO.New_Line;
         end;

         System_Index := System_Index + 1;
      end loop;
   end Get_Performance_Details;


   ------------------------------------------------------------
   -- Reset_Performance_Data
   -- Reset all performance counters
   ------------------------------------------------------------
   procedure Reset_Performance_Data (M : in out ECS_Manager) is
   begin
      for Data of M.Performance_Data loop
         Data := (others => <>);
      end loop;
      M.Frame_Count := 0;
   end Reset_Performance_Data;

end ECS.Manager;
