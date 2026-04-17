-- ecs_performance_test.adb
--
-- Performance stress test for ECS
-- Creates many entities and measures system execution times

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Performance;
with ECS.Manager;
with ECS.Store; use ECS.Store;
with ECS.Entities; use ECS.Entities;
with ECS.Components.Transform; use ECS.Components.Transform;
with ECS.Components.Motion; use ECS.Components.Motion;
with ECS.Components.Collider; use ECS.Components.Collider;

-- Import your systems
with ECS.Systems_Movement;
with ECS.Systems_Collision;

procedure ECS_Performance_Test is

   -- System instances
   Movement_Sys  : aliased ECS.Systems_Movement.Movement_System;
   Collision_Sys : aliased ECS.Systems_Collision.Collision_System;
   
   -- ECS Manager
   Manager : ECS.Manager.ECS_Manager;
   
   -- Performance tracking
   Frame_Timer   : Performance.Timer;
   FPS_Tracker   : Performance.FPS_Counter;
   
   -- Test parameters
   Entity_Counts : constant array (1 .. 5) of Natural := 
      (10, 100, 500, 1000, 5000);
   
   Test_Frames : constant := 300;  -- Run 300 frames per test (5 seconds at 60 FPS)
   
   
   ------------------------------------------------------------
   -- Create test entities with Transform, Motion, Collision
   ------------------------------------------------------------
   procedure Create_Test_Entities (Count : Natural) is
      --S : access Store.Store := Manager.Get_Store;

      S : ECS.Store.Store renames Manager.World;

   begin
      Put_Line ("Creating" & Count'Image & " entities...");
      
      for I in 1 .. Count loop
         declare
            E : constant Entity_ID := S.Create_Entity;
         begin
            --  S.Add_Component (E, Transform'Tag);
            --  S.Add_Component (E, Motion'Tag);
            --  S.Add_Component (E, Collision'Tag);

            Add_Component (S, E, Transform_Component'Tag);
            Add_Component (S, E, Motion_Component'Tag);
            Add_Component (S, E, Collider_Component'Tag);
            
            -- Set some initial values
            declare
               T_Idx : constant Natural := S.Transform.Lookup (E);
               M_Idx : constant Natural := S.Motion.Lookup (E);
            begin
               S.Transform.Data (T_Idx).Position := 
                  (Float (I mod 800), Float (I / 800));
               S.Transform.Data (T_Idx).Scale := (10.0, 10.0);
               
               S.Motion.Data (M_Idx).Linear_Velocity := 
                  (Float (I mod 10), Float (I mod 5));
            end;
         end;
      end loop;
      
      Put_Line ("Done! Created" & Count'Image & " entities.");
   end Create_Test_Entities;
   
   
   ------------------------------------------------------------
   -- Run performance test
   ------------------------------------------------------------
   procedure Run_Test (Entity_Count : Natural) is
      Total_Update_Time : Float := 0.0;
      Min_Frame_Time    : Float := 1000.0;
      Max_Frame_Time    : Float := 0.0;
   begin
      Put_Line ("");
      Put_Line ("========================================");
      Put_Line ("PERFORMANCE TEST:" & Entity_Count'Image & " ENTITIES");
      Put_Line ("========================================");
      
      -- Initialize Manager and create entities
      Manager.Initialize;
      --  Manager.Add_System (Movement_Sys'Access);
      --  Manager.Add_System (Collision_Sys'Access);

      ECS.Manager.Add_System (Manager, new ECS.Systems_Movement.Movement_System);
      ECS.Manager.Add_System (Manager, new ECS.Systems_Collision.Collision_System);

      Create_Test_Entities (Entity_Count);
      
      -- Initialize FPS tracker
      Performance.Initialize (FPS_Tracker);
      
      Put_Line ("Running" & Test_Frames'Image & " frames...");
      
      -- Run test frames
      for Frame in 1 .. Test_Frames loop
         
         -- Time this frame
         Performance.Start (Frame_Timer);
         
         -- Update all systems
         Manager.Update (0.016);  -- Simulate 60 FPS (16.67ms)
         
         -- Measure frame time
         declare
            Frame_Time : constant Float := Performance.Stop (Frame_Timer);
         begin
            Total_Update_Time := Total_Update_Time + Frame_Time;
            
            if Frame_Time < Min_Frame_Time then
               Min_Frame_Time := Frame_Time;
            end if;
            
            if Frame_Time > Max_Frame_Time then
               Max_Frame_Time := Frame_Time;
            end if;
         end;
         
         -- Update FPS counter
         declare
            Current_FPS : constant Float := Performance.Update (FPS_Tracker);
         begin
            -- Print progress every 60 frames
            if Frame mod 60 = 0 then
               Put ("Frame ");
               Put (Frame, 4);
               Put (" | FPS: ");
               Put (Current_FPS, 3, 1, 0);
               Put_Line ("");
            end if;
         end;
      end loop;
      
      -- Calculate and display results
      declare
         Avg_Frame_Time : constant Float := Total_Update_Time / Float (Test_Frames);
         Avg_FPS        : constant Float := Performance.Get_Average_FPS (FPS_Tracker);
         Min_FPS        : constant Float := Performance.Get_Min_FPS (FPS_Tracker);
         Max_FPS        : constant Float := Performance.Get_Max_FPS (FPS_Tracker);
      begin
         Put_Line ("");
         Put_Line ("RESULTS:");
         Put_Line ("--------");
         
         Put ("Average Frame Time: ");
         Put (Avg_Frame_Time * 1000.0, 3, 3, 0);
         Put_Line (" ms");
         
         Put ("Average FPS:        ");
         Put (Avg_FPS, 3, 1, 0);
         Put_Line ("");
         
         Put ("Min FPS (worst):    ");
         Put (Min_FPS, 3, 1, 0);
         Put_Line ("");
         
         Put ("Max FPS (best):     ");
         Put (Max_FPS, 3, 1, 0);
         Put_Line ("");
         
         Put ("Min Frame Time:     ");
         Put (Min_Frame_Time * 1000.0, 3, 3, 0);
         Put_Line (" ms");
         
         Put ("Max Frame Time:     ");
         Put (Max_Frame_Time * 1000.0, 3, 3, 0);
         Put_Line (" ms");
         
         -- Performance verdict
         Put_Line ("");
         if Avg_Frame_Time < 0.016 then
            Put_Line ("✓ EXCELLENT - Well under 16.67ms target (60 FPS)");
         elsif Avg_Frame_Time < 0.020 then
            Put_Line ("✓ GOOD - Acceptable performance");
         elsif Avg_Frame_Time < 0.033 then
            Put_Line ("⚠ WARNING - Below 60 FPS, above 30 FPS");
         else
            Put_Line ("✗ POOR - Below 30 FPS, optimization needed");
         end if;
      end;
   end Run_Test;

begin
   Put_Line ("ECS PERFORMANCE STRESS TEST");
   Put_Line ("===========================");
   Put_Line ("");
   Put_Line ("This test measures ECS performance with varying entity counts.");
   Put_Line ("Each test runs" & Test_Frames'Image & " frames.");
   Put_Line ("");
   
   -- Run tests with increasing entity counts
   for I in Entity_Counts'Range loop
      Run_Test (Entity_Counts (I));
   end loop;
   
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("ALL TESTS COMPLETE");
   Put_Line ("========================================");

end ECS_Performance_Test;
