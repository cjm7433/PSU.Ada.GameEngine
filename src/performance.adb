-- performance.adb
--
-- Implementation of performance timing utilities

package body Performance is

   ------------------------------------------------------------
   -- Timer Implementation
   ------------------------------------------------------------
   
   procedure Start (T : in out Timer) is
   begin
      T.Start_Time := Clock;
      T.Running := True;
   end Start;
   
   
   function Stop (T : in out Timer) return Float is
   begin
      T.Stop_Time := Clock;
      T.Running := False;
      
      declare
         Elapsed_Time : constant Time_Span := T.Stop_Time - T.Start_Time;
      begin
         return Float (To_Duration (Elapsed_Time));
      end;
   end Stop;
   
   
   function Elapsed (T : Timer) return Float is
      Current : constant Time := Clock;
      Elapsed_Time : constant Time_Span := Current - T.Start_Time;
   begin
      return Float (To_Duration (Elapsed_Time));
   end Elapsed;
   
   
   ------------------------------------------------------------
   -- FPS_Counter Implementation
   ------------------------------------------------------------
   
   procedure Initialize (F : in out FPS_Counter) is
   begin
      F.Last_Time := Clock;
      F.Frame_Times := (others => 0.016);
      F.Frame_Index := 1;
      F.Frame_Count := 0;
      F.Current_FPS := 60.0;
      F.Min_FPS := 60.0;
      F.Max_FPS := 60.0;
   end Initialize;
   
   
   function Update (F : in out FPS_Counter) return Float is
      Current_Time : constant Time := Clock;
      Delta_Time   : constant Time_Span := Current_Time - F.Last_Time;
      DT           : constant Float := Float (To_Duration (Delta_Time));
   begin
      -- Store frame time
      F.Frame_Times (F.Frame_Index) := DT;
      
      -- Calculate current FPS
      if DT > 0.0 then
         F.Current_FPS := 1.0 / DT;
      else
         F.Current_FPS := 1000.0;  -- Cap at 1000 FPS for very fast frames
      end if;
      
      -- Update min/max
      if F.Frame_Count > 0 then
         if F.Current_FPS < F.Min_FPS then
            F.Min_FPS := F.Current_FPS;
         end if;
         
         if F.Current_FPS > F.Max_FPS then
            F.Max_FPS := F.Current_FPS;
         end if;
      else
         -- First frame
         F.Min_FPS := F.Current_FPS;
         F.Max_FPS := F.Current_FPS;
      end if;
      
      -- Advance to next frame
      F.Frame_Index := (F.Frame_Index mod 60) + 1;
      F.Frame_Count := F.Frame_Count + 1;
      
      -- Update last time
      F.Last_Time := Current_Time;
      
      return F.Current_FPS;
   end Update;
   
   
   function Get_Average_FPS (F : FPS_Counter) return Float is
      Total : Float := 0.0;
      Count : constant Natural := 
         (if F.Frame_Count < 60 then F.Frame_Count else 60);
   begin
      if Count = 0 then
         return 60.0;
      end if;
      
      -- Sum all frame times
      for I in 1 .. Count loop
         Total := Total + F.Frame_Times (I);
      end loop;
      
      -- Average FPS = 1 / Average Frame Time
      declare
         Avg_Frame_Time : constant Float := Total / Float (Count);
      begin
         if Avg_Frame_Time > 0.0 then
            return 1.0 / Avg_Frame_Time;
         else
            return 1000.0;
         end if;
      end;
   end Get_Average_FPS;
   
   
   function Get_Min_FPS (F : FPS_Counter) return Float is
   begin
      return F.Min_FPS;
   end Get_Min_FPS;
   
   
   function Get_Max_FPS (F : FPS_Counter) return Float is
   begin
      return F.Max_FPS;
   end Get_Max_FPS;

end Performance;
