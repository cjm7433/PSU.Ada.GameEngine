-- performance.ads
--
-- Simple performance timing utilities for ECS profiling
-- Measures execution time for code blocks and provides FPS tracking

with Ada.Real_Time; use Ada.Real_Time;

package Performance is

   ------------------------------------------------------------
   -- Timer: Measures elapsed time between Start and Stop
   ------------------------------------------------------------
   type Timer is tagged private;
   
   -- Start the timer
   procedure Start (T : in out Timer);
   
   -- Stop the timer and return elapsed time in seconds
   function Stop (T : in out Timer) return Float;
   
   -- Get elapsed time without stopping (for continuous timing)
   function Elapsed (T : Timer) return Float;
   
   
   ------------------------------------------------------------
   -- FPS_Counter: Tracks frames per second
   ------------------------------------------------------------
   type FPS_Counter is tagged private;
   
   -- Initialize the counter
   procedure Initialize (F : in out FPS_Counter);
   
   -- Call once per frame - returns current FPS
   function Update (F : in out FPS_Counter) return Float;
   
   -- Get average FPS over the last N frames
   function Get_Average_FPS (F : FPS_Counter) return Float;
   
   -- Get minimum FPS (worst frame)
   function Get_Min_FPS (F : FPS_Counter) return Float;
   
   -- Get maximum FPS (best frame)
   function Get_Max_FPS (F : FPS_Counter) return Float;
   

private

   type Timer is tagged record
      Start_Time : Time;
      Stop_Time  : Time;
      Running    : Boolean := False;
   end record;
   
   
   -- Track last 60 frames for averaging
   type FPS_Array is array (1 .. 60) of Float;
   
   type FPS_Counter is tagged record
      Last_Time    : Time := Clock;
      Frame_Times  : FPS_Array := (others => 0.016);  -- Default 60 FPS
      Frame_Index  : Natural := 1;
      Frame_Count  : Natural := 0;
      Current_FPS  : Float := 60.0;
      Min_FPS      : Float := 60.0;
      Max_FPS      : Float := 60.0;
   end record;

end Performance;
