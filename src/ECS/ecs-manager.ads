-- ecs-manager.ads
--
-- ECS Manager - REDESIGNED
-- ================================
-- Manages ECS systems with dynamic ordering using tagged types
-- and Ada.Containers.Vectors for flexibility.
--
-- Key improvements:
-- 1. Dynamic system list (no fixed size limit)
-- 2. Generic system handling via System'Class
-- 3. No hardcoded system types
-- 4. Proper Get_Store accessibility

with Ada.Containers.Vectors;
with ECS.Store;
with ECS.Systems; use ECS.Systems;
with Performance;

package ECS.Manager is

   ------------------------------------------------------------
   -- System Registration
   -- Systems are stored as System'Class (tagged type)
   ------------------------------------------------------------
   
   -- Equality for System_Access (needed for Vector instantiation)
   function System_Access_Equal (Left, Right : System_Access) return Boolean
      is (Left = Right);
   
   package System_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => ECS.Systems.System_Access,
      "="          => System_Access_Equal);
   
   subtype System_List is System_Vectors.Vector;


   ------------------------------------------------------------
   -- System Performance Data
   ------------------------------------------------------------
   type System_Performance_Data is record
      Total_Time    : Float := 0.0;
      Min_Time      : Float := Float'Last;
      Max_Time      : Float := 0.0;
      Call_Count    : Natural := 0;
   end record;

   package Performance_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => System_Performance_Data);

   subtype Performance_List is Performance_Vectors.Vector;


   -- Equality for Timer (needed for Vector instantiation)
   function Timer_Equal (Left, Right : Performance.Timer) return Boolean
      is (True);  -- Dummy equality, not used for performance tracking
   
   package Timer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Performance.Timer,
      "="          => Timer_Equal);

   subtype Timer_List is Timer_Vectors.Vector;


   ------------------------------------------------------------
   -- ECS Manager
   -- Now with dynamic system ordering and performance tracking
   ------------------------------------------------------------
   type ECS_Manager is tagged limited record
      
      -- Component/Entity storage (a Store)
      World : aliased ECS.Store.Store;

      -- Dynamic system list (no size limit!)
      Systems : System_List;

      -- Performance tracking
      System_Timers : Timer_List;  -- One timer per system
      Performance_Data : Performance_List;    -- Performance stats per system
      Frame_Count : Natural := 0;
   
   end record;


   ------------------------------------------------------------
   -- Initialize
   -- Clear store and system list
   ------------------------------------------------------------
   procedure Initialize (M : in out ECS_Manager);


   ------------------------------------------------------------
   -- Add_System
   -- Register a system to be executed during Update
   -- Systems execute in the order they are added
   --
   -- Example:
   --    Add_System (M, Movement_Sys'Access);
   --    Add_System (M, Collision_Sys'Access);
   ------------------------------------------------------------
   procedure Add_System (
      M   : in out ECS_Manager;
      Sys : ECS.Systems.System_Access);


   ------------------------------------------------------------
   -- Clear_Systems
   -- Remove all registered systems
   -- Useful for reconfiguring system order
   ------------------------------------------------------------
   procedure Clear_Systems (M : in out ECS_Manager);


   ------------------------------------------------------------
   -- Update
   -- Execute all registered systems in order
   --
   -- DT = Delta time in seconds since last frame
   --
   -- Implementation: Iterates through Systems vector,
   -- dispatches to each System.Update via dynamic dispatch
   ------------------------------------------------------------
   procedure Update (M : in out ECS_Manager; DT : Float);


   ------------------------------------------------------------
   -- Get_Store
   -- Access the store for direct entity/component manipulation
   --
   -- FIXED: Proper accessibility via anonymous access type
   -- M must be declared 'aliased' at call site
   ------------------------------------------------------------
   function Get_Store (M : aliased in out ECS_Manager)
      return access ECS.Store.Store;


   ------------------------------------------------------------
   -- Validate_Systems
   -- Check that all registered systems have their required
   -- components available in the Store
   --
   -- Useful for debugging - call after setting up entities
   -- to verify systems will find what they need
   --
   -- Returns: True if all systems can find entities to process
   ------------------------------------------------------------
   function Validate_Systems (M : ECS_Manager) return Boolean;


   ------------------------------------------------------------
   -- Get_System_Count
   -- Return number of registered systems
   ------------------------------------------------------------
   function Get_System_Count (M : ECS_Manager) return Natural;


   ------------------------------------------------------------
   -- Performance Tracking Procedures
   ------------------------------------------------------------

   -- Get performance summary for all systems
   procedure Get_Performance_Summary (M : in ECS_Manager; FPS_Tracker : in Performance.FPS_Counter);

   -- Get detailed performance information
   procedure Get_Performance_Details (M : in ECS_Manager);

   -- Reset performance counters
   procedure Reset_Performance_Data (M : in out ECS_Manager);

end ECS.Manager;
