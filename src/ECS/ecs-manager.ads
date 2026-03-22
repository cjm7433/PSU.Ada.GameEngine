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
   -- ECS Manager
   -- Now with dynamic system ordering
   ------------------------------------------------------------
   type ECS_Manager is tagged limited record
      
      -- Component/Entity storage (a Store)
      World : aliased ECS.Store.Store;

      -- Dynamic system list (no size limit!)
      Systems : System_List;
   
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

end ECS.Manager;
