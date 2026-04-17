-- ecs-systems.ads
--
-- Abstract base type for all ECS systems.
--    - Defines the System interface that all concrete systems implement.
--    - Every system must provide:
--          1. Update procedure - processes entities each frame
--          2. Components_Needed function - declares required component types
--
-- System lifecycle:
--   Systems are stateless (null record), stored in the ECS Manager,
--   and called in a fixed order each frame. The Manager passes the
--   Store and delta time to Update, which queries for matching entities
--   and performs logic.


with ECS.Store;
with ECS.Components;

package ECS.Systems is

   type System is abstract tagged null record;

   -- Access type for storing systems polymorphically
   type System_Access is access all System'Class;

   -- Every system must implement Update
   procedure Update
     (Self : in out System;
      S    : in out ECS.Store.Store;
      DT   : Float) is abstract;

      -- Every individual System must implement Update!
      -- DT (Delta Time) measures the time between frames in seconds.


   -- Systems declare required components
   -- Now every system must declare its requirements.
   function Components_Needed (Self : System) 
      return ECS.Components.Component_Tag_Array is abstract;

   -- Get system name for performance tracking
   function Name (Self : System) return String is abstract;

   -- Olivier's suggested function (changed to above ^)
   -- TODO: Remove once filters are working
   -- function Components_Needed (Self : System) return Component_Tag is abstract;

end ECS.Systems;