--  ecs-systems.ads

--  Abstract base type and behavior for all ECS systems.

with ECS.Store;
with ECS.Components;

package ECS.Systems is

   type System is abstract tagged null record;

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

   -- Olivier's suggested function (changed to above ^)
   -- TODO: Remove once filters are working
   -- function Components_Needed (Self : System) return Component_Tag is abstract;

end ECS.Systems;