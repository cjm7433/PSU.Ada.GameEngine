--  ecs-systems.ads

--  Abstract base type and behavior for all ECS systems.

with ECS.Store;

package ECS.Systems is

   type System is abstract tagged null record;

   procedure Update
     (Self : in out System;
      S    : in out ECS.Store.Store;
      DT   : Float) is abstract;

      -- Every individual System must implement Update!
      -- DT (Delta Time) measures the time between frames in seconds.

end ECS.Systems;
