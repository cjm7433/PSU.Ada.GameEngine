-- ecs-systems_movement.ads

-- Movement System

with ECS.Systems;       use ECS.Systems;
with ECS.Store;

package ECS.Systems_Movement is

   type Movement_System is new System with null record;

   overriding 
   procedure Update
     (Self : in out Movement_System;
      S    : in out ECS.Store.Store;
      DT   : Float);

end ECS.Systems_Movement;