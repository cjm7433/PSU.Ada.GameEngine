-- ecs-systems_movement.ads

-- Movement System

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Movement is

   type Movement_System is new System with null record;

   -- Update will execute logic for the system

   -- Components_Needed is the list of required components 
   -- (Transform and Motion)

   overriding 
   procedure Update
     (Self : in out Movement_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   overriding 
   function Components_Needed (Self : Movement_System) 
      return ECS.Components.Component_Tag_Array;

end ECS.Systems_Movement;