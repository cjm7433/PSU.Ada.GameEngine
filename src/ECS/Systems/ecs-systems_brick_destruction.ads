-- ecs-systems_brick_destruction.ads
--
-- Brick Destruction System
-- Handles brick death animations and removal.
--
-- Required Components: Brick
--
-- Responsibilities:
--   - Update death timers for dying bricks
--   - Remove bricks when death animation completes
--   - TODO: Could trigger particle effects, scoring, power-up spawning (future)

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Brick_Destruction is

   type Brick_Destruction_System is new System with null record;

   --------------------------------------------------------------
   -- Update
   -- Update executes brick destruction logic
   --------------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Brick_Destruction_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   --------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Brick
   --------------------------------------------------------------
   overriding 
   function Components_Needed (Self : Brick_Destruction_System) 
      return ECS.Components.Component_Tag_Array;

end ECS.Systems_Brick_Destruction;
