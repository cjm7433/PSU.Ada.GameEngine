-- ecs-systems_collision.ads
--
-- Collision System
-- Detects and resolves collisions between entities using AABB (Axis-Aligned Bounding Box).
--
-- Required Components: Collision, Transform
--
-- Responsibilities:
--   - Detect overlaps between collision boxes
--   - Resolve collisions by separating entities
--   - Apply bounce physics (reverse velocity)
--   - Trigger collision events (for brick destruction, etc.)
--
-- Algorithm:
-- For each entity with Collision + Transform:
--    - Check against all other entities with Collision + Transform
--    - If boxes overlap and layers/masks match:
--         - Calculate penetration depth
--         - Separate entities
--         - Reflect velocity for bouncing

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Collision is

   type Collision_System is new System with null record;

   ---------------------------------------------------------
   -- Update
   -- Update executes collision detection and resolution
   ---------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Collision_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   ---------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Collision, Transform
   ---------------------------------------------------------
   overriding 
   function Components_Needed (Self : Collision_System) 
      return ECS.Components.Component_Tag_Array;


   -- TODO: Do all the helper functions need to be in this .ads?
   -- Quick google:
   -- No, Ada helper functions or procedures that are only used internally 
   --    within a package should be placed in the .ads (package specification) 
   --    file. They should be placed exclusively in the .adb (package body) 
   --    to maintain proper encapsulation. 

end ECS.Systems_Collision;
