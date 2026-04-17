-- ecs-systems_ball_physics.ads
--
-- Ball Physics System
-- Handles ball-specific physics: speed constraints, attachment to paddle, etc.
--
-- Required Components: Ball, Transform, Motion
--
-- Responsibilities:
--   - Clamp ball speed to Min/Max limits
--   - Handle ball attachment to paddle (before launch)
--   - Process ball launch input
--   - Keep ball velocity normalized

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Ball_Physics is

   type Ball_Physics_System is new System with null record;

   ---------------------------------------------------------------
   -- Update
   -- Update executes ball physics logic
   ---------------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Ball_Physics_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   ---------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Ball, Transform, Motion
   ---------------------------------------------------------------
   overriding 
   function Components_Needed (Self : Ball_Physics_System) 
      return ECS.Components.Component_Tag_Array;

   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Ball_Physics_System) return String;

end ECS.Systems_Ball_Physics;
