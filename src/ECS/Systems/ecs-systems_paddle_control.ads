-- ecs-systems_paddle_control.ads
--
-- Paddle Control System
-- Handles player input for the paddle and updates its motion.
--
-- Required Components: Paddle, Transform, Motion
--
-- Responsibilities:
--   - Read paddle input state (Move_Left, Move_Right)
--   - Update Motion.Linear_Velocity based on input
--   - Enforce paddle boundaries (keep on screen)

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Paddle_Control is

   type Paddle_Control_System is new System with null record;

   --------------------------------------------------------
   -- Update
   -- Update executes paddle control logic
   --------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Paddle_Control_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   --------------------------------------------------------
   -- Components_Required
   -- Components_Needed is the list of required components 
   -- Components required: Paddle, Transform, Motion
   --------------------------------------------------------
   overriding 
   function Components_Needed (Self : Paddle_Control_System) 
      return ECS.Components.Component_Tag_Array;

end ECS.Systems_Paddle_Control;
