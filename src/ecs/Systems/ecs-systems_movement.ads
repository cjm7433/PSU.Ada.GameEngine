-- ecs-systems_movement.ads
--
-- Movement System
-- Integrates velocity and acceleration into entity positions and rotations.
--
-- Required Components: Transform, Motion
--
-- Responsibilities:
--   - Integrate linear acceleration into linear velocity
--   - Integrate angular acceleration into angular velocity
--   - Integrate linear velocity into position (Transform.Position)
--   - Integrate angular velocity into rotation (Transform.Rotation)
--   - Wrap rotation between 0 and 2π to prevent unbounded growth
--
-- Update order:
--   This system should run AFTER input/physics constraint systems
--   (Paddle Control, Ball Physics) and BEFORE collision detection.
--   The standard order is:
--     1. Input systems (set velocities)
--     2. Physics constraint systems (clamp velocities)
--     3. Movement System (integrate into positions) ← THIS SYSTEM
--     4. Collision System (resolve overlaps)
--
-- Integration method:
--   Explicit Euler integration with delta time (DT):
--     velocity = velocity + acceleration * DT
--     position = position + velocity * DT

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Movement is

   type Movement_System is new System with null record;

   ---------------------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- (Transform and Motion)
   ---------------------------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Movement_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   ---------------------------------------------------------------------------
   -- Update
   -- Update will execute movement logic
   ---------------------------------------------------------------------------
   overriding 
   function Components_Needed (Self : Movement_System) 
      return ECS.Components.Component_Tag_Array;

   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Movement_System) return String;

end ECS.Systems_Movement;