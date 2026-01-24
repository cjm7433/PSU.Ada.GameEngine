-- ecs-entities.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the entity record type used in the ECS system.

package ECS.Entities is
   type Entity_ID is new Natural;


   type Entity is record
      ID : Entity_ID;
   end record;
end ECS.Entities;
