-- ecs-entities.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the entity record type used in the ECS system.
-- An Entity is just a Record that holds an ID of type Entity_ID

package ECS.Entities is

   type Entity_ID is new Natural;

   type Entity is record

      ID : Entity_ID;

   end record;

end ECS.Entities;