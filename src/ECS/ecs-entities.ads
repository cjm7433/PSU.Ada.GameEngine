-- ecs-entities.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the entity record type used in the ECS system.

with ECS.Types;

package ECS.Entities is

---------------------------------------------------------------------------------
-- Stub: Move this later!

type Component_Type is (Transform, Velocity);  -- Example component types
---------------------------------------------------------------------------------

type Component_Set is array (ECS.Types.Component_Type) of Boolean; -- Set of components an entity has

   type Entity_Record is record

      ID : ECS.Types.Entity_ID;                    -- Unique identifier for the entity
      Tags  : Component_Set := (others => False);  -- Component presence set for the entity
      
      -- Additional entity metadata can be added here as needed
   
   end record;

end ECS.Entities;
