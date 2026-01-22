-- ecs-types.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file is a shared type-definition package that defines the language the ECS system uses:
--    - It defines basic types used in the ECS (Entity Component System) framework.
-- This structure allows for easy modification and extension of types used throughout the ECS system.
-- This structure promotes strong typing and helps avoid circular dependencies between different ECS packages.
-- No logic is implemented here -- only contains type definitions.


package ECS.Types is

   type Entity_ID is new Natural;      -- Number used to identify an entity
   
   subtype Index is Natural;           -- Number used to index into arrays

   type Component_Type is (      -- Types of components in the ECS system
      Transform,
      Velocity
   -- Collider,
   -- Input,
   -- Sprite
   -- ...
   );
   
   -- Room to grow: Things like component IDs, system IDs, etc. can be added here as needed.

   -- Component masks can be defined as needed, e.g.:
   -- type Component_Mask is mod 2 ** 32;  -- Example: 32-bit mask for components

end ECS.Types;
