-- ecs-entities.ads
--
-- Entity type definition for the ECS system.
--    - An Entity is just an ID (unique integer).
--    - Entities have no behavior or data themselves:
--       * all data lives in components
--       * all behavior lives in systems.
--    - The entity ID is the key that ties components together.

package ECS.Entities is

   type Entity_ID is new Natural;

   type Entity is record
      ID : Entity_ID;
   end record;

end ECS.Entities;