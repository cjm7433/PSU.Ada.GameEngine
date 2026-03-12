-- ecs-components.ads
--
-- Root component types and infrastructure for the ECS system.
--    - All component types must inherit from Component
--    - Component'Tag is used to identify component types at runtime
--    - Systems declare their required components using Component_Tag_Array

with Ada.Tags;

package ECS.Components is

   -- Root Component type
   type Component is abstract tagged null record;

   -- Runtime tag type
   -- The runtime identity of a component type.
   subtype Component_Tag is Ada.Tags.Tag;

   -- Array of component tags for filtering
   -- A list of component type identities.
   -- A dynamic-length list of component tags.
   -- Systems declare these and the Store uses that list to filter Components.
   type Component_Tag_Array is array (Natural range <>) of Component_Tag;

end ECS.Components;