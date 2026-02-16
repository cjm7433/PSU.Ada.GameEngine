-- ecs-components.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the various component types used in the ECS system.
-- Each component type is defined as a separate record type.7
-- Add component types here

with Ada.Tags;

package ECS.Components is

   -- Root Component type
   type Component is abstract tagged null record;

   -- Runtime tag type
   -- “The runtime identity of a component type.”
   subtype Component_Tag is Ada.Tags.Tag;

   -- Array of component tags for filtering
   -- A list of component type identities.
   -- A dynamic-length list of component tags.
   type Component_Tag_Array is array (Natural range <>) of Component_Tag;

end ECS.Components;