-- All component types must inherit from Component
-- Component'Tag is used to identify component types at runtime

with Ada.Tags;


package ECS.Components is
   type Component is abstract tagged null record;


   subtype Component_Tag is Ada.Tags.Tag;


   type Component_Tag_Array is array (Natural range <>) of Component_Tag;
end ECS.Components;