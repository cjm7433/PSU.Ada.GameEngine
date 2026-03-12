-- ecs-components.ads

-- This file is part of the ECS (Entity Component System) framework.
-- This file defines the various component types used in the ECS system.
-- Each component type is defined as a separate record type.

with ECS.Types;

package ECS.Components is

   type Transform_Component is record

      Owner    : ECS.Types.Entity_ID;     -- ID of the owning entity. Small concession to ECS model to facilitate lookups.
      X, Y     : Float;                   -- Position coordinates
      Rotation : Float;                   -- Rotation in degrees
      Scale    : Float;                   -- Uniform scale factor

   end record;
   

   type Velocity_Component is record

      DX, DY : Float;                  -- Velocity components in X and Y directions

   end record;

   -- Additional component types can be defined here as needed, e.g.:

   -- type Collider_Component is record
   --     Width, Height : Float;          -- Dimensions of the collider
   --  end record;

   -- type Input_Component is record
   --     Is_Active : Boolean;            -- Whether input is currently active
   --  end record;
   
   --  type Sprite_Component is record

   --     Image_Path : String(1 .. 256);   -- Path to the sprite image file
   --     Width, Height : Natural;          -- Dimensions of the sprite

   --  end record;

   -- etc.

end ECS.Components;
