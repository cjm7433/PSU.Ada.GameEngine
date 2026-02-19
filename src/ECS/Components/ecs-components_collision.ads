-- ecs-components_collision.ads
--
-- Collision component for the ECS system.
-- Defines an axis-aligned bounding box (AABB) for collision detection.
--
-- Collision filtering uses Ada enumerations and arrays instead of
-- bitwise integer masks, which is cleaner and more type-safe.
--
-- IMPORTANT: Enum values are prefixed with "Layer_" to avoid name
-- clashes with component types (Ball, Brick, Paddle) that are
-- visible in the same scope via use clauses in ecs-store.ads.
--
-- NOTE: If you are confused about collision layers and/or masks, 
--  please see ecs-systems-test.adb, speficially around line 884 
--  where they are used in testing.

with ECS.Components;          use ECS.Components;

package ECS.Components_Collision is

   -- Named collision categories.
   -- Prefixed with "Layer_" to avoid clashing with component type names.
   type Collision_Layer is
     (Layer_None,
      Layer_Ball,
      Layer_Paddle,
      Layer_Brick,
      Layer_Wall);

   -- A mask is a fixed-size array of layers this entity can collide with.
   -- Unused slots should be set to Layer_None.
   type Collision_Mask is array (1 .. 4) of Collision_Layer;
   
   -- Example:
   --   Ball collides with Paddle, Brick, Wall:
   --     Mask => (Layer_Paddle, Layer_Brick, Layer_Wall, Layer_None)
   --   Paddle collides with Ball only:
   --     Mask => (Layer_Ball, Layer_None, Layer_None, Layer_None)
   

   -- Empty mask constant for convenience
   No_Mask : constant Collision_Mask := (others => Layer_None);

   -- Collision component: Axis-Aligned Bounding Box (AABB)
   type Collision is new Component with record

      -- Half-extents of the bounding box (centered on Transform.Position)
      -- Full width  = Half_Width  * 2
      -- Full height = Half_Height * 2
      -- TODO: Talk to Grant grimace emoji
      Half_Width  : Float := 0.5;
      Half_Height : Float := 0.5;

      -- What category this entity belongs to
      Layer       : Collision_Layer := Layer_None;

      -- What categories this entity can collide with
      Mask        : Collision_Mask  := No_Mask;

      -- Trigger: detects overlap but does not physically resolve it.
      -- Useful for scoring zones, power-up pickups, etc.
      Is_Trigger  : Boolean := False;

   end record;

end ECS.Components_Collision;
