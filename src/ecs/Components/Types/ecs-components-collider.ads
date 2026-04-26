with ECS.Entities;   use ECS.Entities;
with Math.Physics;   use Math.Physics;
with Math.Physics.AABBs;  use Math.Physics.AABBs;
with Ada.Containers.Vectors;


package ECS.Components.Collider is
   -- TODO: This should not be hardcoded here.
   type Collision_Layer is
     (Layer_None,
      Layer_Ball,
      Layer_Paddle,
      Layer_Brick,
      Layer_Wall);


   -- TODO: A mask is a bitwise integer, not an array.
   -- NOTE: Type safety can still be achieved, i.e. `type Mask is mod 2 ** 32`;
   -- A mask is a fixed-size array of layers this entity can collide with.
   type Collision_Mask is array (1 .. 4) of Collision_Layer;


   type Collider_Type is (Solid, Area);


   package Colliding_Entities is new Ada.Containers.Vectors
      (Index_Type    => Natural,
      Element_Type   => Entity_ID);


   type Collider_Component is new Component with record
      Name              : String(1..4) := "None";   --DEBUG
      Bounding_Box      : AABB;
      Layer             : Collision_Layer := Layer_None;
      Mask              : Collision_Mask  := (others => Layer_None);
      Collider_Form     : Collider_Type;
      Collided_Entities : Colliding_Entities.Vector;
   end record;
end;
