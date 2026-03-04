-- ecs-systems_brick_destruction.adb
--
-- Implementation of Brick Destruction System
-- Handles brick death timers and entity removal.
--
-- IMPORTANT - collect-then-destroy pattern:
--    Ada.Containers vectors raise Program_Error ("tamper with cursors")
--    if you modify the vector while a reference into it is live, or
--    while iterating over a derived structure.
--    (This error was raised previously but routed out)
--    TODO: Maybe delete this section
--
--   The fix is two passes:
--     Pass 1: iterate the entity list, update timers, collect IDs
--             whose timers have expired into a local To_Destroy array.
--     Pass 2: after the loop finishes (all renames released), destroy
--             each collected entity.

with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Brick;          use ECS.Components.Brick;

package body ECS.Systems_Brick_Destruction is

   --------------------------------------------------------------
   -- Components_Needed
   -- Components_Needed is the list of required components 
   -- Components required: Brick
   --------------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Brick_Destruction_System)
      return ECS.Components.Component_Tag_Array
   is
   begin
      return (0 => ECS.Components.Brick.Brick_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Update executes brick destruction logic
   -- Update: tick death timers, then destroy expired bricks
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Brick_Destruction_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;
   
   begin
      
      -- Get entities with the required components
      Entities := S.Get_Entities_With (Self.Components_Needed);

      if Entities = null then
         return;
      end if;

      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- PASS 1: Update timers and collect expired entity IDs.
      --
      -- We declare To_Destroy with the same length as Entities
      --    (worst case: every brick expires this frame) and track
      --    how many are actually filled with Destroy_Count.
      --
      -- All renames of vector elements are released when this
      --    declare block exits, which is before Pass 2 runs.
      --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      declare
         To_Destroy    : Entity_ID_Array (Entities'Range);
         Destroy_Count : Natural := 0;
      
      begin

         for I in Entities'Range loop
            
            declare
               E : constant Entity_ID := Entities (I);
               Idx : constant Natural := S.Brick.Lookup (E);
               B : Brick_Component renames S.Brick.Data (Idx);
            
            begin
               if B.Is_Dying then
                  B.Death_Timer := B.Death_Timer - DT;

                  if B.Death_Timer <= 0.0 then
                     -- TODO: award score from B.Points
                     -- TODO: spawn power-up if B.Brick_Kind = Special
                     To_Destroy (Destroy_Count) := E;
                     Destroy_Count := Destroy_Count + 1;
                  end if;
               end if;
            end;
            -- The renames 'B' is released here, before we ever
            -- call Destroy_Entity.
         end loop;

         --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         -- PASS 2: Destroy collected entities.
         --
         -- No renames or cursors into the vector are live here,
         --    so Destroy_Entity can safely modify it.
         --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         for J in 0 .. Destroy_Count - 1 loop
            Destroy_Entity (S, To_Destroy (J));
         end loop;

      end; -- To_Destroy declared block

   end Update;

end ECS.Systems_Brick_Destruction;
