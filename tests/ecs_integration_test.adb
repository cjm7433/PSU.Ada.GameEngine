-- ecs_integration_test.adb
--
-- Integration test for the complete ECS system.
-- This test simulates a realistic game engine workflow:
--   1. Create entities representing game objects
--   2. Attach components (Transform for position, Motion for movement)
--   3. Query entities by component composition
--   4. Simulate frame updates (like a game loop)
--   5. Clean up resources
--
-- This validates that all ECS components work together correctly
-- in a real-world usage scenario.
--
-- (These are initial integration tests written when the only System in ECS was Movement)

with Ada.Text_IO;                      use Ada.Text_IO;
with ECS.Entities;                     use ECS.Entities;
with ECS.Components;                   use ECS.Components;
with ECS.Components_Transform;         use ECS.Components_Transform;
with ECS.Components_Motion;            use ECS.Components_Motion;
with ECS.Store;                        use ECS.Store;
with Math.Linear_Algebra;              use Math.Linear_Algebra;

procedure ECS_Integration_Test is

   ------------------------------------------------------------
   -- Test state tracking
   ------------------------------------------------------------
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;


   ------------------------------------------------------------
   -- Helper procedure to report test results
   ------------------------------------------------------------
   procedure Assert (Condition : Boolean; Test_Name : String) is
   
   begin

      Total_Tests := Total_Tests + 1;
      if Condition then
         Put_Line ("[PASS] " & Test_Name);
         Passed_Tests := Passed_Tests + 1;      -- accumulate passed tests run
      
      else
         Put_Line ("[FAIL] " & Test_Name);
         Failed_Tests := Failed_Tests + 1;      -- accumulate failed tests run
      end if;
   
   end Assert;


   ------------------------------------------------------------
   -- Game simulation: ECS Store
   ------------------------------------------------------------
   World : Store;


   ------------------------------------------------------------
   -- Entity IDs for game objects
   ------------------------------------------------------------
   Player_ID        : Entity_ID;
   Enemy1_ID        : Entity_ID;
   Enemy2_ID        : Entity_ID;
   Static_Wall_ID   : Entity_ID;


begin
   Put_Line ("========================================");
   Put_Line ("ECS Integration Test: Game Simulation");
   Put_Line ("========================================");
   Put_Line ("Simulating a simple game with:");
   Put_Line ("  - 1 Player (Transform + Motion)");
   Put_Line ("  - 2 Enemies (Transform + Motion)");
   Put_Line ("  - 1 Static Wall (Transform only)");
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 1: Initialize game world
   -- Store.Initialize()
   ------------------------------------------------------------
   Put_Line ("=== Scenario 1: Initialize Game World ===");
   
   Initialize (World);
   
   Assert (True, "World initialized");
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 2: Create game entities
   -- Store.Create_Entity()
   ------------------------------------------------------------
   Put_Line ("=== Scenario 2: Create Game Entities ===");
   
   -- Create player entity
   Player_ID := Create_Entity (World);
   Put_Line ("Created Player (Entity ID:" & Entity_ID'Image (Player_ID) & ")");
   
   -- Create enemy entities
   Enemy1_ID := Create_Entity (World);
   Put_Line ("Created Enemy 1 (Entity ID:" & Entity_ID'Image (Enemy1_ID) & ")");
   
   Enemy2_ID := Create_Entity (World);
   Put_Line ("Created Enemy 2 (Entity ID:" & Entity_ID'Image (Enemy2_ID) & ")");
   
   -- Create static wall entity
   Static_Wall_ID := Create_Entity (World);
   Put_Line ("Created Static Wall (Entity ID:" & Entity_ID'Image (Static_Wall_ID) & ")");
   
   Assert (Has_Entity (World, Player_ID), "Player entity exists");
   Assert (Has_Entity (World, Enemy1_ID), "Enemy1 entity exists");
   Assert (Has_Entity (World, Enemy2_ID), "Enemy2 entity exists");
   Assert (Has_Entity (World, Static_Wall_ID), "Wall entity exists");
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 3: Attach components to entities
   -- Store.Add_Component()
   -- This defines what properties each game object has
   ------------------------------------------------------------
   Put_Line ("=== Scenario 3: Attach Components ===");
   
   -- Player: needs position and motion
   Add_Component (World, Player_ID, Transform'Tag);
   Add_Component (World, Player_ID, Motion'Tag);
   Put_Line ("Player: Transform + Motion");
   
   -- Enemies: need position and motion
   Add_Component (World, Enemy1_ID, Transform'Tag);
   Add_Component (World, Enemy1_ID, Motion'Tag);
   Put_Line ("Enemy1: Transform + Motion");
   
   Add_Component (World, Enemy2_ID, Transform'Tag);
   Add_Component (World, Enemy2_ID, Motion'Tag);
   Put_Line ("Enemy2: Transform + Motion");
   
   -- Static wall: position only (doesn't move)
   Add_Component (World, Static_Wall_ID, Transform'Tag);
   Put_Line ("Wall: Transform only (static)");
   
   Assert (Has_Component (World, Player_ID, Transform'Tag),
           "Player has Transform");
   Assert (Has_Component (World, Player_ID, Motion'Tag),
           "Player has Motion");
   Assert (Has_Component (World, Static_Wall_ID, Transform'Tag),
           "Wall has Transform");
   Assert (not Has_Component (World, Static_Wall_ID, Motion'Tag),
           "Wall doesn't have Motion (static)");
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 4: Query entities by component signature
   -- Store.Get_Entity_IDs()
   -- This simulates how game systems would find relevant entities
   ------------------------------------------------------------
   Put_Line ("=== Scenario 4: Query Entities for Systems ===");
   
   -- Query for all entities with Transform (for rendering system)
   Put_Line ("Query: All renderable entities (Transform)");
   declare
      Renderables : Entity_ID_Array_Access := Get_Entity_IDs (World, Transform'Tag);
   
   begin
      
      if Renderables /= null then
         Put_Line ("  Found" & Natural'Image (Renderables'Length) & " entities");
         Assert (Renderables'Length = 4, "Should find 4 renderable entities");
      
      else
         Assert (False, "Renderable query failed");
      end if;
   
   end;
   New_Line;
   
   -- Query for all entities with Transform + Motion (for movement system)
   Put_Line ("Query: All moving entities (Transform + Motion)");
   
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Movables : Entity_ID_Array_Access := Get_Entities_With (World, Tags);
   
   begin
      
      if Movables /= null then
         
         Put_Line ("  Found" & Natural'Image (Movables'Length) & " entities");
         Assert (Movables'Length = 3, "Should find 3 moving entities");
         
         -- Verify the moving entities are player and enemies
         declare
            Found_Player : Boolean := False;
            Found_Enemy1 : Boolean := False;
            Found_Enemy2 : Boolean := False;
            Found_Wall   : Boolean := False;
         
         begin
            for I in Movables'Range loop
               if Movables (I) = Player_ID then
                  Found_Player := True;
               elsif Movables (I) = Enemy1_ID then
                  Found_Enemy1 := True;
               elsif Movables (I) = Enemy2_ID then
                  Found_Enemy2 := True;
               elsif Movables (I) = Static_Wall_ID then
                  Found_Wall := True;
               end if;
            end loop;
            
            Assert (Found_Player, "Moving entities include Player");
            Assert (Found_Enemy1, "Moving entities include Enemy1");
            Assert (Found_Enemy2, "Moving entities include Enemy2");
            Assert (not Found_Wall, "Moving entities don't include Wall");
         end;
      
      else
         Assert (False, "Movable query failed");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 5: Simulate game frame (read/write components)
   -- Store.Get_Entities_With()
   -- This simulates what a movement system would do each frame
   ------------------------------------------------------------
   Put_Line ("=== Scenario 5: Simulate Game Frame ===");
   Put_Line ("Movement System: Processing entities with Transform + Motion");
   
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Movables : Entity_ID_Array_Access := Get_Entities_With (World, Tags);
   
   begin

      if Movables /= null then
         
         for I in Movables'Range loop
            
            declare
               E : constant Entity_ID := Movables (I);
               T : Transform := Transform (Get_Component (World, E, Transform'Tag));
               M : Motion := Motion (Get_Component (World, E, Motion'Tag));
            
            begin
               Put_Line ("  Processing Entity" & Entity_ID'Image (E));
               Put_Line ("    Position: (" 
                         & Float'Image (T.Position.X) & "," 
                         & Float'Image (T.Position.Y) & ")");
               Put_Line ("    Velocity: (" 
                         & Float'Image (M.Linear_Velocity.X) & "," 
                         & Float'Image (M.Linear_Velocity.Y) & ")");
               
               -- In a real system, we'd update position based on velocity here
               -- Note: This is read-only in this test since we can't write back
               --       without modifying the component table directly
            end;
         end loop;
         
         Assert (True, "Processed all moving entities");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 6: Dynamic gameplay - Enemy defeated
   -- Store.Destroy_Entity()
   -- Simulates removing an enemy during gameplay
   ------------------------------------------------------------
   Put_Line ("=== Scenario 6: Enemy Defeated (Remove Entity) ===");
   Put_Line ("Enemy1 defeated - removing from world");
   
   Destroy_Entity (World, Enemy1_ID);
   
   Assert (not Has_Entity (World, Enemy1_ID), "Enemy1 removed from world");
   
   -- Verify other entities still exist
   Assert (Has_Entity (World, Player_ID), "Player still exists");
   Assert (Has_Entity (World, Enemy2_ID), "Enemy2 still exists");
   Assert (Has_Entity (World, Static_Wall_ID), "Wall still exists");
   
   -- Re-query moving entities - should now be 2 (player + enemy2)
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Movables : Entity_ID_Array_Access := Get_Entities_With (World, Tags);
   
   begin
      
      if Movables /= null then
         
         Assert (Movables'Length = 2, 
                 "Only 2 moving entities after Enemy1 removed");
      else
         Assert (False, "Query should still return entities");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 7: Remove component (make enemy static)
   -- Store.Remove_Component()
   -- Simulates changing entity behavior at runtime
   ------------------------------------------------------------
   Put_Line ("=== Scenario 7: Freeze Enemy2 (Remove Motion) ===");
   Put_Line ("Enemy2 frozen - removing Motion component");
   
   Remove_Component (World, Enemy2_ID, Motion'Tag);
   
   Assert (Has_Component (World, Enemy2_ID, Transform'Tag),
           "Enemy2 still has Transform");
   Assert (not Has_Component (World, Enemy2_ID, Motion'Tag),
           "Enemy2 no longer has Motion");
   
   -- Re-query moving entities - should now be 1 (only player)
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Movables : Entity_ID_Array_Access := Get_Entities_With (World, Tags);
   
   begin
      
      if Movables /= null then
         
         Assert (Movables'Length = 1, 
                 "Only 1 moving entity after Enemy2 frozen");
         Assert (Movables (0) = Player_ID,
                 "Remaining moving entity is Player");
      
      else
         Assert (False, "Player should still be moving");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 8: Game over - clean up all entities
   -- Store.Destroy_Entity() + Store.Get_Entity_IDs()
   ------------------------------------------------------------
   Put_Line ("=== Scenario 8: Game Over (Cleanup) ===");
   
   Destroy_Entity (World, Player_ID);
   Destroy_Entity (World, Enemy2_ID);
   Destroy_Entity (World, Static_Wall_ID);
   
   Assert (not Has_Entity (World, Player_ID), "Player cleaned up");
   Assert (not Has_Entity (World, Enemy2_ID), "Enemy2 cleaned up");
   Assert (not Has_Entity (World, Static_Wall_ID), "Wall cleaned up");
   
   -- Verify queries return empty
   declare
      Renderables : Entity_ID_Array_Access := Get_Entity_IDs (World, Transform'Tag);
   
   begin
      Assert (Renderables = null, "No entities remain");
   end;
   New_Line;


   ------------------------------------------------------------
   -- SCENARIO 9: Restart game (re-initialize)
   -- Store.Initialize() + Store.Create_Entity() + Store.Add_Component()
   ------------------------------------------------------------
   Put_Line ("=== Scenario 9: Restart Game ===");
   Initialize (World);
   
   -- Create new entities
   declare
      New_Player : Entity_ID := Create_Entity (World);
   
   begin
   
      Add_Component (World, New_Player, Transform'Tag);
      Add_Component (World, New_Player, Motion'Tag);
      
      Assert (New_Player = 1, "Entity IDs reset after re-init");
      
      Assert (Has_Entity (World, New_Player), "New player created");
      
      Assert (Has_Component (World, New_Player, Transform'Tag),
              "New player has Transform");
      
      Assert (Has_Component (World, New_Player, Motion'Tag),
              "New player has Motion");
   end;
   New_Line;


   ------------------------------------------------------------
   -- Final Test Result Summary Readout
   ------------------------------------------------------------
   Put_Line ("========================================");
   Put_Line ("Integration Test Summary:");
   Put_Line ("  Total Tests:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed:       " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed:       " & Natural'Image (Failed_Tests));
   
   if Failed_Tests = 0 then
      Put_Line ("========================================");
      Put_Line ("ALL INTEGRATION TESTS PASSED! :)");
      Put_Line ("ECS system is working correctly!");
      Put_Line ("========================================");
   else
      Put_Line ("========================================");
      Put_Line ("SOME INTEGRATION TESTS FAILED! :)");
      Put_Line ("========================================");
   end if;

   New_Line;
   New_Line;

end ECS_Integration_Test;
