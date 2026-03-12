-- ecs_store_test.adb
--
-- Comprehensive test suite for ECS.Store package.
-- This test validates the main ECS storage system including:
--   - Entity lifecycle (create, destroy, query)
--   - Component operations (add, remove, has, get)
--   - Query functions (get entities with specific components)
--
-- The Store is the central data structure that ties together:
--   - Entity management (unique ID assignment)
--   - Component storage (dense packed arrays per component type)
--   - Entity-component association (lookup maps)

with Ada.Text_IO;                      use Ada.Text_IO;
with ECS.Entities;                     use ECS.Entities;
with ECS.Components;                   use ECS.Components;
with ECS.Components_Transform;         use ECS.Components_Transform;
with ECS.Components_Motion;            use ECS.Components_Motion;
with ECS.Store;                        use ECS.Store;
with Math.Linear_Algebra;              use Math.Linear_Algebra;

procedure ECS_Store_Test is

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
         Passed_Tests := Passed_Tests + 1;
   
      else
         Put_Line ("[FAIL] " & Test_Name);
         Failed_Tests := Failed_Tests + 1;
   
      end if;
   end Assert;


   ------------------------------------------------------------
   -- Test fixture: ECS Store instance
   ------------------------------------------------------------
   S : Store;


begin
   Put_Line ("========================================");
   Put_Line ("ECS Store Test Suite");
   Put_Line ("========================================");
   New_Line;


   ------------------------------------------------------------
   -- TEST 1: Initialize store
   -- Purpose: Ensures Initialize clears all data and resets counters
   ------------------------------------------------------------
   Put_Line ("Test 1: Initialize store");
   
   Initialize (S);
   
   Assert (True, "Store initialized without exception");
   New_Line;


   ------------------------------------------------------------
   -- TEST 2: Create entities with unique IDs
   -- Purpose: Validates Create_Entity assigns sequential unique IDs
   --          starting from 1 (Next_Entity_ID initialized to 0)
   ------------------------------------------------------------
   Put_Line ("Test 2: Create entities with unique IDs");
   
   declare
      E1 : Entity_ID := Create_Entity (S);
      E2 : Entity_ID := Create_Entity (S);
      E3 : Entity_ID := Create_Entity (S);
   
   begin
   
      Assert (E1 = 1, "First entity ID should be 1");
      Assert (E2 = 2, "Second entity ID should be 2");
      Assert (E3 = 3, "Third entity ID should be 3");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 3: Has_Entity returns correct values
   -- Purpose: Verifies entity existence queries work correctly
   ------------------------------------------------------------
   Put_Line ("Test 3: Has_Entity correctness");

   Assert (Has_Entity (S, 1), "Entity 1 should exist");
   Assert (Has_Entity (S, 2), "Entity 2 should exist");
   Assert (Has_Entity (S, 3), "Entity 3 should exist");

   Assert (not Has_Entity (S, 999), "Entity 999 should not exist");
   New_Line;


   ------------------------------------------------------------
   -- TEST 4: Add Transform component
   -- Purpose: Tests adding Transform component (Position, Rotation, Scale)
   --          Default values: Position (0,0), Rotation 0, Scale (1,1)
   ------------------------------------------------------------
   Put_Line ("Test 4: Add Transform component");
   
   Add_Component (S, 1, Transform'Tag);
   
   Assert (Has_Component (S, 1, Transform'Tag), "Entity 1 should have Transform");
   New_Line;


   ------------------------------------------------------------
   -- TEST 5: Add Motion component
   -- Purpose: Tests adding Motion component (velocities, accelerations)
   --          Default values: all zeros
   ------------------------------------------------------------
   Put_Line ("Test 5: Add Motion component");
   
   Add_Component (S, 1, Motion'Tag);
   
   Assert (Has_Component (S, 1, Motion'Tag), "Entity 1 should have Motion");
   New_Line;


   ------------------------------------------------------------
   -- TEST 6: Get Transform component and verify default values
   -- Purpose: Validates Get_Component retrieves correct data
   --          and default initialization works as expected
   ------------------------------------------------------------
   Put_Line ("Test 6: Get Transform component with default values");
   
   declare
      T : Transform := Transform (Get_Component (S, 1, Transform'Tag));
   
   begin
   
      Assert (T.Position.X = 0.0, "Default Position.X is 0.0");
      Assert (T.Position.Y = 0.0, "Default Position.Y is 0.0");
      Assert (T.Rotation = 0.0, "Default Rotation is 0.0");
      Assert (T.Scale.X = 1.0, "Default Scale.X is 1.0");
      Assert (T.Scale.Y = 1.0, "Default Scale.Y is 1.0");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 7: Get Motion component and verify default values
   -- Purpose: Validates Motion component defaults (all zeros)
   ------------------------------------------------------------
   Put_Line ("Test 7: Get Motion component with default values");
   
   declare
      M : Motion := Motion (Get_Component (S, 1, Motion'Tag));
   
   begin
   
      Assert (M.Linear_Velocity.X = 0.0, "Default Linear_Velocity.X is 0.0");
      Assert (M.Linear_Velocity.Y = 0.0, "Default Linear_Velocity.Y is 0.0");
      Assert (M.Angular_Velocity.X = 0.0, "Default Angular_Velocity.X is 0.0");
      Assert (M.Angular_Velocity.Y = 0.0, "Default Angular_Velocity.Y is 0.0");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 8: Add components to multiple entities
   -- Purpose: Tests component distribution across entities:
   --          Entity 1: Transform + Motion
   --          Entity 2: Transform only
   --          Entity 3: Motion only
   ------------------------------------------------------------
   Put_Line ("Test 8: Add components to multiple entities");
   
   Add_Component (S, 2, Transform'Tag);
   Add_Component (S, 3, Motion'Tag);
   
   Assert (Has_Component (S, 2, Transform'Tag), "Entity 2 has Transform");
   Assert (not Has_Component (S, 2, Motion'Tag), "Entity 2 lacks Motion");
   Assert (Has_Component (S, 3, Motion'Tag), "Entity 3 has Motion");
   Assert (not Has_Component (S, 3, Transform'Tag), "Entity 3 lacks Transform");
   New_Line;


   ------------------------------------------------------------
   -- TEST 9: Get_Entity_IDs for single component type
   -- Purpose: Tests query for all entities with Transform component
   --          Should return entities 1 and 2
   ------------------------------------------------------------
   Put_Line ("Test 9: Get entities with Transform component");
   
   declare
      Entities : Entity_ID_Array_Access := Get_Entity_IDs (S, Transform'Tag);
   
   begin
   
      if Entities /= null then
         Assert (Entities'Length = 2, "Should find 2 entities with Transform");
         
         -- Check if results contain expected entities
         declare
            Has_E1 : Boolean := False;
            Has_E2 : Boolean := False;
         
         begin
            
            for I in Entities'Range loop
               if Entities (I) = 1 then
                  Has_E1 := True;
               
               elsif Entities (I) = 2 then
                  Has_E2 := True;
               end if;
            end loop;
            
            Assert (Has_E1, "Results should include Entity 1");
            Assert (Has_E2, "Results should include Entity 2");
         end;
      
      else
         Assert (False, "Query should return non-null result");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 10: Get_Entities_With for multiple component types
   -- Purpose: Tests query for entities with BOTH Transform AND Motion
   --          Should return only entity 1
   ------------------------------------------------------------
   Put_Line ("Test 10: Get entities with Transform AND Motion");
   
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Entities : Entity_ID_Array_Access := Get_Entities_With (S, Tags);
   
   begin
   
      if Entities /= null then
         Assert (Entities'Length = 1, "Should find 1 entity with both");
         Assert (Entities (0) = 1, "Entity 1 has both components");
   
      else
         Assert (False, "Query should return non-null result");
      end if;
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 11: Remove component
   -- Purpose: Tests Remove_Component properly removes from both
   --          the component table and lookup map
   ------------------------------------------------------------
   Put_Line ("Test 11: Remove component");
   
   Remove_Component (S, 1, Motion'Tag);
   
   Assert (not Has_Component (S, 1, Motion'Tag),
           "Entity 1 should not have Motion after removal");
   
   Assert (Has_Component (S, 1, Transform'Tag),
           "Entity 1 should still have Transform");
   New_Line;


   ------------------------------------------------------------
   -- TEST 12: Query after component removal
   -- Purpose: Verifies query results update after removal
   --          Now no entities have both Transform AND Motion
   ------------------------------------------------------------
   Put_Line ("Test 12: Query after component removal");
   
   declare
      Tags : Component_Tag_Array := (Transform'Tag, Motion'Tag);
      Entities : Entity_ID_Array_Access := Get_Entities_With (S, Tags);
   
   begin
   
      Assert (Entities = null, "No entities should have both after removal");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 13: Destroy entity
   -- Purpose: Tests Destroy_Entity removes entity and all its components
   ------------------------------------------------------------
   Put_Line ("Test 13: Destroy entity");
   
   Destroy_Entity (S, 1);
   
   Assert (not Has_Entity (S, 1), "Entity 1 should not exist");
   Assert (not Has_Component (S, 1, Transform'Tag), "Entity 1 Transform should be removed");
   New_Line;


   ------------------------------------------------------------
   -- TEST 14: Destroy nonexistent entity (safe no-op)
   -- Purpose: Ensures Destroy_Entity handles invalid IDs gracefully
   ------------------------------------------------------------
   Put_Line ("Test 14: Destroy nonexistent entity");
   
   Destroy_Entity (S, 999);      -- should do nothing
   
   Assert (True, "Destroying nonexistent entity should not raise exception");
   New_Line;


   ------------------------------------------------------------
   -- TEST 15: Remaining entities still valid
   -- Purpose: Verifies destroying one entity doesn't affect others
   ------------------------------------------------------------
   Put_Line ("Test 15: Other entities still valid after destruction");
   
   Assert (Has_Entity (S, 2), "Entity 2 should still exist");
   Assert (Has_Entity (S, 3), "Entity 3 should still exist");
   
   Assert (Has_Component (S, 2, Transform'Tag), "Entity 2 Transform intact");
   Assert (Has_Component (S, 3, Motion'Tag), "Entity 3 Motion intact");
   New_Line;


   ------------------------------------------------------------
   -- TEST 16: Re-initialize store clears all data
   -- Purpose: Tests Initialize can reset store to clean state
   ------------------------------------------------------------
   Put_Line ("Test 16: Re-initialize clears all data");
   
   Initialize (S);      -- clear everything
   
   Assert (not Has_Entity (S, 1), "Entity 1 gone after re-init");
   Assert (not Has_Entity (S, 2), "Entity 2 gone after re-init");
   Assert (not Has_Entity (S, 3), "Entity 3 gone after re-init");
   New_Line;


   ------------------------------------------------------------
   -- TEST 17: Create entity after re-initialization
   -- Purpose: Verifies store is fully reusable after Initialize
   --          and Entity_ID counter resets to 0
   ------------------------------------------------------------
   Put_Line ("Test 17: Create entity after re-initialization");
   
   declare
      New_E : Entity_ID := Create_Entity (S);
   
   begin
   
      Assert (New_E = 1, "First entity after re-init should be ID 1");
      Assert (Has_Entity (S, New_E), "New entity should exist");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 18: Stress test - Create many entities
   -- Purpose: Tests store handles larger entity counts
   ------------------------------------------------------------
   Put_Line ("Test 18: Create many entities");
   declare
      Entity_Count : constant := 10000;      -- modify this for how stressy you want
   
   begin
   
      Initialize (S);  -- Clean slate
      for I in 1 .. Entity_Count loop
         
         declare
            E : Entity_ID := Create_Entity (S);
         
         begin
            Add_Component (S, E, Transform'Tag);
         end;
      
      end loop;
      
      -- Verify all created
      declare
         All_Exist : Boolean := True;
      
      begin
         
         for I in 1 .. Entity_Count loop
            if not Has_Entity (S, Entity_ID (I)) then
               All_Exist := False;
               exit;
            end if;
         
         end loop;
         Assert (All_Exist, "All " & Integer'Image (Entity_Count) & " entities should exist");
      end;
      
      -- Verify query returns all
      declare
         Entities : Entity_ID_Array_Access := Get_Entity_IDs (S, Transform'Tag);
      
      begin
         
         if Entities /= null then
            Assert (Entities'Length = Entity_Count,
                    "Query should find all " & Integer'Image (Entity_Count) & " entities");
         else
            Assert (False, "Query should return entities");
         end if;
      
      end;
   end;
   New_Line;


   ------------------------------------------------------------
   -- Final Test Result Summary Readout
   ------------------------------------------------------------
   Put_Line ("========================================");
   Put_Line ("ECS-Store Test Summary:");
   Put_Line ("  Total Tests:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed:       " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed:       " & Natural'Image (Failed_Tests));
   
   if Failed_Tests = 0 then
      Put_Line ("========================================");
      Put_Line ("ALL TESTS PASSED! :)");
      Put_Line ("========================================");
   else
      Put_Line ("========================================");
      Put_Line ("SOME TESTS FAILED! :()");
      Put_Line ("========================================");
   end if;

   New_Line;
   New_Line;

end ECS_Store_Test;
