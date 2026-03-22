-- component_table_test.adb
-- 
-- Comprehensive test suite for ECS.Component_Table generic package.
-- This test validates the dense storage mechanism used for components in the ECS system.
-- 
-- Tests cover:
--   1. Basic operations (Add, Has, Get, Remove)
--   2. Swap-remove behavior (ensuring cache-friendly removal)
--   3. Lookup index integrity
--   4. Edge cases (duplicate adds, removing nonexistent entities)

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Containers;
with ECS.Entities;            use ECS.Entities;
with ECS.Component_Table;

procedure Component_Table_Test is

   ------------------------------------------------------------
   -- Test Component Type: Simple integer component for testing
   ------------------------------------------------------------
   type Mock_Component is record
      
      Value : Integer;     -- Just a data value for this fake component to hold
   
   end record;


   ------------------------------------------------------------
   -- Hash function required for Component_Table instantiation
   ------------------------------------------------------------
   function Hash_Entity_ID (ID : Entity_ID) return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (ID);
   
   end Hash_Entity_ID;


   ------------------------------------------------------------
   -- Instantiate Component_Table with our Mock_Component
   ------------------------------------------------------------
   package Mock_Table is new ECS.Component_Table
     (Component_Type => Mock_Component,
      Hash           => Hash_Entity_ID);


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
   
      Total_Tests := Total_Tests + 1;        -- accumulate total tests run
   
      if Condition then
         Put_Line ("[PASS] " & Test_Name);
         Passed_Tests := Passed_Tests + 1;   -- accumulate passed tests run
   
      else
         Put_Line ("[FAIL] " & Test_Name);
         Failed_Tests := Failed_Tests + 1;   -- accumulate failed tests run
   
      end if;
   
   end Assert;


   ------------------------------------------------------------
   -- Test fixture: reusable table instance
   ------------------------------------------------------------
   Table : Mock_Table.Table;

begin
   Put_Line ("========================================");
   Put_Line ("Component_Table Test Suite");
   Put_Line ("========================================");
   New_Line;


   ------------------------------------------------------------
   -- TEST 1: Add component and verify it exists
   -- Purpose: Ensures Add() properly inserts component into data vector
   --          and creates lookup entry mapping Entity_ID to index
   ------------------------------------------------------------
   Put_Line ("Test 1: Add component and verify existence");

   Mock_Table.Add (Table, 1, (Value => 42));    -- Add component for Entity_ID 1 (with value 42)
   
   Assert (Mock_Table.Has (Table, 1), "Entity 1 should exist after Add");
   New_Line;


   ------------------------------------------------------------
   -- TEST 2: Get component and verify value
   -- Purpose: Ensures Get retrieves correct component data from vector
   --          using the lookup map to find the index
   ------------------------------------------------------------
   Put_Line ("Test 2: Get component value");
   
   declare
      Component : Mock_Component := Mock_Table.Get (Table, 1);
   
   begin
      Assert (Component.Value = 42, "Component value should be 42");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 3: Duplicate Add should be ignored
   -- Purpose: Ensures Add doesn't overwrite existing components
   --          (idempotent behavior prevents accidental data loss)
   ------------------------------------------------------------
   Put_Line ("Test 3: Duplicate Add should not overwrite");
   
   Mock_Table.Add (Table, 1, (Value => 999));
   
   declare
      Component : Mock_Component := Mock_Table.Get (Table, 1);
   
   begin
      Assert (Component.Value = 42, "Original value should be preserved");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 4: Add multiple entities
   -- Purpose: Verifies the table can store multiple components
   --          and maintain separate lookup entries
   ------------------------------------------------------------
   Put_Line ("Test 4: Add multiple entities");

   Mock_Table.Add (Table, 2, (Value => 100));
   Mock_Table.Add (Table, 3, (Value => 200));
   Mock_Table.Add (Table, 4, (Value => 300));
   
   Assert (Mock_Table.Has (Table, 1), "Entity 1 should exist");
   Assert (Mock_Table.Has (Table, 2), "Entity 2 should exist");
   Assert (Mock_Table.Has (Table, 3), "Entity 3 should exist");
   Assert (Mock_Table.Has (Table, 4), "Entity 4 should exist");
   New_Line;


   ------------------------------------------------------------
   -- TEST 5: Lookup_Index returns correct indices
   -- Purpose: Ensures the lookup map correctly maps Entity_IDs
   --          to their vector indices (critical for iteration)
   ------------------------------------------------------------
   Put_Line ("Test 5: Lookup indices are correct");

   Assert (Mock_Table.Lookup_Index (Table, 1) = 0, "Entity 1 at index 0");
   Assert (Mock_Table.Lookup_Index (Table, 2) = 1, "Entity 2 at index 1");
   Assert (Mock_Table.Lookup_Index (Table, 3) = 2, "Entity 3 at index 2");
   Assert (Mock_Table.Lookup_Index (Table, 4) = 3, "Entity 4 at index 3");
   New_Line;


   ------------------------------------------------------------
   -- TEST 6: Remove entity (tests swap-remove algorithm)
   -- Purpose: Validates O(1) swap-remove implementation:
   --          1. Last element swaps into removed slot
   --          2. Lookup for swapped element updates to new index
   --          3. Removed entity deleted from lookup
   --          4. Vector size decreases by 1
   ------------------------------------------------------------
   Put_Line ("Test 6: Remove entity (swap-remove)");
   
   -- Remove entity 2 (at index 1)
   -- Expected: Entity 4 (last) swaps into index 1
   Mock_Table.Remove (Table, 2);
   
   Assert (not Mock_Table.Has (Table, 2), "Entity 2 should be removed");
   
   Assert (Mock_Table.Has (Table, 1), "Entity 1 should still exist");
   Assert (Mock_Table.Has (Table, 3), "Entity 3 should still exist");
   Assert (Mock_Table.Has (Table, 4), "Entity 4 should still exist");
   New_Line;


   ------------------------------------------------------------
   -- TEST 7: Verify swap-remove updated indices correctly
   -- Purpose: After removal, the last element should have moved
   --          to fill the gap, and its lookup index should update
   ------------------------------------------------------------
   Put_Line ("Test 7: Swap-remove updated indices");
   
   -- Entity 4 should now be at index 1 (where entity 2 was)
   Assert (Mock_Table.Lookup_Index (Table, 4) = 1, 
           "Entity 4 should be at index 1 after swap");
   
   -- Verify values are still correct
   declare
      C1 : Mock_Component := Mock_Table.Get (Table, 1);
      C3 : Mock_Component := Mock_Table.Get (Table, 3);
      C4 : Mock_Component := Mock_Table.Get (Table, 4);
   
   begin
      Assert (C1.Value = 42, "Entity 1 value unchanged");
      Assert (C3.Value = 200, "Entity 3 value unchanged");
      Assert (C4.Value = 300, "Entity 4 value unchanged after swap");
   
   end;
   New_Line;


   ------------------------------------------------------------
   -- TEST 8: Remove nonexistent entity (should be safe no-op)
   -- Purpose: Ensures Remove doesn't crash on invalid Entity_ID
   ------------------------------------------------------------
   Put_Line ("Test 8: Remove nonexistent entity");
   
   Mock_Table.Remove (Table, 999);     -- should harmlessly return null
   
   -- If we get here without exception, test passes
   Assert (True, "Removing nonexistent entity should not raise exception");
   New_Line;


   ------------------------------------------------------------
   -- TEST 9: Remove all remaining entities
   -- Purpose: Validates table can be emptied completely
   ------------------------------------------------------------
   Put_Line ("Test 9: Remove all remaining entities");
   
   Mock_Table.Remove (Table, 1);
   Mock_Table.Remove (Table, 3);
   Mock_Table.Remove (Table, 4);
   
   Assert (not Mock_Table.Has (Table, 1), "Entity 1 removed");
   Assert (not Mock_Table.Has (Table, 3), "Entity 3 removed");
   Assert (not Mock_Table.Has (Table, 4), "Entity 4 removed");
   New_Line;

   ------------------------------------------------------------
   -- TEST 10: Table is empty after all removals
   -- Purpose: Verifies table state is clean and reusable
   ------------------------------------------------------------
   Put_Line ("Test 10: Table is empty");

   Assert (not Mock_Table.Has (Table, 1), "No entities should remain");
   Assert (not Mock_Table.Has (Table, 2), "No entities should remain");
   Assert (not Mock_Table.Has (Table, 3), "No entities should remain");
   Assert (not Mock_Table.Has (Table, 4), "No entities should remain");
   New_Line;


   ------------------------------------------------------------
   -- TEST 11: Can reuse table after clearing
   -- Purpose: Ensures table is reusable after all data removed
   ------------------------------------------------------------
   Put_Line ("Test 11: Reuse table after clearing");
   
   Mock_Table.Add (Table, 10, (Value => 777));
   
   Assert (Mock_Table.Has (Table, 10), "Can add to cleared table");
   Assert (Mock_Table.Get (Table, 10).Value = 777, "New data correct");
   New_Line;


   ------------------------------------------------------------
   -- Final Test Result Summary Readout
   ------------------------------------------------------------
   Put_Line ("========================================");
   Put_Line ("Component Table Test Summary:");

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

end Component_Table_Test;
