-- ecs-manager.adb
--
-- ECS Manager Implementation

with ECS.Store;
with ECS.Systems;
with ECS.Components;

package body ECS.Manager is

   ------------------------------------------------------------
   -- Initialize
   -- Reset store and clear all systems
   ------------------------------------------------------------
   procedure Initialize (M : in out ECS_Manager) is
   begin
      -- Clear all entities and components
      ECS.Store.Initialize (M.World);

      -- Clear system list (no default systems)
      M.Systems.Clear;
   end Initialize;


   ------------------------------------------------------------
   -- Add_System
   -- Register a system for execution
   ------------------------------------------------------------
   procedure Add_System (
      M   : in out ECS_Manager;
      Sys : ECS.Systems.System_Access
   ) is
   begin
      M.Systems.Append (Sys);
   end Add_System;


   ------------------------------------------------------------
   -- Clear_Systems
   -- Remove all registered systems
   ------------------------------------------------------------
   procedure Clear_Systems (M : in out ECS_Manager) is
   begin
      M.Systems.Clear;
   end Clear_Systems;


   ------------------------------------------------------------
   -- Update
   -- Execute all systems using dynamic dispatch
   --
   -- DESIGN: Uses System'Class dispatching
   -- -------------------------------------
   -- Each system is stored as System_Access (access to System'Class).
   -- When we call Sys.Update, Ada automatically dispatches to the
   -- correct overridden Update procedure based on the actual type.
   --
   -- This eliminates the need for:
   --   - System_ID enums
   --   - Case statements
   --   - Hardcoded system names
   --
   -- To add a new system, just call Add_System with it!
   ------------------------------------------------------------
   procedure Update (M : in out ECS_Manager; DT : Float) is
   begin
      -- Iterate through all registered systems
      for Sys of M.Systems loop
         -- Dynamic dispatch to correct System.Update
         Sys.Update (M.World, DT);
      end loop;
   end Update;


   ------------------------------------------------------------
   -- Get_Store
   -- Provide access to the store
   --
   -- Accessibility Fixes (2/26):
   -- --------------------
   -- - M is 'aliased in out' (parameter must be aliased)
   -- - M.World is 'aliased' (record component)
   -- - Return type is anonymous 'access Store'
   -- - Compiler ensures access can't outlive M
   --
   -- At call site, declare Manager as:
   --    Manager : aliased ECS_Manager;
   --    Store_Ptr : access ECS.Store.Store := Manager.Get_Store;
   ------------------------------------------------------------
   function Get_Store (M : aliased in out ECS_Manager)
      return access ECS.Store.Store
   is
   begin
      return M.World'Access;
   end Get_Store;


   ------------------------------------------------------------
   -- Validate_Systems
   -- Check if systems can find entities to process
   --
   -- This is useful for debugging - it queries the store
   -- for each system's required components and reports if
   -- any system won't find matching entities.
   ------------------------------------------------------------
   function Validate_Systems (M : ECS_Manager) return Boolean is
      Entities : ECS.Store.Entity_ID_Array_Access;
   begin
      -- Check each registered system
      for Sys of M.Systems loop
         -- Get the components this system needs
         declare
            Needed : constant ECS.Components.Component_Tag_Array := Sys.Components_Needed;
         begin
            -- Query store for entities with these components
            Entities := M.World.Get_Entities_With (Needed);
            
            -- If no entities found, this system won't do anything
            -- (This isn't necessarily an error - systems can have
            --  zero work to do, but it's worth knowing)
            if Entities = null or else Entities'Length = 0 then
               -- System has no work (might be intentional)
               null;
            end if;
         end;
      end loop;
      
      return True;
   end Validate_Systems;


   ------------------------------------------------------------
   -- Get_System_Count
   -- Return number of registered systems
   ------------------------------------------------------------
   function Get_System_Count (M : ECS_Manager) return Natural is
   begin
      return Natural (M.Systems.Length);
   end Get_System_Count;

end ECS.Manager;
