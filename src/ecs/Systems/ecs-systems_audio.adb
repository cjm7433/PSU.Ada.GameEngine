-- ecs-systems_audio.adb
--
-- Implementation of Audio System
-- Plays audio tracks
with Ada.Text_IO;                   use Ada.Text_IO;
with ECS.Store;                     use ECS.Store;
with ECS.Entities;                  use ECS.Entities;
with ECS.Components.Audio;          use ECS.Components.Audio;
with Audio;                         use Audio;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

package body ECS.Systems_Audio is

   --------------------------------------------------------
   -- Components_Required
   -- Components_Needed is the list of required components 
   -- Components required: Audio
   --------------------------------------------------------
   overriding
   function Components_Needed
     (Self : Audio_System)
      return ECS.Components.Component_Tag_Array is
   begin
      -- Paddle Control System requires: Audio
      return (0 => ECS.Components.Audio.Audio_Component'Tag);
   end Components_Needed;


   ------------------------------------------------------------
   -- Update
   -- Update: Play audio files
   ------------------------------------------------------------
   overriding
   procedure Update
     (Self : in out Audio_System;
      S    : in out Store.Store;
      DT   : Float) is

      Entities : Entity_ID_Array_Access;

   begin
      -- Get entities with the required components
      -- (Audio component)
      Entities := S.Get_Entities_With (Self.Components_Needed);

      -- If no audio entities exist, nothing to do
      if Entities = null then
         return;
      end if;

      -- Process each audio entity
      for I in Entities'Range loop

         declare
            E : constant Entity_ID := Entities (I);

            -- Get components for this audio entity
            Index_Audio : constant Natural := S.Audio.Lookup (E);
            A : Audio_Component renames S.Audio.Data (Index_Audio);

         begin

            -- Play audio track if requested
            if A.Playing then
               A.Playing := False;
               Play_Audio (Filename => To_String(A.File_Path), Looping => False, Volume => A.Volume);
            end if;

         end;
      end loop;

   end Update;


   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Audio_System) return String is
   begin
      return "Audio";
   end Name;

end ECS.Systems_Audio;
