-- ecs-systems_audio.ads
--
-- Required Components: Audio
--
-- Responsibilities:
--   - Play audio tracks on audio components

with ECS.Systems;       use ECS.Systems;
with ECS.Store;
with ECS.Components;

package ECS.Systems_Audio is

   type Audio_System is new System with null record;

   --------------------------------------------------------
   -- Update
   -- Update plays audio files
   --------------------------------------------------------
   overriding 
   procedure Update
     (Self : in out Audio_System;
      S    : in out ECS.Store.Store;
      DT   : Float);


   --------------------------------------------------------
   -- Components_Required
   -- Components_Needed is the list of required components 
   -- Components required: Audio
   --------------------------------------------------------
   overriding 
   function Components_Needed (Self : Audio_System) 
      return ECS.Components.Component_Tag_Array;

   ---------------------------------------------------------------------------
   -- Name
   -- Return system name for performance tracking
   ---------------------------------------------------------------------------
   overriding
   function Name (Self : Audio_System) return String;

end ECS.Systems_Audio;
