with ECS.Systems; use ECS.Systems;
with ECS.Manager; use ECS.Manager;
  
package ECS.System.Animation is
   type Animation_T is new System with null record;
   overriding
   procedure Execute ( Self : in out Animation_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager'Class);
end ECS.System.Animation;