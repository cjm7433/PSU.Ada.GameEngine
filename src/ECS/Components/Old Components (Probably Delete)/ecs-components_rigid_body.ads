package ECS.Components_Rigid_Body is

   type Rigid_Body is record
      Restitution : Float := 1.0; -- bounciness in collisions
      Is_Static   : Boolean := False;
   end record;

end ECS.Components_Rigid_Body;
