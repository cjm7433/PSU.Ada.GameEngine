package body Bricks is 
   procedure Add_Brick(
      S : in out Store;
      P : Vector2;
      Brick_Type : Integer;
      C : Color
      ) is
      begin
         --  Initialize (S);
         -- Create brick entity in store
         Brick : Entity_ID := Create_Entity (S);

         -- Add required components to brick
         Add_Component (S, Brick, Transform_Component'Tag);

         -- Position brick according to inputs
         S.Transform.Data (S.Transform.Lookup (Brick)).Position := (P.X, P.Y);

      end Add_Brick;
end Bricks;