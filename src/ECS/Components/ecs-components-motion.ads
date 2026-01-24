package ECS.Components.Motion is
   --TODO: Replace all floats with Vectors
   type Motion is record
      Linear_Velocity:        Float;
      Angular_Velocity:       Float;
      Linear_Acceleration:    Float;
      Angular_Acceleration:   Float;
   end record;
end;