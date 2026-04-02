with Math.Linear_Algebra;     use Math.Linear_Algebra;
with Graphics.Rendering; use Graphics.Rendering;


package ECS.Components.Texture is
   type Texture_Component is new Component with record
      Width : Integer;
      Height : Integer;
      Data : Storage_Array_Access;
   end record;
end;
