with Graphics.Color; use Graphics.Color;

package ECS.Components.Quad is
   type Quad_Component is new Component with record
      Width : Float;
      Height : Float;
      C : Color;
   end record;
end;
