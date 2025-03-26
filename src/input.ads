-- input.ads
-- Specification file for handling input in the ASCII engine.

package Input is
   type Arrow_T is (Up, Down, Left, Right, Undefined);
   
   type Input_T is record
      Key : Character := ASCII.NUL;
      Arrow : Arrow_T := Undefined;
   end record;
   
   procedure Simple;
   
   -- Returns the current input state
   function Get_Input return Input_T;
   
   -- Resets the input state
   procedure Reset_Input;
end Input;