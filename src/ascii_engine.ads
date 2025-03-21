package Ascii_Engine is

   type Arrow_T is (Up, Down, Left, Right, Undefined);
   
   type Input_T is record
      Key : Character := ASCII.NUL;
      Arrow : Arrow_T := Undefined;
   end record;

   task type Input is
      entry Stop;
   end Input;

   function Get_Input return Input_T;

end Ascii_Engine;
