package Ascii_Engine is

   type Command_T is (Up, Down, Left, Right, Quit);

   task type Input_T is
      entry Start;
      entry Stop;
   end Input_T;
   
   function Command return Command_T;

end Ascii_Engine;
