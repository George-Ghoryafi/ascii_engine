package Snake_Core is

   task type Input is
      entry Stop;
   end Input;

   function Get_Command return Character;
   procedure Set_Command (Cmd : Character);

   procedure Render_Game;

end Snake_Core;