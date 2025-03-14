package Snake_Core is

   task type Input is
      entry Stop;
   end Input;

   function Get_Command return Character;
   procedure Set_Command (Cmd : Character);

   procedure Render_Game;

   procedure Move_Cursor_To_End;

   procedure Update_Game (Cmd : Character);

   function Quit_Game (Cmd : Character) return Boolean;
   function Game_Over return Boolean;


end Snake_Core;