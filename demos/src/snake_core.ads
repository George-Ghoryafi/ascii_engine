with Ascii_Engine; use Ascii_Engine;

package Snake_Core is

   procedure Init_Game;
   procedure Update_Game (Cmd : Command_T);
   procedure Render_Game;

   procedure Move_Cursor_To_End;

   function Game_Over return Boolean;

end Snake_Core;