with Ascii_Engine; use Ascii_Engine;

package Snake_Core is

   procedure Render_Game;

   procedure Move_Cursor_To_End;

   procedure Init_Game;
   procedure Update_Game (Input : Input_T);

   function Quit_Game (Key : Character) return Boolean;
   function Game_Over return Boolean;


end Snake_Core;