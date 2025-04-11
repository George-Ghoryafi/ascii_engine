with Widget;
with Handler;

package Snake_Core is
   task type Input is
      entry Stop;
   end Input;

   function Get_Command return Character;
   procedure Set_Command (Cmd : Character);

   -- Initialize the game with widgets
   procedure Initialize_Game;
   
   -- Render the game using the widget system
   procedure Render_Game;

   procedure Move_Cursor_To_End;

   -- Update game state based on command
   procedure Update_Game (Cmd : Character);

   function Quit_Game (Cmd : Character) return Boolean;
   function Game_Over return Boolean;
   
private
   -- Game UI elements
   Game_Container : Widget.Any_Acc;
   Score_Display : Widget.Any_Acc;
   Game_Board : Widget.Any_Acc;
end Snake_Core;