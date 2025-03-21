with Ascii_Engine; use Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Snake_Core;

procedure Snake is
   Input_Task    : Ascii_Engine.Input;
   Input         : Ascii_Engine.Input_T;
   Next_Release : Time               := Clock;
   Period       : constant Time_Span := Milliseconds (100);
begin
   Snake_Core.Init_Game;
   loop
      Input := Ascii_Engine.Get_Input;
      
      exit when Snake_Core.Quit_Game (Input.Key);
      exit when Snake_Core.Quit_Game (Input.Key);

      Snake_Core.Update_Game (Input);
      exit when Snake_Core.Game_Over;

      Snake_Core.Render_Game;
      Next_Release := Next_Release + Period;

      delay until Next_Release; 
   end loop;
   Snake_Core.Move_Cursor_To_End;
   Input_Task.Stop;
end Snake;