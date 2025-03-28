with Ascii_Engine; use Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Snake_Core;

procedure Snake is
   Cmd : Ascii_Engine.Command_T;
   Input_Task : Ascii_Engine.Input_T;
   Next_Release : Time               := Clock;
   Period       : constant Time_Span := Milliseconds (200);
begin
   Snake_Core.Init_Game;
   Input_Task.Start;
   loop
      Cmd := Ascii_Engine.Command;
      
      exit when Cmd = Quit;

      Snake_Core.Update_Game (Cmd);
      exit when Snake_Core.Game_Over;

      Snake_Core.Render_Game;
      
      Next_Release := Next_Release + Period;
      delay until Next_Release; 
   end loop;
   Snake_Core.Move_Cursor_To_End;
   Input_Task.Stop;
end Snake;