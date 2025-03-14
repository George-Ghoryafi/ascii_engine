with Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Snake_Core;

procedure Snake is
   Cmd : Character;
   Avail : Boolean;
   Input_Task : Snake_Core.Input;
   Next_Release : Time := Clock;
   Period : constant Time_Span := Milliseconds (500);
begin
   loop
      Cmd := Snake_Core.Get_Command;
      exit when Snake_Core.Quit_Game (Cmd) or Snake_Core.Game_Over;
      Snake_Core.Update_Game (Cmd);
      Snake_Core.Render_Game;
      Next_Release := Next_Release + Period;
      delay until Next_Release; 
   end loop;
   Snake_Core.Move_Cursor_To_End;
   Input_Task.Stop;
end Snake;
