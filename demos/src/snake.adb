with Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;


with Snake_Core;

procedure Snake is
   Cmd : Character;
   Avail : Boolean;
   Input_Task : Snake_Core.Input;
begin
   loop
      Cmd := Snake_Core.Get_Command;
      Snake_Core.Render_Game;

      exit when Cmd = 'q';
   end loop;
   Input_Task.Stop;
end Snake;
