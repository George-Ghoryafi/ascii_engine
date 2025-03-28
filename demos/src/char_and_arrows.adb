with Ascii_Engine; use Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;

procedure Char_And_Arrows is
   Cmd : Ascii_Engine.Command_T;
   Input_Task : Ascii_Engine.Input_T;
begin
   Input_Task.Start;
   loop
      delay 0.1;
      Cmd := Ascii_Engine.Command;
      Put_Line (Cmd'Image);
   end loop;
   Input_Task.Stop;
end Char_And_Arrows;