with Ascii_Engine; use Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;

procedure Char_And_Arrows is
   Input_Task : Ascii_Engine.Input;
   Cmd : Ascii_Engine.Input_T;
begin
   loop
      delay 0.1;
      Cmd := Ascii_Engine.Get_Input;
      if Cmd.Key /= ASCII.NUL then
         Put_Line (Cmd.Key'Image);
      elsif Cmd.Arrow /= Undefined then
         Put_Line (Cmd.Arrow'Image);
      end if;
   end loop;
end Char_And_Arrows;