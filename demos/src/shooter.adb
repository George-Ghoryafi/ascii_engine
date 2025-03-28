with Shooter_Core;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Shooter is
   Next_Release : Time               := Clock;
   Period       : constant Time_Span := Milliseconds (1000);
begin
   Shooter_Core.Init_Game;
   loop
      Shooter_Core.Update_Game;
      Shooter_Core.Render_Game;
      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Shooter;