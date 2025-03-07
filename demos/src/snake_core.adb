with Ada.Text_IO; use Ada.Text_IO;

with Ada.Real_Time; use Ada.Real_Time;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Snake_Core is

   protected type Data is
      procedure Set_Command (Cmd : Character);
      function Get_Command return Character;
   private
      Command : Character := 'a';
   end Data;

   protected body Data is
      procedure Set_Command (Cmd : Character) is
      begin
         Command := Cmd;
      end Set_Command;

      function Get_Command return Character is
         (Command);
   end Data;

   D : Data;

   task body Input is
      Cmd : Character;
      Avail : Boolean;
      Next_Release : Time := Clock;
      Period : constant Time_Span := Milliseconds (100);
   begin
      loop
         select
            accept Stop do
               null;
            end Stop;
            exit;
         else
            Get_Immediate (Cmd, Avail);
            if Cmd /= Ascii.NUL then
               Snake_Core.Set_Command (Cmd);
            end if;
            Next_Release := Next_Release + Period;
            delay until Next_Release; 
         end select;
      end loop;
   end Input;

   function Get_Command return Character is
      (D.Get_Command);

   procedure Set_Command (Cmd : Character) is
   begin
      D.Set_Command (Cmd);
   end Set_Command;

   type Game_Buffer is array (1 .. 80, 1 .. 24) of Character;

   GB : Game_Buffer;

   function Clear_Screen return String is (ESC & "[2J" & ESC & "[H");

   procedure Render_Game is
      Clr_Rst : String := ASCII.ESC & "[2J" & ASCII.ESC & "[H";
   begin
      GB := (others => (others => D.Get_Command));
      for I in GB'Range (2) loop
         for J in GB'Range (1) loop
            Put (GB (J, I));
         end loop;
         New_Line;
      end loop;
      Put_Line (Clr_Rst);
   end Render_Game;

end Snake_Core;