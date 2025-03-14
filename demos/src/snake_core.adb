with Ada.Text_IO; use Ada.Text_IO;

with Ada.Real_Time; use Ada.Real_Time;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Ada.Numerics.Discrete_Random;
with Snake;

package body Snake_Core is

   Up : Character := 'w';
   Down : Character := 's';
   Left : Character := 'a';
   Right : Character := 'd';
   Quit : Character := 'q';

   function Quit_Game (Cmd : Character) return Boolean is
      (Cmd = Quit);

   protected type Data is
      procedure Set_Command (Cmd : Character);
      function Get_Command return Character;
   private
      Command : Character := 'w';
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
      Period : constant Time_Span := Microseconds (5);
   begin
      loop
         select
            accept Stop do
               null;
            end Stop;
            exit;
         else
            Get_Immediate (Cmd, Avail);
            if Cmd = Up or Cmd = Down or Cmd = Left or Cmd = Right or Cmd = Quit then
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

   subtype Width is Integer range 1 .. 36;
   subtype Interior_W is Width range Width'First+1 .. Width'Last-1;
   subtype Height is Integer range 1 .. 18;
   subtype Interior_H is Height range Height'First+1 .. Height'Last-1;

   type Game_Buffer is array (Height'Range, Width'Range) of Character;

   package Rand_Width is new Ada.Numerics.Discrete_Random (Interior_W);
   Gen_Width : Rand_Width.Generator;

   package Rand_Height is new Ada.Numerics.Discrete_Random (Interior_H);
   Gen_Height : Rand_Height.Generator;

   type Position is record
      Row : Height;
      Col : Width;
   end record;
   
   function Generate_Food_Pos return Position is
   begin
      Rand_Width.Reset (Gen_Width);
      Rand_Height.Reset (Gen_Height);
      return (Rand_Height.Random (Gen_Height), Rand_Width.Random (Gen_Width));
   end;

   GB : Game_Buffer := (others => (others => ' '));
   Food_Img : Character := '*';
   Snake_Img : Character := 'o';
   
   type Snake is array (Positive range <>) of Position;
   type Snake_Ptr is access Snake;

   S : Snake_Ptr := new Snake'(1 .. 1 => Position'(Height'Last / 2, Width'Last / 2));

   function Collide (S : Snake_Ptr) return Boolean is
   begin
      for I in S'First+1 .. S'Last loop
         if S (S'First) = S (I) then
            return True;
         end if;
      end loop;
      if S (S'First).Col = Width'First or S (S'First).Col = Width'Last or 
         S (S'First).Row = Height'First or S (S'First).Row = Height'Last then
         return True;
      end if;
      return False;
   end;

   function Game_Over return Boolean is
   begin
      return Collide (S);
   end;

   --  function Snake (S : Snake_Ptr) return Snake_Ptr is
   --     S : Snake_Ptr := new Snake (1 .. Len);
   --  begin
   --     for I in S'Range loop
   --        S (I).Row := 1;
   --        S (I).Col := I;
   --     end loop;
   --     return S;
   --  end;


   Trigger_Food : Boolean := True;

   function Update_Pos (P : Position; Cmd : Character) return Position is
      (case Cmd is when 'w' => (P.Row - 1, P.Col),
                   when 's' => (P.Row + 1, P.Col),
                   when 'a' => (P.Row, P.Col - 1),
                   when 'd' => (P.Row, P.Col + 1),
                   when others => (P.Row, P.Col));

   procedure Update_Game (Cmd : Character) is
      Pos : Position;
   begin
      GB (S.all (1).Row, S.all (1).Col) := ' ';
      S.all (1) := Update_Pos (S.all (1), Cmd);
      GB (S.all (1).Row, S.all (1).Col) := Snake_Img;
      if Trigger_Food then
         Pos := Generate_Food_Pos;
         GB (Pos.Row, Pos.Col) := Food_Img;
         Trigger_Food := False;
      end if;
   end;

   function Trim (S : String) return String is
            (S (S'First + 1 .. S'Last));

   function Move_Cursor (R : Natural; C : Natural) return String is
        (ESC & "[" & Trim (R'Image) & ";" & Trim (C'Image) & "H");

   procedure Move_Cursor_To_End is
   begin
      Put (Move_Cursor (GB'Length (2), 1));
   end Move_Cursor_To_End;

   function Hide_Cursor return String is (ESC & "[?25l");


   function Text_Color  return String is (ESC & "[38;2");
   function Reset       return String is (ESC & "[0m");
   function Terminator  return String is ("m"); 

   procedure Render_Game is
   begin
      Put (Hide_Cursor);
      for R in GB'Range (1) loop
         for C in GB'Range (2) loop
            Put (Move_Cursor (R+1, C));
            Put (GB (R, C));
         end loop;
      end loop;
   end Render_Game;

end Snake_Core;