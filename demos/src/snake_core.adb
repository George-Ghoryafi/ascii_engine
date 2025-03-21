with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Ada.Numerics.Discrete_Random;
with Snake;

package body Snake_Core is

   --  Up        : Input_T := ('w', Up);
   --  Down      : constant Input_T := ('s', Down);
   --  Left      : constant Input_T := ('a', Left);
   --  Right     : constant Input_T := ('d', Right);
   Quit      : Character := 'q';
   Contour   : Character := '.';
   Empty_Pix : Character := ' ';
   Food_Pix  : Character := '*';
   Snake_Pix : Character := 'o';

   function Clear_Screen return String is (ESC & "[2J" & ESC & "[H");
   function Hide_Cursor  return String is (ESC & "[?25l");
   function Text_Color   return String is (ESC & "[38;2");
   function Reset        return String is (ESC & "[0m");
   function Terminator   return String is ("m"); 

   subtype Width is Integer range 1 .. 36;
   subtype Interior_W is Width range Width'First+1 .. Width'Last-1;
   subtype Height is Integer range 1 .. 18;
   subtype Interior_H is Height range Height'First+1 .. Height'Last-1;

   type Render_Buffer is array (Height'Range, Width'Range) of Character;

   
   type Char_Slice is array (Positive range <>) of Character;

   function Trim (S : String) return String is
            (S (S'First + 1 .. S'Last));

   function Move_Cursor (R : Height; C : Width) return String is
        (ESC & "[" & Trim (R'Image) & ";" & Trim (C'Image) & "H");

   function Quit_Game (Key : Character) return Boolean is
      (Key = Quit);


   package Rand_Width is new Ada.Numerics.Discrete_Random (Interior_W);
   Gen_Width : Rand_Width.Generator;

   package Rand_Height is new Ada.Numerics.Discrete_Random (Interior_H);
   Gen_Height : Rand_Height.Generator;

   type Position is record
      Row : Height;
      Col : Width;
   end record;
   
   function Generate_Food_Pos (RB : Render_Buffer) return Position is
      R_Pos : Position;
      Found_Spot : Boolean := False;
   begin
      Rand_Width.Reset (Gen_Width);
      Rand_Height.Reset (Gen_Height);
      loop
         R_Pos.Row := Rand_Height.Random (Gen_Height);
         R_Pos.Col := Rand_Width.Random (Gen_Width);
         if RB (R_Pos.Row, R_Pos.Col) = Empty_Pix then
            return R_Pos;
         end if;
      end loop;
   end;

   procedure Generate_Contour (RB : in out Render_Buffer) is
   begin
      for I in RB'Range (1) loop
         RB (I, RB'First (2)) := Contour;
         RB (I, RB'Last (2)) := Contour;
      end loop;
      for J in RB'Range (2) loop
         RB (RB'First (1), J) := Contour;
         RB (RB'Last (1), J) := Contour;
      end loop;
   end;

   RB : Render_Buffer := (others => (others => ' '));
   FP : Position;
   
   type Snake is array (Positive range <>) of Position;
   type Snake_Ptr is access Snake;

   S : Snake_Ptr := new Snake'(1 .. 1 => Position'(Height'Last / 2, Width'Last / 2));
   S_Old_Pos  : Position := S.all (1);
   S_First_Pos : Position := S.all (1);
   
   Trigger_Food : Boolean := True;

   function Eat_Food (S : in out Snake_Ptr; FP : Position) return Snake_Ptr is
      procedure Free is new Ada.Unchecked_Deallocation (Snake, Snake_Ptr);
   begin
      if S.all (1) = FP then
         declare
            New_S : Snake_Ptr := new Snake (S'First .. S'Length + 1);
         begin
            New_S.all := S.all & S_Old_Pos;
            Trigger_Food := True;
            Free (S);
            return New_S;
         end;
      end if;
      return S;
   end;

   function Game_Over return Boolean is
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

   function Update_Pos (P : Position; Input : Input_T) return Position is
   begin
      if Input.Key = 'w' or else Input.Arrow = Up  then 
         return (P.Row - 1, P.Col);
      elsif Input.Key = 's' or else Input.Arrow = Down then
         return (P.Row + 1, P.Col);
      elsif Input.Key = 'a' or else Input.Arrow = Left then
         return (P.Row, P.Col - 1);
      else
         return (P.Row, P.Col + 1);
      end if;
   end;

   procedure Init_Game is
      Pos : Position;
   begin
      Put (Clear_Screen);
      Generate_Contour (RB);
   end;

   procedure Render_Snake (S : Snake_Ptr; Pix : Character) is
   begin
      for I in S'Range loop
         RB (S (I).Row, S (I).Col) := Pix;
      end loop;
   end;

   function Update_Snake (Fisrt_Pos : Position; S : Snake_Ptr) return Snake_Ptr is
   begin
      S.all := Fisrt_Pos & S (S'First .. S'Last - 1);
      return S;
   end;

   procedure Update_Game (Input : Input_T) is
   begin
      Render_Snake (S, Empty_Pix);
      S_Old_Pos := S.all (1);
      S_First_Pos := Update_Pos (S (S'First), Input);
      S := Eat_Food (S, FP);
      S := Update_Snake (S_First_Pos, S);
      Render_Snake (S, Snake_Pix);
      RB (S.all (1).Row, S.all (1).Col) := Snake_Pix;
      if Trigger_Food then
         FP := Generate_Food_Pos (RB);
         RB (FP.Row, FP.Col) := Food_Pix;
         Trigger_Food := False;
      end if;
   end;

   procedure Move_Cursor_To_End is
   begin
      Put (Move_Cursor (RB'Length (1), 1));
   end Move_Cursor_To_End;

   procedure Render_Game is
   begin
      Put (Hide_Cursor);
      for R in RB'Range (1) loop
         for C in RB'Range (2) loop
            Put (Move_Cursor (R, C));
            Put (RB (R, C));
         end loop;
      end loop;
   end Render_Game;

end Snake_Core;