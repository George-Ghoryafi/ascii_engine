with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;


with Ada.Containers.Multiway_Trees;

package body Shooter_Core is

   function Clear_Screen return String is (ESC & "[2J" & ESC & "[H");
   function Hide_Cursor  return String is (ESC & "[?25l");
   function Text_Color   return String is (ESC & "[38;2");
   function Reset        return String is (ESC & "[0m");
   function Terminator   return String is ("m"); 

   type Byte is mod 2 ** 8;

   subtype Width is Byte range 1 .. 30;
   subtype Interior_W is Width range Width'First+1 .. Width'Last-1;
   subtype Height is Byte range 1 .. 15;
   subtype Interior_H is Height range Height'First+1 .. Height'Last-1;

   type Render_Buffer is array (Width'Range, Height'Range) of Character;

   function Trim (S : String) return String is
            (S (S'First + 1 .. S'Last));
   function Move_Cursor (X : Width; Y : Height) return String is
      (ESC & "[" & Trim (Y'Image) & ";" & Trim (X'Image) & "H"); --- LOOK OUT!

   DW : constant := 1.0 / 8.0;
   type Width_Fixed is delta DW range 2.0 .. 29.0 with size => 8;

   function Random_Width return Interior_W is
      package Rand_Width is new Ada.Numerics.Discrete_Random (Interior_W);
      Gen_Width : Rand_Width.Generator;
   begin
      return Rand_Width.Random (Gen_Width);
   end Random_Width;

   type Position is record
      X : Width := Width'First;
      Y : Height := Height'First;
   end record;
   function Valid_Pos (P: Position) return Boolean is
      (P.X /= Width'First and P.Y /= Height'First);

   Max_Projectiles  : constant := 5;                            -- Nombre maximum de tirs
   Max_Enemies      : constant := 5;                            -- Nombre d'ennemis
   Ship_Pos         : Position   := 
      (Interior_W'Last / 2, Interior_H'Last);                   -- Position du vaisseau
   Running          : Boolean    := True;                       -- Variable pour contrôler la boucle du jeu
   Score            : Natural    := 0;                          -- Score du joueur

   Proj    : Character := '|';                                  -- Caractère pour le projectile
   Ship    : Character := '^';                                  -- Caractère pour le vaisseau
   Enemy   : Character := 'X';                                  -- Caractère pour l'ennemi
   Contour : Character := '.';
   Empty_Pix : Character := ' ';

   RB : Render_Buffer := (others => (others => ' '));

   type Enemies is array (1 .. Max_Enemies) of Position;
   type Projectiles is array (1 .. Max_Projectiles) of Position;

   PS : Projectiles := ((1, 1), (5, Interior_H'Last), (1, 1), (8, Interior_H'Last - 3), (1, 1));

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

   function Generate_Enemy_Pos (RB : Render_Buffer) return Position is
      R_Pos : Position := (Interior_W'First, Interior_H'First);
      Found_Spot : Boolean := False;
   begin
      loop
         R_Pos.X := Random_Width;
         if RB (R_Pos.X, Interior_H'First) = Empty_Pix then
            return R_Pos;
         end if;
      end loop;
   end Generate_Enemy_Pos;

   procedure Update_Projectiles (RB : in out Render_Buffer; P : in out Projectiles) is
      New_Pos : Position;
   begin
      for I in P'Range loop
         if Valid_Pos (P (I)) then
            New_Pos := (P (I).X, P (I).Y - 1);
            if RB (New_Pos.X, New_Pos.Y) = Empty_Pix then
               RB (P (I).X, P (I).Y) := Empty_Pix;
               P (I) := New_Pos;
               RB (P (I).X, P (I).Y) := Proj;     
            elsif RB (New_Pos.X, New_Pos.Y) = Enemy then
               RB (P (I).X, P (I).Y) := Empty_Pix;
               P (I) := (Width'First, Height'First);
            end if;
         end if; 
      end loop;
   end;

   procedure Init_Game is
   begin
      Put (Clear_Screen);
      Generate_Contour (RB);
   end;

   procedure Update_Game is
   begin
      Update_Projectiles (RB, PS);
   end;

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



   --  type Projectile is record
   --     X : Integer := 0;
   --     Y : Integer := 0;
   --     Active : Boolean := False;
   --  end record;

   --  --  type Enemy is record
   --  --     X : Integer;
   --  --     Y : Integer;
   --  --     Active : Boolean;
   --  --  end record;

   --  Projectiles : array (1 .. Max_Projectiles) of Projectile;

   --  Enemies : array (1 .. Max_Enemies) of Enemy :=
   --     (others => (X => 0, Y => 0, Active => False));
   
   --   -- Instancier le package générique pour générer des entiers aléatoires dans l'intervalle 1..100
   --  -- type randRange is new Integer range 1..Screen_Width;
   --  -- package Rand_Int is new ada.numerics.discrete_random(randRange);

   

   --  procedure Draw_Screen is
   --  begin
   --     -- Effacer l'écran
   --     for I in 1 .. 20 loop
   --        Put_Line("");
   --     end loop;

   --     -- Afficher le score
   --     -- Set_Cursor(1, 1);
   --     Put("Score: ");
   --     Put(Integer'Image(Score));
   --     New_Line;

   --     -- Afficher l'écran de jeu
   --     for Row in reverse 1 .. 10 loop
   --        for Col in 1 .. 20 loop
   --           declare
   --              Drawn : Boolean := False;
   --           begin
   --              -- Afficher un projectile si présent
   --              for P of Projectiles loop
   --                 if P.Active and then P.X = Col and then P.Y = Row then
   --                    Put("|");
   --                    Drawn := True;
   --                    exit;
   --                 end if;
   --              end loop;

   --              -- Afficher un ennemi si présent
   --              for E of Enemies loop
   --                 if E.Active and then E.X = Col and then E.Y = Row then
   --                    Put("X");
   --                    Drawn := True;
   --                    exit;
   --                 end if;
   --              end loop;

   --              -- Afficher le vaisseau
   --              if not Drawn then
   --                 if Row = 1 and then Col = Ship_Pos then
   --                    Put("^");
   --                 else
   --                    Put(" ");
   --                 end if;
   --              end if;
   --           end;
   --        end loop;
   --        New_Line;
   --     end loop;

   --     -- Afficher la ligne de sol
   --     for I in 1 .. 20 loop
   --        Put("-");
   --     end loop;
   --     New_Line;
   --  end Draw_Screen;

   --  type Command is (Left, Shoot, Right, Quit, None);
   --  for Command use (Left => 0, Shoot => 1, Right => 2, Quit => 3, None => 4);

   --  function Read_Command return Command is
   --  begin
   --     if GPIO.Read_Pin (Command'Enum_Rep (Left)) then
   --        return Left;
   --     elsif GPIO.Read_Pin (Command'Enum_Rep (Shoot)) then
   --        return Shoot;
   --     elsif GPIO.Read_Pin (Command'Enum_Rep (Right)) then
   --        return Right;
   --     elsif GPIO.Read_Pin (Command'Enum_Rep (Quit)) then
   --        return Quit;
   --     else
   --        return None;
   --     end if;
   --  end;

   --  procedure Read_Input is
   --     Droite           : constant Integer := 2; -- Pin pour la direction droite
   --     Gauche           : constant Integer := 0; -- Pin pour la direction gauche
   --     Espace           : constant Integer := 1; -- Pin pour le tir
   --     Quitter          : constant Integer := 3; -- Pin pour quitter le jeu
   --     State            : Boolean := False;      -- Variable pour stocker l'état du bouton
   --  begin
   --     while not State loop
   --        if GPIO.Read_Pin(Gauche) = True and then Ship_Pos > 1 then
   --           Ship_Pos := Ship_Pos - 1;
   --           State := True;
   --        elsif GPIO.Read_Pin(Droite) = True and then Ship_Pos < 20 then
   --           Ship_Pos := Ship_Pos + 1;
   --           State := True;
   --        elsif GPIO.Read_Pin(ESpace) = True then
   --           -- Ajouter un tir
   --           for P of Projectiles loop
   --              if not P.Active then
   --                 P.X := Ship_Pos;
   --                 P.Y := 2; -- Juste au-dessus du vaisseau
   --                 P.Active := True;
   --                 exit;
   --              end if;
   --           State := True; 
   --           end loop;
   --        elsif GPIO.Read_Pin(Quitter) = True then
   --           Running := False; -- Quitter le jeu
   --           State := True;
   --        end if;
   --     end loop;
   --  exception
   --     when others => null;
   --  end Read_Input;

   --  procedure Update_Projectiles is
   --  begin
   --     for P of Projectiles loop
   --        if P.Active then
   --           P.Y := P.Y + 1; -- Déplacer vers le haut
   --           if P.Y > 10 then
   --              P.Active := False; -- Désactiver si en dehors de l’écran
   --           end if;
   --        end if;
   --     end loop;
   --  end Update_Projectiles;

   --  procedure Update_Enemies is
   --  begin
   --     for E of Enemies loop
   --        if E.Active then
   --           E.Y := E.Y - 1; -- Déplacer vers le bas
   --           if E.Y <= 0 then
   --              E.Active := False; -- Désactiver si en dehors de l’écran
   --           end if;
   --        end if;
   --     end loop;
   --  end Update_Enemies;

   --  procedure Spawn_Enemies is
   --  begin
   --     for E of Enemies loop
   --        if not E.Active then
   --           E.X := 3;
   --           --E.X := randomN;
   --           E.Y := 10; -- Pour que les ennemis puissent apparaitre 
   --           E.Active := True;
   --           exit;
   --        end if;
   --     end loop;
   --  end Spawn_Enemies;

   --  -- Logique de collision : si un projectile touche un ennemi
   --  procedure Check_Collisions is
   --  begin
   --     for P of Projectiles loop
   --        if P.Active then
   --           for E of Enemies loop
   --              if E.Active and then P.X = E.X and then P.Y = E.Y then
   --                 -- Désactiver l'ennemi et le projectile
   --                 E.Active := False;
   --                 P.Active := False;
   --                 -- Augmenter le score
   --                 Score := Score + 1;
   --                 --exit;  -- Sortir dès qu'une collision est détectée
   --              end if;
   --              if E.Active and then E.X = Ship_Pos and then E.Y = 1 then
   --                 -- Si un ennemi touche le vaisseau, le jeu est terminé
   --                 Running := False;
   --                 exit;
   --              end if;
   --           end loop;
   --        end if;
   --     end loop;
   --  end Check_Collisions;

   --  procedure Game is
   --     Cmd : Command := None;
   --  begin
   --     loop
   --        exit when not Running; -- Quitter la boucle si Running = False
   --        Spawn_Enemies;  -- Spawner de nouveaux ennemis
   --        Draw_Screen;
   --        Update_Projectiles;
   --        Update_Enemies;
   --        Check_Collisions;
   --        Timer.Init;
   --        Timer.Wait (10);
   --        Read_Input;
   --     end loop;

   --     Put_Line("Oops, vous avez perdu !!");
   --     Put(" Votre Score: ");
   --     Put(Integer'Image(Score));
   --     New_Line;
   --     Running := True;
   --  end Game;

   procedure Game is
   begin
      null;
   end Game;

end Shooter_Core;
