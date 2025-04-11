with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Widget.Text;
with Widget.Button;
with Ada.Tags; use Ada.Tags;

package body Snake_Core is
   Command : Character := ' ';
   
   -- Game state variables
   Game_Is_Over : Boolean := False;
   Score : Natural := 0;
   
   -- Snake position and direction
   type Direction is (Up, Down, Left, Right);
   Current_Direction : Direction := Right;
   
   -- Board dimensions
   Board_Width : constant := 20;
   Board_Height : constant := 10;
   
   -- Snake representation
   type Position is record
      X, Y : Integer;
   end record;
   
   type Snake_Array is array (1..100) of Position;
   Snake : Snake_Array;
   Snake_Length : Natural := 3;
   
   -- Food position
   Food : Position;
   
   -- Initialize the game with widgets
   procedure Initialize_Game is
   begin
      -- Create game UI elements using the widget system
      -- Main container
      Game_Container := Widget.Create(
         id => "game_container",
         parent => Handler.main_widget,
         min_width => Board_Width + 4,
         min_height => Board_Height + 6
      );
      
      -- Score display
      Score_Display := Widget.Text.Create(
         id => "score_display",
         parent => Game_Container,
         text => "Score: 0",
         priority => 1
      );
      
      -- Game board
      Game_Board := Widget.Create(
         id => "game_board",
         parent => Game_Container,
         min_width => Board_Width,
         min_height => Board_Height,
         priority => 0
      );
      
      -- Initialize snake
      Snake(1) := Position'(X => 3, Y => 1);
      Snake(2) := Position'(X => 2, Y => 1);
      Snake(3) := Position'(X => 1, Y => 1);
      Snake_Length := 3;
      
      -- Initialize food
      Food := Position'(X => 10, Y => 5);
      
      -- Reset game state
      Game_Is_Over := False;
      Score := 0;
      Current_Direction := Right;
      
      -- Request a render update
      Handler.update_render := True;
   end Initialize_Game;
   
   function Get_Command return Character is
   begin
      return Command;
   end Get_Command;
   
   procedure Set_Command (Cmd : Character) is
   begin
      Command := Cmd;
   end Set_Command;
   
   task body Input is
      C : Character;
   begin
      loop
         Get_Immediate(C);
         Set_Command(C);
         
         select
            accept Stop;
            exit;
         else
            null;
         end select;
      end loop;
   end Input;
   
   procedure Render_Game is
      use Widget;  -- Add this use clause to make operators visible
   begin
      -- Update score display
      if Score_Display /= null then
         -- Check if it's a Text widget and update the score
         if Score_Display.all'Tag = Widget.Text.Instance'Tag then
            -- Use the Set_Text procedure we just added
            Widget.Text.Set_Text(Widget.Text.Instance(Score_Display.all), 
                               "Score: " & Trim(Score'Image, Ada.Strings.Left));
         end if;
      end if;
      
      -- The actual rendering is handled by the widget system
      Handler.Update_Display;
   end Render_Game;
   
   procedure Move_Cursor_To_End is
   begin
      -- This is handled by the widget system now
      null;
   end Move_Cursor_To_End;
   
   procedure Update_Game (Cmd : Character) is
      New_Head : Position;
      Ate_Food : Boolean := False;
   begin
      if Game_Is_Over then
         return;
      end if;
      
      -- Update direction based on command
      case Cmd is
         when 'w' | 'W' =>
            if Current_Direction /= Down then
               Current_Direction := Up;
            end if;
         when 's' | 'S' =>
            if Current_Direction /= Up then
               Current_Direction := Down;
            end if;
         when 'a' | 'A' =>
            if Current_Direction /= Right then
               Current_Direction := Left;
            end if;
         when 'd' | 'D' =>
            if Current_Direction /= Left then
               Current_Direction := Right;
            end if;
         when others =>
            null;
      end case;
      
      -- Calculate new head position
      New_Head := Snake(1);
      case Current_Direction is
         when Up =>
            New_Head.Y := New_Head.Y - 1;
         when Down =>
            New_Head.Y := New_Head.Y + 1;
         when Left =>
            New_Head.X := New_Head.X - 1;
         when Right =>
            New_Head.X := New_Head.X + 1;
      end case;
      
      -- Check for collisions with walls
      if (New_Head.X < 1) or else (New_Head.X > Board_Width) or else
         (New_Head.Y < 1) or else (New_Head.Y > Board_Height) then
         Game_Is_Over := True;
         return;
      end if;
      
      -- Check for collisions with self
      for I in 2..Snake_Length loop
         if (New_Head.X = Snake(I).X) and then (New_Head.Y = Snake(I).Y) then
            Game_Is_Over := True;
            return;
         end if;
      end loop;
      
      -- Check if food was eaten
      if (New_Head.X = Food.X) and then (New_Head.Y = Food.Y) then
         Ate_Food := True;
         Snake_Length := Snake_Length + 1;
         Score := Score + 10;
         
         -- Generate new food
         Food.X := (Food.X * 7 + 13) mod Board_Width + 1;
         Food.Y := (Food.Y * 11 + 17) mod Board_Height + 1;
      end if;
      
      -- Move snake
      for I in reverse 2..Snake_Length loop
         Snake(I) := Snake(I-1);
      end loop;
      Snake(1) := New_Head;
      
      -- Request a render update
      Handler.update_render := True;
   end Update_Game;
   
   function Quit_Game (Cmd : Character) return Boolean is
   begin
      return Cmd = 'q' or Cmd = 'Q';
   end Quit_Game;
   
   function Game_Over return Boolean is
   begin
      return Game_Is_Over;
   end Game_Over;
   
begin
   -- Package initialization
   null;
end Snake_Core;